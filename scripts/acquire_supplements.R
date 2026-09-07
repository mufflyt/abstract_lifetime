# acquire_supplements.R — manifest and integrity gate for the raw supplements.
#
# Mandate steps 2 and 3. The boundary recovery converts twelve PDFs into twelve
# numbers no reader can verify by eye, so the identity of the inputs is part of
# the evidence rather than housekeeping. This builds the manifest and refuses to
# bless a set that cannot support the claim.
#
# It does NOT parse and does NOT touch the frozen code path. Step 4 requires
# scripts/extract_session_boundaries.R to run unchanged on the complete set, so
# nothing here modifies cohort logic, parser rules, page-boundary semantics or
# adjudication criteria.
#
# HOW TO USE
#   1. Save one supplement PDF per congress in data/raw/, 2012 through 2023,
#      with the year in the filename: aagl_supplement_2012.pdf ...
#      Include 2022 and 2023. They are the validation controls; without them no
#      historical boundary can be checked against a known answer.
#   2. Rscript scripts/acquire_supplements.R
#      This writes/refreshes data/raw/acquisition_manifest.csv with a row per
#      file: year, filename, byte_count, sha256, retrieved_at, plus the
#      provenance columns you fill in by hand (source_url, source_title,
#      acquisition_method, notes).
#   3. Fill the blank provenance columns. A boundary a reviewer cannot trace to
#      a source is not evidence.
#   4. Re-run. When it reports PASS, proceed to
#      Rscript scripts/extract_session_boundaries.R

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(cli)
})
source(here("R", "utils_acquisition.R"))

EXPECTED_YEARS <- 2012:2023
raw_dir  <- here("data", "raw")
man_path <- file.path(raw_dir, "acquisition_manifest.csv")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

pdfs <- list.files(raw_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)

cli_h1("Supplement acquisition")
cli_alert_info("Found {length(pdfs)} PDF(s) in {.path data/raw/}; {length(EXPECTED_YEARS)} required.")

problems <- check_acquisition(pdfs, EXPECTED_YEARS)

if (length(pdfs)) {
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  fresh <- tibble(
    congress_year     = year_from_filename(pdfs),
    filename          = basename(pdfs),
    byte_count        = file.info(pdfs)$size,
    sha256            = vapply(pdfs, file_sha256, character(1), USE.NAMES = FALSE),
    retrieved_at      = now,
    source_url        = NA_character_,
    source_title      = NA_character_,
    acquisition_method = NA_character_,
    notes             = NA_character_
  ) |> arrange(.data$congress_year)

  # Never overwrite provenance a human typed. Carry it forward by sha256, so a
  # re-download that changes the bytes correctly loses its old provenance.
  if (file.exists(man_path)) {
    old <- suppressWarnings(read_csv(man_path, show_col_types = FALSE, progress = FALSE))
    keep <- intersect(c("source_url", "source_title", "acquisition_method",
                        "notes", "retrieved_at"), names(old))
    if (length(keep) && "sha256" %in% names(old)) {
      fresh <- fresh |>
        select(-any_of(keep)) |>
        left_join(old |> select(all_of(c("sha256", keep))), by = "sha256")
      cli_alert_info("Carried existing provenance forward for \\
                      {sum(!is.na(fresh$source_url))} of {nrow(fresh)} file(s).")
    }
  }

  write_csv(fresh, man_path)
  cli_alert_success("Wrote {.path data/raw/acquisition_manifest.csv} ({nrow(fresh)} row(s)).")

  unfilled <- sum(is.na(fresh$source_url) | !nzchar(as.character(fresh$source_url)))
  if (unfilled > 0)
    problems <- rbind(problems, data.frame(
      severity = "blocker", year = NA_integer_, file = NA_character_,
      problem = sprintf("%d manifest row(s) have no source_url; provenance must be recorded by hand",
                        unfilled), stringsAsFactors = FALSE))
}

blockers <- problems[problems$severity == "blocker", , drop = FALSE]
warnings_ <- problems[problems$severity == "warning", , drop = FALSE]

if (nrow(warnings_)) {
  cli_h2("Warnings")
  for (i in seq_len(nrow(warnings_))) cli_alert_warning(warnings_$problem[i])
}

if (nrow(blockers)) {
  cli_h2("Blockers")
  for (i in seq_len(nrow(blockers))) {
    lbl <- if (!is.na(blockers$file[i])) paste0(blockers$file[i], ": ") else ""
    cli_alert_danger("{lbl}{blockers$problem[i]}")
  }
  cli_alert_danger("Acquisition gate FAILED. Do not run the boundary extractor yet.")
  cli_alert_info("Every boundary this pipeline produces is a number no reader can \\
                  check by eye, so an input that is not provably the right file \\
                  makes the output unverifiable rather than merely uncertain.")
  quit(status = 1, save = "no")
} else {
  cli_alert_success("Acquisition gate PASSED: {length(pdfs)} files, all identified, \\
                     hashed and provenance-recorded.")
  cli_alert_info("Next: Rscript scripts/extract_session_boundaries.R")
}
