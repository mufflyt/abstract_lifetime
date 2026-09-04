# 09k_gender_from_nppes.R — First-author gender from the NPPES registry.
#
# Why this exists
# ---------------
# Tier 1 of the gender waterfall reads `npi_gender` out of the ABOG
# board-certification export. That export's `LATEST` symlink was repointed
# upstream at some time after data/processed/npi_matches.csv was built, and the
# file it now targets carries no gender column at all (see
# docs/FAILURE_MODES.md F16). The shipped 256 tier-1 genders can therefore be
# used but not regenerated, which is a reproducibility blocker for a quarter of
# the gender variable.
#
# NPPES holds a registrant-reported sex for essentially every US clinician with
# an NPI. Reading it keyed on the NPI we already resolved makes tier 1
# reproducible from a public registry rather than from a private file whose
# schema moves.
#
# It is also a methodological improvement. Every other tier in the waterfall
# infers gender from a given name; this one does not guess. See
# docs/AUTHOR_ENRICHMENT.md section 4.
#
# Source: mysterycall::mysterycall_nppes_gender(), which reads the NPPES
# `basic_sex` field via the `npi` package. Pinned in docs/REPRODUCIBILITY.md.
#
# Population: high-confidence NPI matches with a resolved NPI. That is the same
# population R/10e_merge_demographics.R already trusts for npi_state, so this
# introduces no new identity claim - only a new attribute for identities that
# were already accepted.
#
# Output: data/processed/gender_from_nppes.csv
# Cache:  data/cache/nppes_gender/<npi>.rds (one file per NPI, resumable)

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(cli); library(tibble)
})

cli_h2("Gender from the NPPES registry")

npi_path <- here("data", "processed", "npi_matches.csv")
out_path <- here("data", "processed", "gender_from_nppes.csv")
cache_dir <- here("data", "cache", "nppes_gender")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

empty_out <- function() {
  tibble(abstract_id = character(), npi_number = character(),
         nppes_gender = character(), nppes_sex_raw = character())
}

if (!file.exists(npi_path)) {
  cli_alert_warning("No npi_matches.csv - writing an empty sidecar")
  write_csv(empty_out(), out_path)
} else if (!requireNamespace("mysterycall", quietly = TRUE)) {
  # Degrade rather than fail: every other gender tier is unaffected, and an
  # existing sidecar from a machine that HAS the package must not be clobbered.
  cli_alert_danger("Package 'mysterycall' is not installed.")
  cli_alert_info("Install with: remotes::install_github('mufflyt/mysterycall')")
  if (!file.exists(out_path)) write_csv(empty_out(), out_path)
} else {

  targets <- read_csv(npi_path, show_col_types = FALSE) |>
    filter(npi_match_confidence == "high", !is.na(npi_number)) |>
    mutate(npi_number = as.character(npi_number)) |>
    distinct(abstract_id, .keep_all = TRUE)

  cli_alert_info("High-confidence NPIs to look up: {nrow(targets)}")

  #' Registry sex for one NPI, cached on disk
  #'
  #' @param npi Character scalar. A 10-digit NPI.
  #' @return Character scalar: "Male", "Female" or NA_character_.
  #' @keywords internal
  nppes_sex_cached <- function(npi) {
    cache_file <- file.path(cache_dir, paste0(npi, ".rds"))
    if (file.exists(cache_file)) return(readRDS(cache_file))
    res <- tryCatch(
      mysterycall::mysterycall_nppes_gender(npi)$gender[1],
      error = function(e) {
        cli_alert_warning("NPPES lookup failed for {npi}: {conditionMessage(e)}")
        NA_character_
      }
    )
    # Only a definite answer is cached. A failed lookup must be retried on the
    # next run rather than frozen as a negative - the failure mode that
    # docs/FAILURE_MODES.md F5 describes for the search layer.
    if (!is.na(res)) saveRDS(res, cache_file)
    Sys.sleep(0.2)
    res
  }

  n_cached <- sum(file.exists(file.path(cache_dir,
                                        paste0(targets$npi_number, ".rds"))))
  cli_alert_info("{n_cached} already cached; {nrow(targets) - n_cached} to fetch")

  raw <- vapply(targets$npi_number, nppes_sex_cached, character(1),
                USE.NAMES = FALSE)

  out <- targets |>
    transmute(
      abstract_id,
      npi_number,
      nppes_sex_raw = raw,
      nppes_gender = case_when(
        nppes_sex_raw == "Female" ~ "female",
        nppes_sex_raw == "Male"   ~ "male",
        TRUE ~ NA_character_
      )
    ) |>
    select(abstract_id, npi_number, nppes_gender, nppes_sex_raw)

  n_res <- sum(!is.na(out$nppes_gender))
  cli_alert_success(
    "NPPES gender: {n_res}/{nrow(out)} ({round(100 * n_res / nrow(out), 1)}%)"
  )
  print(table(out$nppes_gender, useNA = "ifany"))

  # --- Agreement with the ABOG-derived value it is replacing -----------------
  abog <- read_csv(npi_path, show_col_types = FALSE) |>
    filter(npi_match_confidence == "high", !is.na(npi_gender)) |>
    transmute(abstract_id,
              abog_gender = case_when(npi_gender == "F" ~ "female",
                                      npi_gender == "M" ~ "male",
                                      TRUE ~ NA_character_))

  cmp <- out |>
    inner_join(abog, by = "abstract_id") |>
    filter(!is.na(nppes_gender), !is.na(abog_gender))

  if (nrow(cmp) > 0) {
    agree <- sum(cmp$nppes_gender == cmp$abog_gender)
    cli_h3("Agreement with the ABOG export")
    cli_alert_info(
      "{agree}/{nrow(cmp)} ({round(100 * agree / nrow(cmp), 1)}%) agree"
    )
    disagree <- cmp |> filter(nppes_gender != abog_gender)
    if (nrow(disagree) > 0) {
      cli_alert_warning("{nrow(disagree)} disagreement{?s}:")
      print(as.data.frame(disagree))
      write_csv(disagree, here("data", "processed", "gender_nppes_abog_conflicts.csv"))
      cli_alert_info("Logged to data/processed/gender_nppes_abog_conflicts.csv")
    }
    gained <- out |>
      anti_join(abog, by = "abstract_id") |>
      filter(!is.na(nppes_gender))
    cli_alert_success(
      "{nrow(gained)} abstract{?s} gain a registry gender the ABOG export lacked"
    )
  }

  write_csv(out, out_path)
  cli_alert_success("Saved: {out_path}")
}
