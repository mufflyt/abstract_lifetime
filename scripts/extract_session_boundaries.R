# extract_session_boundaries.R — read the oral/video boundary out of supplement PDFs.
#
# WHY: ten of the twelve congresses are truncated. The ScienceDirect listing caps
# at ~100 items, and for 2012-2021 every captured record carries an "Oral"
# heading, so abstraction stopped inside the oral block and an unknown number of
# orals were never ingested. Crossref holds every supplement item with its page,
# but carries no session type. The one missing number per congress -- the page at
# which orals end -- is printed in the supplement's own table of contents.
#
# WHAT TO DO: put one PDF per congress in data/raw/, named so the four-digit year
# appears in the filename (e.g. jmig_2015_supplement.pdf), then run:
#
#   Rscript scripts/extract_session_boundaries.R
#
# VALIDATION: 2022 and 2023 are ground truth. Their boundaries are already known
# from the web scrape, which ran past the end of the oral block into video, so
# this script checks itself against them before anything else is believed. Drop
# those two PDFs in alongside the others. If the parser cannot reproduce a known
# boundary it must not be trusted on the ten unknown ones, and the script says so
# and exits non-zero.
#
#   2022: Oral S1-S36, Video begins S37
#   2023: Oral S1-S26, Video begins S27

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(cli)
})
source(here("R", "utils_toc.R"))

GROUND_TRUTH <- tibble::tribble(
  ~congress_year, ~oral_last_page, ~video_first_page,
  2022L,          36L,             37L,
  2023L,          26L,             27L
)

raw_dir <- here("data", "raw")
pdfs <- list.files(raw_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)

if (!length(pdfs)) {
  cli_alert_warning("No PDFs in {.path data/raw/}. Nothing to do.")
  cli_alert_info("Place one supplement PDF per congress, with the year in the filename.")
  cli_alert_info("Include 2022 and 2023: they are the only years that can validate the parser.")
} else if (!requireNamespace("pdftools", quietly = TRUE)) {
  cli_alert_danger("pdftools is required. install.packages('pdftools')")
} else {

  read_one <- function(path) {
    yr <- as.integer(regmatches(basename(path),
                                regexpr("(19|20)[0-9]{2}", basename(path))))
    if (is.na(yr)) {
      cli_alert_warning("No year in filename, skipping: {.path {basename(path)}}")
      return(NULL)
    }
    txt <- tryCatch(pdftools::pdf_text(path), error = function(e) NULL)
    if (is.null(txt)) {
      cli_alert_danger("Could not read {.path {basename(path)}}")
      return(NULL)
    }
    lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
    b <- find_session_boundaries(lines)
    if (!nrow(b)) {
      cli_alert_danger("No session heading found in {.path {basename(path)}}.")
      cli_alert_info("The parser recognises Oral / Video / Poster headings. If this \\
                      supplement words them differently, extend normalize_session_label().")
      return(NULL)
    }
    b$congress_year <- yr
    b[, c("congress_year", "section", "heading", "first_page", "last_page",
          "n_entries", "sparse")]
  }

  res <- bind_rows(lapply(pdfs, read_one))

  if (!nrow(res)) {
    cli_alert_danger("Nothing parsed. Not writing an output file.")
  } else {
    # --- validate against the two congresses whose boundary is already known ---
    checked <- res |>
      filter(.data$congress_year %in% GROUND_TRUTH$congress_year) |>
      select(congress_year, section, first_page, last_page) |>
      tidyr::pivot_wider(names_from = section,
                         values_from = c(first_page, last_page)) |>
      inner_join(GROUND_TRUTH, by = "congress_year")

    ok <- TRUE
    if (nrow(checked)) {
      for (i in seq_len(nrow(checked))) {
        r <- checked[i, ]
        got_oral  <- r$last_page_Oral %||% NA_integer_
        got_video <- r$first_page_Video %||% NA_integer_
        agree <- isTRUE(got_oral == r$oral_last_page) &&
                 isTRUE(got_video == r$video_first_page)
        if (agree) {
          cli_alert_success("{r$congress_year}: parser reproduces the known boundary \\
                             (oral ends S{got_oral}, video begins S{got_video})")
        } else {
          ok <- FALSE
          cli_alert_danger("{r$congress_year}: parser says oral ends S{got_oral} / video \\
                            begins S{got_video}, but the web scrape shows S{r$oral_last_page} \\
                            / S{r$video_first_page}")
        }
      }
    } else {
      ok <- FALSE
      cli_alert_warning("Neither 2022 nor 2023 was supplied, so the parser is unvalidated.")
      cli_alert_info("Add those two PDFs before trusting the other congresses.")
    }

    out <- here("output", "supplement_session_boundaries.csv")
    if (ok) {
      write_csv(res, out)
      cli_alert_success("Validated. Wrote {.path {out}} for \\
                         {length(unique(res$congress_year))} congress(es).")
      orals <- res |> filter(section == "Oral") |> arrange(congress_year)
      cli_alert_info("Oral block ends at: {paste0(orals$congress_year, '=S',
                      orals$last_page, collapse = ', ')}")
      cli_alert_info("Next: take every Crossref supplement item at or before that \\
                      page as an oral presentation, and ingest the ones not already held.")
    } else {
      cli_alert_danger("Validation failed. NOT writing {.path {basename(out)}}.")
      cli_alert_info("A boundary the parser cannot reproduce where the answer is known \\
                      must not be trusted where it is not.")
      quit(status = 1, save = "no")
    }
  }
}
