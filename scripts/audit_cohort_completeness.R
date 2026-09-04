#!/usr/bin/env Rscript
# audit_cohort_completeness.R — Independently verify how much of each AAGL
# congress supplement the pipeline actually ingested.
#
# Why this exists
# ---------------
# R/01b_parse_web.R fetches ONE issue-listing page per congress from
# ScienceDirect. Offset pagination is attempted only when that page returns
# exactly 100 items, and ScienceDirect returns the same page for every offset,
# so the loop breaks immediately. The practical effect is a ceiling of roughly
# 100 presentations per congress. Recorded as F1 in docs/FAILURE_MODES.md and
# as section A14 of docs/technical_appendix.Rmd.
#
# ScienceDirect now returns HTTP 403 to this machine, so the truncation cannot
# be measured against the source it came from. Crossref holds the complete
# deposit for every issue, is public and unauthenticated, and can. This script
# is the evidence behind the A14 table, which was previously produced ad hoc and
# had no producer in the repository.
#
# It measures the gap. It does not close it: re-ingesting the supplements would
# expand the cohort by several hundred abstracts per congress and require the
# whole search and human adjudication to be redone.
#
# Usage:  Rscript scripts/audit_cohort_completeness.R
# Output: output/cohort_completeness_audit.csv
# Cache:  data/cache/crossref_supplements/<year>.json  (one file per congress)

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(purrr)
  library(stringr); library(tibble); library(cli); library(jsonlite)
})

cli_h1("Cohort completeness against the Crossref deposit")

JMIG_ISSN <- "1553-4650"
CONTACT <- Sys.getenv("PIPELINE_EMAIL", unset = "tyler.muffly@dhha.org")
cache_dir <- here("data", "cache", "crossref_supplements")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

parsed_path <- here("data", "processed", "abstracts_parsed_web.csv")
if (!file.exists(parsed_path)) {
  stop("No abstracts_parsed_web.csv - nothing to audit", call. = FALSE)
}

parsed <- read_csv(parsed_path, show_col_types = FALSE) |>
  mutate(doi_bare = tolower(str_remove(str_squish(doi), "^https?://doi\\.org/")),
         page_num = as.integer(str_extract(page_range, "(?<=S)[0-9]+")))

#' Fetch one congress year's JMIG November deposit from Crossref, cached
#'
#' @param year Integer congress year.
#' @return List of Crossref work items.
#' @keywords internal
fetch_year <- function(year) {
  cache_file <- file.path(cache_dir, paste0(year, ".json"))
  if (file.exists(cache_file) && file.size(cache_file) > 1000) {
    return(fromJSON(cache_file, simplifyVector = FALSE)$message$items)
  }
  url <- sprintf(
    paste0("https://api.crossref.org/journals/%s/works",
           "?filter=from-pub-date:%d-11-01,until-pub-date:%d-11-30",
           "&rows=1000&select=DOI,page,volume,issue,title&mailto=%s"),
    JMIG_ISSN, year, year, utils::URLencode(CONTACT, reserved = TRUE))

  cli_alert_info("Crossref: {year}")
  ok <- tryCatch({
    utils::download.file(url, cache_file, quiet = TRUE); TRUE
  }, error = function(e) {
    cli_alert_warning("{year}: {conditionMessage(e)}"); FALSE
  })
  if (!ok || !file.exists(cache_file)) return(NULL)
  Sys.sleep(1)
  fromJSON(cache_file, simplifyVector = FALSE)$message$items
}

years <- sort(unique(parsed$congress_year))
rows <- map(years, function(y) {
  items <- fetch_year(y)
  if (is.null(items) || length(items) == 0) return(NULL)

  supp <- keep(items, function(it) {
    pg <- it$page %||% ""
    is.character(pg) && nchar(pg) > 0 && str_starts(toupper(pg), "S")
  })
  if (length(supp) == 0) return(NULL)

  supp_dois <- tolower(map_chr(supp, ~ .x$DOI %||% NA_character_))
  supp_pages <- as.integer(map_chr(supp, ~ str_extract(toupper(.x$page %||% ""),
                                                       "(?<=S)[0-9]+")))
  ours <- parsed |> filter(congress_year == y)

  tibble(
    congress_year        = y,
    captured             = nrow(ours),
    captured_oral        = sum(ours$session_type == "Oral", na.rm = TRUE),
    captured_video       = sum(ours$session_type == "Video", na.rm = TRUE),
    captured_page_first  = min(ours$page_num, na.rm = TRUE),
    captured_page_last   = max(ours$page_num, na.rm = TRUE),
    supplement_items     = length(supp),
    supplement_page_first = min(supp_pages, na.rm = TRUE),
    supplement_page_last  = max(supp_pages, na.rm = TRUE),
    # Everything we captured should be in the deposit. Anything else means the
    # comparison itself is wrong and the shortfall below cannot be trusted.
    captured_not_in_crossref = sum(!ours$doi_bare %in% supp_dois),
    # Within the pages we did reach, did we get everything on them?
    crossref_within_our_pages = sum(supp_pages <= max(ours$page_num, na.rm = TRUE),
                                    na.rm = TRUE),
    captured_share       = round(nrow(ours) / length(supp) * 100, 1)
  )
})

audit <- bind_rows(rows)
if (nrow(audit) == 0) stop("Crossref returned nothing for any congress", call. = FALSE)

write_csv(audit, here("output", "cohort_completeness_audit.csv"))
print(as.data.frame(audit))

cli_h2("Verdict")
cli_alert_info("Captured {sum(audit$captured)} of {sum(audit$supplement_items)} \\
                supplement items ({round(100 * sum(audit$captured) / sum(audit$supplement_items), 1)}%)")

bad_join <- audit |> filter(captured_not_in_crossref > 0)
if (nrow(bad_join) > 0) {
  cli_alert_danger("{nrow(bad_join)} congress(es) have captured DOIs absent from \\
                    Crossref - the comparison is unreliable for those years")
} else {
  cli_alert_success("Every captured DOI is in the Crossref deposit")
}

ceilinged <- audit |> filter(captured <= 100)
if (nrow(ceilinged) == nrow(audit)) {
  cli_alert_danger("Every congress captured <= 100 items. That is a page-size \\
                    ceiling, not a distribution. See docs/FAILURE_MODES.md F1.")
}

no_video <- audit |> filter(captured_video == 0)
if (nrow(no_video) > 0) {
  cli_alert_warning(
    "{nrow(no_video)} congress(es) captured no Video presentations at all \\
     ({paste(no_video$congress_year, collapse = ', ')}). The window ended inside \\
     the Oral block, so an unknown number of ORAL presentations were never \\
     ingested."
  )
}
