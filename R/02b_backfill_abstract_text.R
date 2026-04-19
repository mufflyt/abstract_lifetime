# 02b_backfill_abstract_text.R — Backfill abstract_text for 2012-2018 congresses
#
# ScienceDirect paywalls the individual article pages for older supplement issues,
# so the web scraper could not retrieve abstract text for 2012-2018.
# This script fetches it from PubMed XML using the DOIs already in the data,
# then patches abstracts_cleaned.csv in place.
#
# Safe to re-run: skips any abstract_id that already has abstract_text.

suppressPackageStartupMessages({
  library(here); library(config); library(cli); library(dplyr); library(readr)
  library(stringr); library(xml2); library(rentrez); library(purrr)
})

cfg <- config::get(file = here("config.yml"))

cli_h2("Backfilling abstract text for 2012-2018 from PubMed")

abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")
abstracts <- read_csv(abstracts_path, show_col_types = FALSE)

missing <- abstracts |>
  filter(is.na(abstract_text) | nchar(abstract_text) < 10) |>
  filter(!is.na(doi) & nchar(doi) > 5)

cli_alert_info("{nrow(missing)} abstracts need backfill")

if (nrow(missing) == 0) {
  cli_alert_success("Nothing to backfill")
  invisible(NULL)
}

cache_dir <- here("data", "cache", "pubmed_xml")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
delay   <- if (has_key) 1 / cfg$pubmed$rate_limit_with_key else 1 / cfg$pubmed$rate_limit_per_sec

#' Fetch PubMed XML for a DOI via esearch + efetch, using file cache
#'
#' @param doi_raw Character. DOI (with or without URL prefix).
#' @return Character scalar. PubMed XML string, or \code{NA_character_}.
#' @keywords internal
fetch_pubmed_by_doi <- function(doi_raw) {
  doi_bare <- str_replace(doi_raw, "^https?://doi\\.org/", "")
  cache_key <- str_replace_all(doi_bare, "[/:]", "_")
  cache_file <- file.path(cache_dir, paste0(cache_key, ".xml"))

  if (file.exists(cache_file) && file.info(cache_file)$size > 100) {
    return(readr::read_file(cache_file))
  }

  Sys.sleep(delay)
  ids <- tryCatch(
    rentrez::entrez_search(db = "pubmed", term = paste0(doi_bare, "[DOI]"),
                           retmax = 1)$ids,
    error = function(e) character(0)
  )
  if (length(ids) == 0) return(NA_character_)

  Sys.sleep(delay)
  xml <- tryCatch(
    rentrez::entrez_fetch(db = "pubmed", id = ids[1], rettype = "xml"),
    error = function(e) NA_character_
  )
  if (!is.na(xml) && nchar(xml) > 100) write_file(xml, cache_file)
  xml
}

#' Parse abstract text from PubMed XML string
#'
#' @param xml_str Character. Raw PubMed XML.
#' @return Character scalar. Concatenated abstract text, or \code{NA_character_}.
#' @keywords internal
parse_abstract_from_xml <- function(xml_str) {
  if (is.na(xml_str) || nchar(xml_str) < 100) return(NA_character_)
  doc <- tryCatch(read_xml(xml_str), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  nodes <- xml_find_all(doc, ".//AbstractText")
  if (length(nodes) == 0) return(NA_character_)
  parts <- xml_text(nodes)
  parts <- parts[!is.na(parts) & nchar(parts) > 0]
  if (length(parts) == 0) return(NA_character_)
  paste(parts, collapse = " ")
}

results <- vector("list", nrow(missing))
n_found <- 0L

for (i in seq_len(nrow(missing))) {
  row <- missing[i, ]
  xml  <- fetch_pubmed_by_doi(row$doi)
  txt  <- parse_abstract_from_xml(xml)
  results[[i]] <- tibble(abstract_id = row$abstract_id, abstract_text_new = txt)
  if (!is.na(txt)) n_found <- n_found + 1L
  if (i %% 50 == 0) cli_alert_info("  [{i}/{nrow(missing)}] found so far: {n_found}")
}

backfill <- bind_rows(results) |>
  filter(!is.na(abstract_text_new) & nchar(abstract_text_new) > 10)

cli_alert_success("Retrieved abstract text for {nrow(backfill)} / {nrow(missing)} abstracts")

if (nrow(backfill) == 0) {
  cli_alert_warning("No abstract text retrieved — JMIG supplement DOIs are not indexed in PubMed. Abstract text backfill from PubMed is not applicable for conference supplement abstracts. Use Step 2c (ScienceDirect snippets) instead.")
  invisible(NULL)
} else {

# Patch abstracts_cleaned.csv
abstracts_patched <- abstracts |>
  left_join(backfill, by = "abstract_id") |>
  mutate(
    abstract_text = if_else(
      !is.na(abstract_text_new) & (is.na(abstract_text) | nchar(abstract_text) < 10),
      abstract_text_new,
      abstract_text
    )
  ) |>
  select(-abstract_text_new)

write_csv(abstracts_patched, abstracts_path)
cli_alert_success("Patched abstracts_cleaned.csv ({nrow(backfill)} rows updated)")

coverage <- abstracts_patched |>
  group_by(congress_year) |>
  summarise(n = n(), has_text = sum(!is.na(abstract_text) & nchar(abstract_text) > 10),
            pct = round(has_text / n * 100, 1), .groups = "drop")
print(coverage)
}
