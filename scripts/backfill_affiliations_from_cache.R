#!/usr/bin/env Rscript
# backfill_affiliations_from_cache.R
#
# Extracts author given-names, surnames, and affiliations from the JSON blob
# embedded in cached ScienceDirect HTML files. Populates affiliation_raw in
# abstracts_cleaned.csv and writes a full per-author affiliation table to
# data/processed/abstract_author_affiliations.csv.
#
# The ScienceDirect HTML contains a JSON data island with structure:
#   "affiliations": { "aff1": { "$$": [{"#name":"textfn","_":"University of..."}] } }
#   "authors": [ { "given-name": "R.S.", "surname": "Guido", cross-refs to aff ids } ]

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(stringr); library(purrr); library(cli)
})

cache_dir      <- here("data", "cache", "sd_html")
abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")
out_path       <- here("data", "processed", "abstract_author_affiliations.csv")

abstracts <- read_csv(abstracts_path, show_col_types = FALSE)

html_files <- list.files(cache_dir, pattern = "\\.html$", full.names = TRUE)
cli_alert_info("Parsing {length(html_files)} cached HTML files")

# ── Extract affiliations JSON block ────────────────────────────────────────────
parse_affiliations <- function(html_txt) {
  # The affiliations block spans multiple nested braces — extract via fixed markers
  start <- regexpr('"affiliations":\\{', html_txt, fixed = FALSE)
  if (start < 0) return(list())
  # Take a generous window (5000 chars) after the opening brace
  chunk <- substr(html_txt, start, start + 6000)

  # Handle both formats:
  #   old: "textfn","_":"University..."
  #   new: "textfn","$":{...},"_":"Mayo Clinic..."
  textfn <- regmatches(chunk, gregexpr('"textfn"[^_]{0,120}"_":"([^"]+)"', chunk, perl = TRUE))[[1]]
  ids    <- regmatches(chunk, gregexpr('"id":"(aff[0-9]+)"', chunk, perl = TRUE))[[1]]

  aff_id  <- str_match(ids,    '"id":"(aff[0-9]+)"')[, 2]
  aff_txt <- str_match(textfn, '"textfn","_":"([^"]+)"')[, 2]

  if (length(aff_id) == 0 || length(aff_txt) == 0) return(list())
  n <- min(length(aff_id), length(aff_txt))
  setNames(as.list(aff_txt[seq_len(n)]), aff_id[seq_len(n)])
}

# ── Extract authors with name + affiliation ref ────────────────────────────────
parse_authors_json <- function(html_txt, aff_map) {
  # Strategy: find the ordered author list block. ScienceDirect places all authors
  # inside an "authors" array that begins after "normalized-first-auth-surname".
  # We locate each author by finding surname occurrences WITHIN that block only,
  # skipping the earlier "last-author" / "first-author" singleton blocks.

  # Find start of the main authors array (after the singleton blocks)
  main_start <- regexpr('"normalized-first-auth-surname"', html_txt, fixed = TRUE)
  if (main_start < 0) main_start <- 1L

  # Extract a generous chunk covering all authors (up to 30 KB)
  chunk <- substr(html_txt, main_start, main_start + 30000L)

  # Find every given-name occurrence in this chunk
  given_pos <- gregexpr('"#name":"given-name","_":"', chunk, fixed = TRUE)[[1]]
  if (length(given_pos) == 0 || given_pos[1] < 0) return(tibble())

  rows <- vector("list", length(given_pos))
  for (i in seq_along(given_pos)) {
    blk_start <- max(1L, given_pos[i] - 50L)
    blk       <- substr(chunk, blk_start, blk_start + 600L)

    given   <- str_match(blk, '"#name":"given-name","_":"([^"]+)"')[, 2]
    surname <- str_match(blk, '"#name":"surname","_":"([^"]+)"')[, 2]
    if (is.na(surname)) next

    aff_ids <- str_match_all(blk, '"refid":"(aff[0-9]+)"')[[1]][, 2]
    aff_txt <- if (length(aff_ids) > 0 && length(aff_map) > 0) {
      valid <- aff_ids[aff_ids %in% names(aff_map)]
      if (length(valid) > 0) paste(unlist(aff_map[valid]), collapse = " | ") else NA_character_
    } else NA_character_

    rows[[i]] <- tibble(position = i, given_name = given %||% NA_character_,
                        surname = surname, affiliation = aff_txt)
  }
  bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
null_or_na <- function(x) is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))

# ── Main loop ─────────────────────────────────────────────────────────────────
all_rows  <- vector("list", length(html_files))
first_aff <- vector("character", length(html_files))  # first-author affiliation per file

cli_progress_bar("Parsing HTML cache", total = length(html_files))
for (i in seq_along(html_files)) {
  f   <- html_files[[i]]
  pii <- str_remove(basename(f), "\\.html$")

  html_txt <- tryCatch(read_file(f), error = function(e) NA_character_)
  if (is.na(html_txt)) { cli_progress_update(); next }

  aff_map  <- parse_affiliations(html_txt)
  auth_tbl <- parse_authors_json(html_txt, aff_map)

  if (nrow(auth_tbl) > 0) {
    auth_tbl$pii <- pii
    all_rows[[i]] <- auth_tbl
    first_aff[i]  <- auth_tbl$affiliation[1] %||% NA_character_
  }
  cli_progress_update()
}
cli_progress_done()

author_aff <- bind_rows(all_rows)
cli_alert_success("Parsed {nrow(author_aff)} author-affiliation rows from {length(html_files)} files")
cli_alert_info("Authors with affiliation: {sum(!is.na(author_aff$affiliation))} / {nrow(author_aff)}")

# ── Map PII → abstract_id ─────────────────────────────────────────────────────
# DOI contains the PII: 10.1016/j.jmig.2012.08.010 → pii S1553465012003433
pii_from_doi <- function(doi) {
  m <- str_match(doi, "pii/([A-Za-z0-9]+)")
  if (!is.na(m[1, 2])) return(m[1, 2])
  NA_character_
}

pii_map <- abstracts |>
  mutate(
    pii_from_url = str_match(coalesce(article_url, ""), "pii/([A-Za-z0-9]+)")[, 2],
    pii_from_doi = str_match(coalesce(doi, ""), "pii/([A-Za-z0-9]+)")[, 2],
    pii = coalesce(pii_from_url, pii_from_doi)
  ) |>
  filter(!is.na(pii)) |>
  distinct(pii, .keep_all = TRUE) |>
  select(abstract_id, pii)

author_aff_mapped <- author_aff |>
  left_join(pii_map, by = "pii") |>
  relocate(abstract_id, pii)

write_csv(author_aff_mapped, out_path)
cli_alert_success("Wrote {out_path} ({nrow(author_aff_mapped)} rows)")

# ── Backfill affiliation_raw in abstracts_cleaned.csv ─────────────────────────
first_author_aff <- author_aff_mapped |>
  filter(position == 1, !is.na(abstract_id), !is.na(affiliation)) |>
  distinct(abstract_id, .keep_all = TRUE) |>
  select(abstract_id, affiliation_raw_new = affiliation)

n_before <- sum(!is.na(abstracts$affiliation_raw) & nchar(abstracts$affiliation_raw) > 2)

abstracts_updated <- abstracts |>
  left_join(first_author_aff, by = "abstract_id") |>
  mutate(
    affiliation_raw = if_else(
      !is.na(affiliation_raw_new) & (is.na(affiliation_raw) | nchar(affiliation_raw) < 3),
      affiliation_raw_new, affiliation_raw
    )
  ) |>
  select(-affiliation_raw_new)

n_after <- sum(!is.na(abstracts_updated$affiliation_raw) & nchar(abstracts_updated$affiliation_raw) > 2)
cli_alert_success("affiliation_raw: {n_before} → {n_after} / {nrow(abstracts)} ({round(n_after/nrow(abstracts)*100,1)}%)")

write_csv(abstracts_updated, abstracts_path)
cli_alert_success("Updated abstracts_cleaned.csv")

# ── Coverage by year ───────────────────────────────────────────────────────────
abstracts_updated |>
  group_by(congress_year) |>
  summarise(
    n = n(),
    has_aff = sum(!is.na(affiliation_raw) & nchar(affiliation_raw) > 2),
    pct = round(has_aff / n * 100, 1),
    .groups = "drop"
  ) |> print()
