#!/usr/bin/env Rscript
# Prefill the Google Sheet with the algorithm's automatic decisions.
#
# For every abstract that does NOT already have a saved decision (any reviewer),
# append a row tagged reviewer="AUTO" with the pipeline's verdict:
#   classification == "accept"        -> manual_decision = "match", PMID filled
#   classification == "reject"        -> manual_decision = "no_match"
#   classification == "no_candidates" -> manual_decision = "no_match"
#   classification == "review"        -> skipped (left for humans)
#
# Idempotent — re-running will only add rows for abstracts still missing one.

suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(cli)
  library(yaml)
  library(here)
})

cfg <- yaml::read_yaml(here("config.yml"))
sheet_id <- cfg$default$google_sheet_id
creds    <- here("shiny", "adjudication_app", "google_credentials.json")
gs4_auth(path = creds)

cli_h1("Prefilling algorithm decisions")
cli_alert_info("Sheet: {sheet_id}")

matches   <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
candidates <- read_csv(here("data", "processed", "pubmed_candidates.csv"), show_col_types = FALSE)

existing <- read_sheet(sheet_id, sheet = "decisions", col_types = "c")
already_logged <- unique(existing$abstract_id[!is.na(existing$abstract_id)])

cli_alert_info("Abstracts in matches file: {nrow(matches)}")
cli_alert_info("Already in sheet: {length(already_logged)}")

# Only auto-fill accepts, rejects, and no-candidates — leave "review" for humans.
auto_fill <- matches |>
  filter(!abstract_id %in% already_logged) |>
  filter(classification %in% c("accept", "reject", "no_candidates"))

cli_alert_info("Rows to add: {nrow(auto_fill)}")
if (nrow(auto_fill) == 0) {
  cli_alert_success("Nothing to add — sheet is up to date.")
  quit(save = "no")
}

pick <- function(df, col) {
  if (!col %in% names(df) || nrow(df) == 0) return(NA_character_)
  v <- df[[col]][1]
  if (is.null(v) || is.na(v)) NA_character_ else as.character(v)
}

now_ts <- as.character(Sys.time())

rows <- lapply(seq_len(nrow(auto_fill)), function(i) {
  m <- auto_fill[i, ]
  abs_row <- abstracts |> filter(abstract_id == m$abstract_id)

  is_match <- m$classification == "accept"

  final_pmid <- if (is_match && !is.na(m$best_pmid) && nchar(as.character(m$best_pmid)) > 0) {
    as.character(m$best_pmid)
  } else NA_character_

  cand_row <- if (!is.na(final_pmid)) {
    candidates |> filter(as.character(pmid) == final_pmid)
  } else tibble()

  matched_pub_title   <- if (is_match) pick(cand_row, "pub_title")   else NA_character_
  matched_pub_journal <- if (is_match) pick(cand_row, "pub_journal") else NA_character_
  matched_pub_year    <- if (is_match) pick(cand_row, "pub_year")    else NA_character_
  if (is_match) {
    if (is.na(matched_pub_title))   matched_pub_title   <- pick(m, "pub_title")
    if (is.na(matched_pub_journal)) matched_pub_journal <- pick(m, "pub_journal")
  }
  matched_score            <- if (is_match) pick(m, "best_score") else NA_character_
  matched_title_similarity <- if (is_match) pick(m, "title_sim")  else NA_character_

  tibble(
    abstract_id              = m$abstract_id,
    reviewer                 = "AUTO",
    manual_decision          = if (is_match) "match" else "no_match",
    manual_pmid              = final_pmid,
    reviewer_notes           = sprintf("Auto-filled from algorithm (classification=%s, score=%.2f)",
                                       m$classification, m$best_score %||% 0),
    review_timestamp         = now_ts,
    abstract_title           = pick(abs_row, "title"),
    abstract_first_author    = pick(abs_row, "first_author_normalized") |> {\(x) if (is.na(x)) pick(abs_row, "author_name_first") else x}(),
    abstract_subtype         = pick(abs_row, "subtype"),
    matched_pub_title        = matched_pub_title,
    matched_pub_journal      = matched_pub_journal,
    matched_pub_year         = matched_pub_year,
    matched_score            = matched_score,
    matched_title_similarity = matched_title_similarity
  )
})

new_rows <- bind_rows(rows)
cli_alert_info("Built {nrow(new_rows)} new rows; summary:")
print(table(new_rows$manual_decision, new_rows$reviewer))

# Align columns with the sheet's current header, then append.
header <- names(read_sheet(sheet_id, sheet = "decisions", n_max = 0))
for (col in setdiff(header, names(new_rows))) new_rows[[col]] <- NA_character_
new_rows <- new_rows[, header, drop = FALSE]

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

sheet_append(sheet_id, new_rows, sheet = "decisions")
cli_alert_success("Appended {nrow(new_rows)} AUTO rows to the sheet.")
