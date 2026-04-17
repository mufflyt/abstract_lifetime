#!/usr/bin/env Rscript
# Backfill pub_type + author characteristics columns onto existing Google Sheet
# decision rows. Adds missing columns to the sheet header first.

suppressPackageStartupMessages({
  library(googlesheets4); library(dplyr); library(readr); library(here)
  library(yaml); library(cli); library(tibble); library(stringr)
})

cfg <- yaml::read_yaml(here("config.yml"))
sid <- cfg$default$google_sheet_id
gs4_auth(path = here("shiny", "adjudication_app", "google_credentials.json"))

matches <- read_csv(here("output", "abstracts_with_matches.csv"),
                    show_col_types = FALSE) |>
  select(abstract_id,
         matched_pub_type = pub_type_canonical,
         matched_pub_types_raw = pub_types,
         any_of(c("n_authors", "n_unique_affiliations",
                  "first_author_state", "first_author_acog_district",
                  "first_author_gender")))

new_cols <- c("matched_pub_type", "matched_pub_types_raw",
              "n_authors", "n_unique_affiliations",
              "first_author_state", "first_author_acog_district",
              "first_author_gender")

d <- read_sheet(sid, sheet = "decisions", col_types = "c")
missing <- setdiff(new_cols, names(d))
if (length(missing) > 0) {
  hdr <- c(names(d), missing)
  hdr_tbl <- as.data.frame(setNames(replicate(length(hdr), character(),
                                              simplify = FALSE), hdr))
  range_write(sid, hdr_tbl, sheet = "decisions", range = "A1",
              col_names = TRUE, reformat = FALSE)
  d <- read_sheet(sid, sheet = "decisions", col_types = "c")
  cli_alert_info("Added columns: {paste(missing, collapse=', ')}")
}

# Only touch rows that are AUTO-match rows or reviewer-confirmed matches —
# those are the rows where the pub_type / author info for best_pmid applies.
# For no_match/skip rows we deliberately leave these blank.
target_idx <- which(d$manual_decision %in% "match")
cli_alert_info("Rows eligible (decision=match): {length(target_idx)}")

if (length(target_idx) > 0) {
  lkp <- matches |> distinct(abstract_id, .keep_all = TRUE)
  for (col in new_cols) {
    d[[col]][target_idx] <- as.character(lkp[[col]][match(d$abstract_id[target_idx],
                                                          lkp$abstract_id)])
  }
  range_write(sid, d, sheet = "decisions", range = "A1",
              col_names = TRUE, reformat = FALSE)
  cli_alert_success("Backfilled {length(target_idx)} match rows across {length(new_cols)} columns")
}
