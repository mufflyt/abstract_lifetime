#!/usr/bin/env Rscript
# Blank out matched-publication columns on existing no_match / skip rows in the
# Google Sheet decisions tab. Idempotent — safe to run multiple times.

suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
  library(cli)
  library(yaml)
  library(here)
})

cfg <- yaml::read_yaml(here("config.yml"))
sheet_id <- cfg$default$google_sheet_id

creds <- here("shiny", "adjudication_app", "google_credentials.json")
gs4_auth(path = creds)

cli_h1("Retroactive cleanup: no_match / skip rows")
cli_alert_info("Sheet: {sheet_id}")

d <- read_sheet(sheet_id, sheet = "decisions", col_types = "c")
cli_alert_info("Current rows: {nrow(d)}")

blank_cols <- c("manual_pmid", "matched_pub_title", "matched_pub_journal",
                "matched_pub_year", "matched_score", "matched_title_similarity")

targets <- which(d$manual_decision %in% c("no_match", "skip"))
cli_alert_info("Rows to clean: {length(targets)}")

if (length(targets) == 0) {
  cli_alert_success("Nothing to do.")
  quit(save = "no")
}

for (col in intersect(blank_cols, names(d))) {
  d[[col]][targets] <- NA_character_
}

# Overwrite the full data range (row 2 onwards) in-place.
range_write(sheet_id, d, sheet = "decisions", range = "A1",
            col_names = TRUE, reformat = FALSE)

cli_alert_success("Blanked {length(targets)} rows across {length(intersect(blank_cols, names(d)))} columns.")
