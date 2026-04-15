#!/usr/bin/env Rscript
# Backfill congress_year on existing Google Sheet rows. Also prefill new rows
# for the 2022 cohort.

suppressPackageStartupMessages({
  library(googlesheets4); library(dplyr); library(readr); library(here)
  library(yaml); library(cli); library(tibble); library(stringr)
})

cfg <- yaml::read_yaml(here("config.yml"))
sid <- cfg$default$google_sheet_id
gs4_auth(path = here("shiny", "adjudication_app", "google_credentials.json"))

abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                show_col_types = FALSE) |>
  select(abstract_id, congress_year)

d <- read_sheet(sid, sheet = "decisions", col_types = "c")
if (!"congress_year" %in% names(d)) {
  hdr <- c(names(d), "congress_year")
  hdr_tbl <- as.data.frame(setNames(replicate(length(hdr), character(), simplify = FALSE), hdr))
  range_write(sid, hdr_tbl, sheet = "decisions", range = "A1",
              col_names = TRUE, reformat = FALSE)
  d <- read_sheet(sid, sheet = "decisions", col_types = "c")
}

targets <- which(is.na(d$congress_year) | d$congress_year == "")
cli_alert_info("Rows missing congress_year: {length(targets)}")
if (length(targets) > 0) {
  fill <- abs$congress_year[match(d$abstract_id[targets], abs$abstract_id)]
  d$congress_year[targets] <- as.character(fill)
  range_write(sid, d, sheet = "decisions", range = "A1",
              col_names = TRUE, reformat = FALSE)
  cli_alert_success("Filled congress_year on {sum(!is.na(fill))} rows; {sum(is.na(fill))} unmatched.")
}
