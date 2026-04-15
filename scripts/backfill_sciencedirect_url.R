#!/usr/bin/env Rscript
# One-shot: populate sciencedirect_url on existing Google Sheet rows from
# abstracts_cleaned.csv. Idempotent.

suppressPackageStartupMessages({
  library(googlesheets4); library(dplyr); library(readr); library(here)
  library(yaml); library(cli)
})

cfg <- yaml::read_yaml(here("config.yml"))
sid <- cfg$default$google_sheet_id
gs4_auth(path = here("shiny", "adjudication_app", "google_credentials.json"))

abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                show_col_types = FALSE) |>
  select(abstract_id, sciencedirect_url = article_url)

d <- read_sheet(sid, sheet = "decisions", col_types = "c")
if (!"sciencedirect_url" %in% names(d)) {
  hdr <- c(names(d), "sciencedirect_url")
  hdr_tbl <- as.data.frame(setNames(replicate(length(hdr), character(), simplify = FALSE), hdr))
  range_write(sid, hdr_tbl, sheet = "decisions", range = "A1",
              col_names = TRUE, reformat = FALSE)
  d <- read_sheet(sid, sheet = "decisions", col_types = "c")
}

targets <- which(is.na(d$sciencedirect_url) | d$sciencedirect_url == "")
cli_alert_info("Rows missing sciencedirect_url: {length(targets)}")
if (length(targets) == 0) { cli_alert_success("Nothing to do."); quit(save = "no") }

fill <- abs$sciencedirect_url[match(d$abstract_id[targets], abs$abstract_id)]
d$sciencedirect_url[targets] <- fill

range_write(sid, d, sheet = "decisions", range = "A1",
            col_names = TRUE, reformat = FALSE)
cli_alert_success("Filled {sum(!is.na(fill))} rows; {sum(is.na(fill))} unmatched.")
