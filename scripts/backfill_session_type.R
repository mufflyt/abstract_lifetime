#!/usr/bin/env Rscript
# One-shot: fill session_type on existing Google Sheet decision rows that
# predate the session-type feature. Idempotent.

suppressPackageStartupMessages({
  library(googlesheets4); library(dplyr); library(readr); library(here)
  library(yaml); library(cli)
})

cfg <- yaml::read_yaml(here("config.yml"))
sid <- cfg$default$google_sheet_id
gs4_auth(path = here("shiny", "adjudication_app", "google_credentials.json"))

abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                show_col_types = FALSE) |>
  select(abstract_id, session_type)

d <- read_sheet(sid, sheet = "decisions", col_types = "c")
if (!"session_type" %in% names(d)) {
  cli_alert_info("Adding session_type column via gs_ensure_headers...")
  # header extender logic (inline)
  hdr <- c(names(d), "session_type")
  hdr_tbl <- as.data.frame(setNames(replicate(length(hdr), character(), simplify = FALSE), hdr))
  range_write(sid, hdr_tbl, sheet = "decisions", range = "A1",
              col_names = TRUE, reformat = FALSE)
  d <- read_sheet(sid, sheet = "decisions", col_types = "c")
}

targets <- which(is.na(d$session_type) | d$session_type == "")
cli_alert_info("Rows missing session_type: {length(targets)}")
if (length(targets) == 0) { cli_alert_success("Nothing to do."); quit(save = "no") }

fill <- abs$session_type[match(d$abstract_id[targets], abs$abstract_id)]
d$session_type[targets] <- fill

range_write(sid, d, sheet = "decisions", range = "A1",
            col_names = TRUE, reformat = FALSE)
cli_alert_success("Filled {sum(!is.na(fill))} rows; {sum(is.na(fill))} still unmatched.")
