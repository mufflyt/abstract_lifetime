#!/usr/bin/env Rscript
# Backfill ALL available columns from the pipeline onto the Google Sheet.
# Safe to re-run: overwrites existing values with current pipeline data.

suppressPackageStartupMessages({
  library(googlesheets4); library(dplyr); library(readr); library(here)
  library(yaml); library(cli); library(tibble); library(stringr)
})

cfg <- yaml::read_yaml(here("config.yml"))
sid <- cfg$default$google_sheet_id
gs4_auth(path = here("shiny", "adjudication_app", "google_credentials.json"))

matches <- read_csv(here("output", "abstracts_with_matches.csv"),
                    show_col_types = FALSE) |>
  mutate(across(everything(), as.character))

# All columns to push — abstract-level (every row) and match-level (rows with best_pmid)
push_cols <- intersect(
  c(
    # Algorithm outcome
    "classification", "n_candidates", "best_pmid", "best_score",
    # Abstract characteristics (from text)
    "sample_size", "is_rct", "is_multicenter", "is_us_based", "is_academic",
    "has_funding", "has_industry", "has_trial_registration", "has_irb_statement",
    "stat_sig_reported", "result_positivity", "has_numeric_results",
    "is_database_study", "abstract_word_count", "research_category",
    "primary_procedure", "study_design",
    # Matched publication
    "pub_doi", "pub_first_author", "months_to_pub",
    # Author characteristics
    "n_authors", "n_unique_affiliations", "n_authors_aagl",
    "first_author_last", "first_author_first",
    "first_author_state", "first_author_acog_district",
    "first_author_country", "first_author_gender", "first_author_gender_p",
    "practice_type", "subspecialty", "career_stage"
  ),
  names(matches)
)

cli_alert_info("Columns to push: {length(push_cols)}")

d <- read_sheet(sid, sheet = "decisions", col_types = "c")

# Add any new columns to the sheet header
missing_cols <- setdiff(push_cols, names(d))
if (length(missing_cols) > 0) {
  hdr <- c(names(d), missing_cols)
  hdr_tbl <- as.data.frame(setNames(replicate(length(hdr), character(),
                                              simplify = FALSE), hdr))
  range_write(sid, hdr_tbl, sheet = "decisions", range = "A1",
              col_names = TRUE, reformat = FALSE)
  d <- read_sheet(sid, sheet = "decisions", col_types = "c")
  cli_alert_info("Added {length(missing_cols)} new columns: {paste(missing_cols, collapse=', ')}")
}

lkp <- matches |> distinct(abstract_id, .keep_all = TRUE)

for (col in push_cols) {
  d[[col]] <- as.character(lkp[[col]][match(d$abstract_id, lkp$abstract_id)])
}

range_write(sid, d, sheet = "decisions", range = "A1",
            col_names = TRUE, reformat = FALSE)
cli_alert_success("Backfilled {nrow(d)} rows across {length(push_cols)} columns")
