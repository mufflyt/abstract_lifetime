#!/usr/bin/env Rscript
# deploy.R — Prepare bundle and deploy adjudication app to shinyapps.io
#
# Usage: Rscript shiny/adjudication_app/deploy.R
#
# Steps:
#   1. Copy latest data files into bundle/
#   2. Slim pubmed_candidates.csv (truncate abstracts, drop unneeded columns)
#   3. Deploy to shinyapps.io via rsconnect

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(stringr); library(cli)
})

app_dir    <- here("shiny", "adjudication_app")
bundle_dir <- file.path(app_dir, "bundle")

# ── Step 1: Ensure bundle directories exist ──────────────────────────────────
dir.create(file.path(bundle_dir, "data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(bundle_dir, "output"), recursive = TRUE, showWarnings = FALSE)

# ── Step 2: Copy latest files into bundle ────────────────────────────────────
cli_h2("Preparing bundle")

copies <- list(
  list(from = here("data", "processed", "abstracts_cleaned.csv"),
       to   = file.path(bundle_dir, "data", "processed", "abstracts_cleaned.csv")),
  list(from = here("data", "processed", "match_scores_detailed.rds"),
       to   = file.path(bundle_dir, "data", "processed", "match_scores_detailed.rds")),
  list(from = here("output", "abstracts_with_matches.csv"),
       to   = file.path(bundle_dir, "output", "abstracts_with_matches.csv")),
  list(from = here("output", "manual_review_decisions.csv"),
       to   = file.path(bundle_dir, "output", "manual_review_decisions.csv")),
  list(from = here("output", "manual_review_queue.csv"),
       to   = file.path(bundle_dir, "output", "manual_review_queue.csv")),
  list(from = here("config.yml"),
       to   = file.path(bundle_dir, "config.yml"))
)

for (item in copies) {
  if (file.exists(item$from)) {
    file.copy(item$from, item$to, overwrite = TRUE)
    cli_alert_success("Copied {basename(item$from)} ({round(file.info(item$from)$size/1e6,1)} MB)")
  } else {
    cli_alert_warning("Missing: {item$from}")
  }
}

# ── Step 3: Slim pubmed_candidates.csv ───────────────────────────────────────
cli_h2("Slimming pubmed_candidates.csv")

candidates_full <- here("data", "processed", "pubmed_candidates.csv")
candidates_out  <- file.path(bundle_dir, "data", "processed", "pubmed_candidates.csv")

if (file.exists(candidates_full)) {
  review_ids <- read_csv(here("output", "abstracts_with_matches.csv"),
                         show_col_types = FALSE) |>
    pull(abstract_id) |>
    unique()

  cands <- read_csv(candidates_full, show_col_types = FALSE) |>
    filter(abstract_id %in% review_ids) |>
    select(any_of(c("abstract_id", "pmid", "pub_title", "pub_first_author",
                     "pub_last_author", "pub_journal", "pub_year", "pub_doi",
                     "pub_abstract"))) |>
    mutate(pub_abstract = str_trunc(pub_abstract, 500, ellipsis = "..."))

  write_csv(cands, candidates_out)

  full_mb <- round(file.info(candidates_full)$size / 1e6, 1)
  slim_mb <- round(file.info(candidates_out)$size / 1e6, 1)
  cli_alert_success("Slimmed: {full_mb} MB -> {slim_mb} MB ({nrow(cands)} rows)")
} else {
  cli_alert_warning("No pubmed_candidates.csv found — skipping")
}

# ── Step 4: Report bundle size ───────────────────────────────────────────────
bundle_files <- list.files(bundle_dir, recursive = TRUE, full.names = TRUE)
total_mb <- round(sum(file.info(bundle_files)$size) / 1e6, 1)
cli_h2("Bundle ready: {total_mb} MB total")

# ── Step 5: Deploy ───────────────────────────────────────────────────────────
cli_h2("Deploying to shinyapps.io")

rsconnect::deployApp(
  appDir = app_dir,
  appName = "aagl-adjudication",
  account = "mufflyt",
  forceUpdate = TRUE,
  launch.browser = FALSE
)

cli_alert_success("Deploy complete!")
