#!/usr/bin/env Rscript
# deploy_shiny.R — Build the adjudication app bundle and deploy to shinyapps.io
#
# Prereqs (one-time):
#   rsconnect::setAccountInfo(name=..., token=..., secret=...)
#   shiny/adjudication_app/google_credentials.json present
#
# Usage:
#   Rscript deploy_shiny.R                 # deploy to default app name
#   Rscript deploy_shiny.R my-app-name     # custom app slug

suppressPackageStartupMessages({
  library(rsconnect)
  library(here)
  library(cli)
})

args <- commandArgs(trailingOnly = TRUE)
app_name <- if (length(args) >= 1) args[1] else "aagl-adjudication"

app_dir <- here("shiny", "adjudication_app")
stopifnot(dir.exists(app_dir))

# --- 1. Refresh the bundle directory from current pipeline outputs ---
bundle_dir <- file.path(app_dir, "bundle")
unlink(bundle_dir, recursive = TRUE)
dir.create(file.path(bundle_dir, "output"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(bundle_dir, "data", "processed"), recursive = TRUE, showWarnings = FALSE)

copy_if <- function(src, dst) {
  if (file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    cli_alert_success("copied {basename(src)}")
  } else {
    cli_alert_warning("missing: {src}")
  }
}

copy_if(here("output", "abstracts_with_matches.csv"),       file.path(bundle_dir, "output"))
copy_if(here("output", "manual_review_queue.csv"),          file.path(bundle_dir, "output"))
copy_if(here("output", "manual_review_decisions.csv"),      file.path(bundle_dir, "output"))
# Slim pubmed_candidates to only abstracts in review queue (full file is 150MB+)
cand_src <- here("data", "processed", "pubmed_candidates.csv")
if (file.exists(cand_src)) {
  rq <- readr::read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)
  cands <- readr::read_csv(cand_src, show_col_types = FALSE) |>
    dplyr::filter(abstract_id %in% rq$abstract_id)
  cand_dst <- file.path(bundle_dir, "data", "processed", "pubmed_candidates.csv")
  readr::write_csv(cands, cand_dst)
  cli_alert_success("slimmed pubmed_candidates: {nrow(cands)} rows ({round(file.info(cand_dst)$size/1e6,1)} MB)")
}
copy_if(here("data", "processed", "match_scores_detailed.rds"), file.path(bundle_dir, "data", "processed"))
copy_if(here("data", "processed", "abstracts_cleaned.csv"), file.path(bundle_dir, "data", "processed"))
copy_if(here("config.yml"),                                  bundle_dir)

# --- 2. Sanity checks ---
creds <- file.path(app_dir, "google_credentials.json")
if (!file.exists(creds)) stop("Missing ", creds, " — place service-account JSON there before deploying.")

bundle_size_mb <- sum(file.info(list.files(bundle_dir, recursive = TRUE, full.names = TRUE))$size,
                     file.info(creds)$size) / 1e6
cli_alert_info("bundle size: {round(bundle_size_mb, 1)} MB")
if (bundle_size_mb > 1000) stop("Bundle exceeds shinyapps.io 1 GB limit.")

# --- 3. Deploy ---
cli_h1("Deploying '{app_name}' to shinyapps.io")
rsconnect::deployApp(
  appDir     = app_dir,
  appName    = app_name,
  account    = "mufflyt",
  server     = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE
)

cli_alert_success("Deploy complete. Check https://www.shinyapps.io/admin/#/applications/all")
