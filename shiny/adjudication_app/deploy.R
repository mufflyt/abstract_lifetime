#!/usr/bin/env Rscript
# deploy.R — Prepare bundle and deploy adjudication app to shinyapps.io
#
# Usage: Rscript shiny/adjudication_app/deploy.R
#
# Steps:
#   1. Copy latest data files into bundle/
#   2. Verify the bundle is byte-identical to the analysis inputs
#   3. Refuse to deploy if it is not
#   2. Slim pubmed_candidates.csv (truncate abstracts, drop unneeded columns)
#   3. Deploy to shinyapps.io via rsconnect

suppressPackageStartupMessages({
  library(purrr)
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

# ── Step 4: Verify the bundle before anything can be published ───────────────
# The bundle was 135 days stale for the whole of 2026, so reviewers adjudicated
# against a pre-denominator-fix cohort and a candidate pool missing 283 of the
# 1,102 winning PMIDs. Steps 2 and 3 above only WARN when a source file is
# absent, which is how that survived. Nothing may deploy unless the bundle is
# demonstrably the data the analysis was run on.
cli_h2("Verifying bundle")

verify_problems <- character()

verbatim <- list(
  c("data/processed/abstracts_cleaned.csv",     "data/processed/abstracts_cleaned.csv"),
  c("data/processed/match_scores_detailed.rds", "data/processed/match_scores_detailed.rds"),
  c("output/abstracts_with_matches.csv",        "output/abstracts_with_matches.csv"),
  c("output/manual_review_decisions.csv",       "output/manual_review_decisions.csv"),
  c("output/manual_review_queue.csv",           "output/manual_review_queue.csv")
)
for (pair in verbatim) {
  src <- here(pair[1])
  bun <- file.path(bundle_dir, pair[2])
  if (!file.exists(src)) {
    verify_problems <- c(verify_problems, paste("source missing:", pair[1]))
  } else if (!file.exists(bun)) {
    verify_problems <- c(verify_problems, paste("bundle missing:", pair[2]))
  } else if (!identical(unname(tools::md5sum(src)), unname(tools::md5sum(bun)))) {
    verify_problems <- c(verify_problems, paste("bundle differs from source:", pair[2]))
  }
}

# The winning candidate is the one a reviewer is asked to rule on. If it is not
# in the slimmed pool the comparison pane renders blank.
scores_path <- here("data", "processed", "match_scores.csv")
if (file.exists(scores_path) && file.exists(candidates_out)) {
  scores <- read_csv(scores_path, show_col_types = FALSE) |>
    mutate(best_pmid = as.character(best_pmid))
  slim <- read_csv(candidates_out, show_col_types = FALSE,
                   col_types = readr::cols(.default = readr::col_character()))
  missing_best <- scores |>
    filter(!is.na(best_pmid)) |>
    anti_join(distinct(select(slim, abstract_id, pmid)),
              by = c("abstract_id", "best_pmid" = "pmid"))
  if (nrow(missing_best) > 0) {
    verify_problems <- c(verify_problems, paste(
      nrow(missing_best), "winning PMIDs absent from the bundle candidate pool",
      "- run scripts/rebuild_candidate_pool.R"))
  }
  short <- scores |>
    filter(n_candidates > 0) |>
    select(abstract_id, n_candidates) |>
    left_join(count(slim, abstract_id, name = "n_bundle"), by = "abstract_id") |>
    mutate(n_bundle = coalesce(n_bundle, 0L)) |>
    filter(n_bundle < n_candidates)
  if (nrow(short) > 0) {
    verify_problems <- c(verify_problems, paste(
      nrow(short), "abstracts have fewer candidates in the bundle than were scored"))
  }
} else {
  verify_problems <- c(verify_problems,
                       "match_scores.csv or the slimmed candidate pool is missing")
}

if (length(verify_problems) > 0) {
  for (p in verify_problems) cli_alert_danger(p)
  stop("Bundle verification failed. Refusing to deploy stale or incomplete data.",
       call. = FALSE)
}
cli_alert_success("Bundle verified against the current analysis")

# ── Step 4b: Record what this bundle was built from ──────────────────────────
# bundle/ is gitignored, so every test that inspects it SKIPS in CI - the whole
# of tests/testthat/test-shiny_bundle_currency.R, 48 assertions, never runs
# there. The guard against the defect that actually happened (a bundle 135 days
# behind the analysis) therefore had no CI protection at all.
#
# This manifest is tracked. It records the checksum of every source at the
# moment the bundle was built, so CI can answer the question that matters -
# "have the sources moved since the last deploy?" - using only files it has.
# pubmed_candidates.csv is listed too, marked untracked, so the record is
# complete even though CI cannot check that row.
manifest_sources <- c(
  vapply(verbatim, `[`, character(1), 1),
  "data/processed/pubmed_candidates.csv"
)
tracked_files <- tryCatch(
  system("git ls-files", intern = TRUE),
  error = function(e) character(0)
)

bundle_manifest <- purrr::map_dfr(manifest_sources, function(rel) {
  src <- here(rel)
  tibble::tibble(
    source = rel,
    md5 = if (file.exists(src)) unname(tools::md5sum(src)) else NA_character_,
    bytes = if (file.exists(src)) file.size(src) else NA_real_,
    git_tracked = rel %in% tracked_files
  )
}) |>
  dplyr::mutate(bundle_built_utc = format(Sys.time(), tz = "UTC",
                                          "%Y-%m-%d %H:%M:%S"))

manifest_path <- file.path(app_dir, "bundle_manifest.csv")
readr::write_csv(bundle_manifest, manifest_path)
cli_alert_success(
  "Wrote {.path {manifest_path}} - {sum(bundle_manifest$git_tracked)} of \
   {nrow(bundle_manifest)} sources are git-tracked and checkable in CI"
)

# ── Step 5: Report bundle size ───────────────────────────────────────────────
bundle_files <- list.files(bundle_dir, recursive = TRUE, full.names = TRUE)
total_mb <- round(sum(file.info(bundle_files)$size) / 1e6, 1)
cli_h2("Bundle ready: {total_mb} MB total")

# ── Step 6: Deploy ───────────────────────────────────────────────────────────
# Deployment publishes to a live application that human reviewers use, so it is
# opt-in. Set SHINY_DEPLOY=true to push. Without it this script only refreshes
# bundle/, which is what tests/testthat/test-shiny_app.R checks for staleness.
#
# Note that a fresh bundle does NOT make the deployed app current. Until this
# runs with SHINY_DEPLOY=true, reviewers on shinyapps.io continue to see
# whatever was last pushed.
if (identical(tolower(Sys.getenv("SHINY_DEPLOY", "false")), "true")) {
  cli_h2("Deploying to shinyapps.io")
  rsconnect::deployApp(
    appDir = app_dir,
    appName = "aagl-adjudication",
    account = "mufflyt",
    forceUpdate = TRUE,
    launch.browser = FALSE
  )
  cli_alert_success("Deploy complete!")
} else {
  cli_alert_info("Bundle refreshed. Set SHINY_DEPLOY=true to publish to \
                  shinyapps.io; until then the live app still serves the \
                  previously deployed data.")
}
