# test-shiny_bundle_currency.R — does the adjudication app serve the data the
# analysis was run on?
#
# The pre-existing check in test-shiny_app.R compares modification times, which
# a `touch` satisfies and which says nothing about content. It was also the
# suite's only failing test for 135 days, because the bundle genuinely was
# stale: reviewers were adjudicating against a pre-denominator-fix cohort and a
# candidate pool missing 283 of the 1,102 winning PMIDs.
#
# These tests compare CONTENT, and check that the bundle is functionally
# complete for what the app has to render.
#
# Scope note: this verifies the bundle that the NEXT deploy will ship. It cannot
# verify what shinyapps.io is currently serving; only a deploy makes the live
# app current. See docs/FAILURE_MODES.md F11.

suppressPackageStartupMessages({
  library(testthat); library(readr); library(dplyr); library(here)
})

BUNDLE <- here("shiny", "adjudication_app", "bundle")

bundle_path <- function(...) file.path(BUNDLE, ...)
need_bundle <- function(...) {
  p <- bundle_path(...)
  skip_if_not(file.exists(p), paste("bundle file absent (gitignored):", p))
  p
}

# Files the deploy copies verbatim. Anything other than an exact match means
# the app and the analysis disagree about the data.
VERBATIM <- list(
  c("data/processed/abstracts_cleaned.csv",     "data/processed/abstracts_cleaned.csv"),
  c("data/processed/match_scores_detailed.rds", "data/processed/match_scores_detailed.rds"),
  c("output/abstracts_with_matches.csv",        "output/abstracts_with_matches.csv"),
  c("output/manual_review_decisions.csv",       "output/manual_review_decisions.csv"),
  c("output/manual_review_queue.csv",           "output/manual_review_queue.csv")
)

test_that("every verbatim bundle file is byte-identical to its source", {
  for (pair in VERBATIM) {
    src <- here(pair[1])
    bun <- bundle_path(pair[2])
    skip_if_not(file.exists(bun), paste("bundle file absent:", bun))
    skip_if_not(file.exists(src), paste("source file absent:", src))

    expect_equal(
      unname(tools::md5sum(bun)), unname(tools::md5sum(src)),
      label = paste0(basename(pair[2]), " in the bundle differs from ", pair[1],
                     ". Run: Rscript shiny/adjudication_app/deploy.R")
    )
  }
})

test_that("the bundle cohort is the analytical cohort", {
  bun <- need_bundle("data/processed/abstracts_cleaned.csv")
  src <- here("data", "processed", "abstracts_cleaned.csv")
  skip_if_not(file.exists(src))

  b <- read_csv(bun, show_col_types = FALSE)
  s <- read_csv(src, show_col_types = FALSE)

  expect_equal(nrow(b), nrow(s))
  expect_setequal(b$abstract_id, s$abstract_id)
  expect_true(all(b$session_type == "Oral"),
              info = "a video presentation reached the reviewer queue")
})

test_that("the bundle carries every candidate the app must display", {
  bun <- need_bundle("data/processed/pubmed_candidates.csv")
  scores_path <- here("data", "processed", "match_scores.csv")
  skip_if_not(file.exists(scores_path))

  cands <- read_csv(bun, show_col_types = FALSE,
                    col_types = cols(.default = col_character()))
  scores <- read_csv(scores_path, show_col_types = FALSE) |>
    mutate(best_pmid = as.character(best_pmid))

  # The winning candidate is the one the reviewer is asked to rule on. If it is
  # absent the app shows a blank comparison pane.
  missing_best <- scores |>
    filter(!is.na(best_pmid)) |>
    anti_join(distinct(select(cands, abstract_id, pmid)),
              by = c("abstract_id", "best_pmid" = "pmid"))
  expect_equal(nrow(missing_best), 0L,
               label = "winning PMIDs absent from the bundle candidate pool")

  # And the full candidate set per abstract, so reviewers can pick a different
  # one. The bundle is slimmed by column, never by row.
  per_abstract <- cands |> count(abstract_id, name = "n_bundle")
  expected <- scores |>
    filter(n_candidates > 0) |>
    select(abstract_id, n_candidates) |>
    left_join(per_abstract, by = "abstract_id") |>
    mutate(n_bundle = coalesce(n_bundle, 0L))

  short <- expected |> filter(n_bundle < n_candidates)
  expect_equal(nrow(short), 0L,
               label = paste("abstracts whose bundle candidate list is shorter",
                             "than what was scored"))
})

test_that("the candidate/score join the app performs actually resolves", {
  cand_p <- need_bundle("data/processed/pubmed_candidates.csv")
  sd_p   <- need_bundle("data/processed/match_scores_detailed.rds")

  # app.R:1310-1317 joins candidates to score_details by `pmid`. A type
  # mismatch between the two would join nothing and silently render an
  # unsorted, score-free candidate table.
  cands <- read_csv(cand_p, show_col_types = FALSE)
  detail <- readRDS(sd_p)

  has_scores <- which(!vapply(detail$score_details, is.null, logical(1)))
  skip_if(length(has_scores) == 0, "no scored abstracts in the bundle")

  for (i in head(has_scores, 25)) {
    id <- detail$abstract_id[i]
    sd <- detail$score_details[[i]]
    joined <- cands |>
      filter(abstract_id == id) |>
      left_join(select(sd, pmid, total_score), by = "pmid")
    if (nrow(joined) == 0) next
    expect_true(all(!is.na(joined$total_score)),
                info = paste("candidate/score join failed for", id,
                             "- check that pmid has the same type on both sides"))
  }
})

test_that("the bundle decision log matches the analysis decision log", {
  bun <- need_bundle("output/manual_review_decisions.csv")
  src <- here("output", "manual_review_decisions.csv")
  skip_if_not(file.exists(src))

  b <- read_csv(bun, show_col_types = FALSE)
  s <- read_csv(src, show_col_types = FALSE)
  expect_equal(nrow(b), nrow(s))
  expect_equal(sort(table(b$manual_decision)), sort(table(s$manual_decision)))
})

test_that("the bundle review queue matches what 05_adjudicate.R emitted", {
  bun <- need_bundle("output/manual_review_queue.csv")
  scores_path <- here("data", "processed", "match_scores.csv")
  skip_if_not(file.exists(scores_path))

  q <- read_csv(bun, show_col_types = FALSE)
  scores <- read_csv(scores_path, show_col_types = FALSE)
  expected <- scores |>
    filter(classification %in% c("probable", "possible") | has_tie)

  expect_equal(nrow(q), nrow(expected))
  expect_setequal(q$abstract_id, expected$abstract_id)
})

test_that("the deploy script does not publish unless asked", {
  # Deployment writes to a live application human reviewers use. It must be
  # opt-in, so that refreshing the bundle in CI or in a test run cannot push.
  src <- readLines(here("shiny", "adjudication_app", "deploy.R"), warn = FALSE)
  txt <- paste(src, collapse = "\n")

  expect_true(grepl("SHINY_DEPLOY", txt, fixed = TRUE),
              info = "deploy.R must gate deployApp() behind SHINY_DEPLOY")
  deploy_line <- grep("rsconnect::deployApp", src)
  gate_line   <- grep("SHINY_DEPLOY", src)
  expect_true(length(deploy_line) > 0 && length(gate_line) > 0 &&
                min(gate_line) < min(deploy_line),
              info = "the SHINY_DEPLOY gate must precede deployApp()")
})

# ── End-to-end: what the running server actually loads ────────────────────────

test_that("the running server loads the current cohort and full candidate sets", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  cleaned_p <- here("data", "processed", "abstracts_cleaned.csv")
  scores_p  <- here("data", "processed", "match_scores.csv")
  skip_if_not(file.exists(cleaned_p) && file.exists(scores_p))
  skip_if_not(file.exists(bundle_path("data/processed/pubmed_candidates.csv")),
              "bundle absent (gitignored)")

  cleaned <- read_csv(cleaned_p, show_col_types = FALSE)
  scores  <- read_csv(scores_p, show_col_types = FALSE)

  app_env <- new.env(parent = globalenv())
  source(here("shiny", "adjudication_app", "app.R"), local = app_env, echo = FALSE)

  shiny::testServer(app_env$server, {
    session$flushReact()
    d <- data()
    expect_false(is.null(d))

    # The cohort the reviewer sees is the cohort that was analysed.
    expect_equal(nrow(d$review_queue), nrow(cleaned))
    expect_setequal(d$review_queue$abstract_id, cleaned$abstract_id)
    expect_setequal(unique(d$review_queue$congress_year),
                    unique(cleaned$congress_year))

    # Every abstract's candidate list is complete. Before the candidate pool was
    # rebuilt on 2026-09-03 the app showed 26 candidates for AAGL2012_001 where
    # 35 had been scored, and no candidate at all for 283 winning PMIDs.
    loaded <- d$candidates |>
      dplyr::count(abstract_id, name = "n_loaded")
    cmp <- scores |>
      dplyr::filter(n_candidates > 0) |>
      dplyr::select(abstract_id, n_candidates) |>
      dplyr::left_join(loaded, by = "abstract_id") |>
      dplyr::mutate(n_loaded = dplyr::coalesce(n_loaded, 0L))
    expect_equal(sum(cmp$n_loaded < cmp$n_candidates), 0L,
                 label = "abstracts the app under-serves candidates for")

    # And the reviewer can see the winning candidate for every abstract.
    best <- scores |>
      dplyr::filter(!is.na(best_pmid)) |>
      dplyr::mutate(best_pmid = as.character(best_pmid))
    have <- d$candidates |>
      dplyr::transmute(abstract_id, pmid = as.character(pmid)) |>
      dplyr::distinct()
    expect_equal(
      nrow(dplyr::anti_join(best, have,
                            by = c("abstract_id", "best_pmid" = "pmid"))),
      0L,
      label = "winning candidates the app cannot display"
    )
  }, args = list())
})

test_that("the server serves the current decision log", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  dec_p <- here("output", "manual_review_decisions.csv")
  skip_if_not(file.exists(dec_p))
  skip_if_not(file.exists(bundle_path("output/manual_review_decisions.csv")),
              "bundle absent (gitignored)")

  source_env <- new.env()
  source(here("R", "utils_decisions.R"), local = source_env)
  expected <- source_env$dedup_decisions_for_analysis(
    read_csv(dec_p, show_col_types = FALSE)
  )

  app_env <- new.env(parent = globalenv())
  source(here("shiny", "adjudication_app", "app.R"), local = app_env, echo = FALSE)

  shiny::testServer(app_env$server, {
    session$flushReact()
    d <- data()
    # The app deduplicates per (abstract_id, reviewer) for display, while the
    # analysis reduces to one row per abstract, so the counts differ by design.
    # What must hold is that the app knows about every abstract the analysis
    # has a decision for.
    expect_true(all(expected$abstract_id %in% d$decisions$abstract_id),
                info = "the app is missing decisions the analysis uses")
  }, args = list())
})
