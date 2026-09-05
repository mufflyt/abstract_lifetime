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
# ── Bundle manifest: what lets this file run in CI ───────────────────────────
#
# bundle/ is gitignored, so every assertion that read it directly used to SKIP
# in CI. Forty-five of them. The defect they guard against (a bundle 135 days
# behind the analysis) therefore had no CI protection at all, which is the
# same shape of hole as a test that reads a gitignored cache: green locally,
# inert where it matters.
#
# shiny/adjudication_app/bundle_manifest.csv IS tracked. deploy.R copies the
# verbatim files unchanged and, in the same run, records each source's md5 and
# byte count. So a source that still matches the manifest is byte-identical to
# its copy in the bundle, and an assertion about bundle CONTENT can be answered
# by reading the source instead. bundle_or_source() below does exactly that.
#
# The limit, stated plainly rather than papered over: this establishes that the
# SOURCES have not moved since the bundle was built. It cannot detect a bundle
# directory deleted or corrupted after the deploy, because the manifest records
# the source checksum, not the copy's. deploy.R:104 compares each copy against
# its source at deploy time, and when the bundle is present the byte-identical
# test below still checks the real files.

MANIFEST <- here("shiny", "adjudication_app", "bundle_manifest.csv")

test_that("the bundle manifest lists exactly the sources deploy.R copies", {
  skip_if_not(file.exists(MANIFEST), "no bundle_manifest.csv - run deploy.R")
  m <- read_csv(MANIFEST, show_col_types = FALSE)

  expect_true(all(c("source", "md5", "bytes", "git_tracked",
                    "bundle_built_utc") %in% names(m)))
  expect_gt(nrow(m), 0)
  expect_equal(anyDuplicated(m$source), 0L)

  # Read the verbatim list straight out of deploy.R so the manifest cannot
  # silently stop covering a file the deploy still ships.
  deploy_src <- readLines(here("shiny", "adjudication_app", "deploy.R"),
                          warn = FALSE)
  i <- grep("^verbatim <- list\\(", deploy_src)
  skip_if(length(i) == 0, "verbatim list not found in deploy.R")
  j <- min(grep("^\\)$", deploy_src)[grep("^\\)$", deploy_src) > i])
  quoted <- unlist(regmatches(deploy_src[i:j],
                              gregexpr('"[^"]+"', deploy_src[i:j])))
  copied <- unique(gsub('"', "", quoted))

  missing_from_manifest <- setdiff(copied, m$source)
  expect_equal(missing_from_manifest, character(0),
               info = "deploy.R copies a file the manifest does not record")
})

test_that("no bundle source has changed since the last deploy", {
  skip_if_not(file.exists(MANIFEST), "no bundle_manifest.csv - run deploy.R")
  m <- read_csv(MANIFEST, show_col_types = FALSE) |> filter(git_tracked)
  skip_if(nrow(m) == 0, "no git-tracked sources in the manifest")

  # Both md5 and byte count. The checksum alone would catch any real change,
  # but a size that disagrees with a matching checksum means the manifest was
  # written by something other than the file it claims to describe, which is a
  # different and worse failure than staleness.
  drifted <- character()
  for (i in seq_len(nrow(m))) {
    src <- here(m$source[i])
    if (!file.exists(src)) {
      drifted <- c(drifted, paste0(m$source[i], " (missing)"))
      next
    }
    if (!identical(unname(tools::md5sum(src)), m$md5[i])) {
      drifted <- c(drifted, m$source[i])
    } else if (!identical(as.numeric(file.size(src)), as.numeric(m$bytes[i]))) {
      drifted <- c(drifted, paste0(m$source[i], " (md5 matches but size does not: ",
                                   file.size(src), " on disk vs ", m$bytes[i],
                                   " recorded)"))
    }
  }

  expect_equal(
    drifted, character(0),
    label = paste0(
      "sources changed since the bundle was built on ", m$bundle_built_utc[1],
      " UTC: ", paste(drifted, collapse = ", "),
      ". The deployed app is serving older data than the analysis. ",
      "Run: Rscript shiny/adjudication_app/deploy.R"
    )
  )
})

test_that("the manifest records which sources CI cannot check", {
  skip_if_not(file.exists(MANIFEST), "no bundle_manifest.csv - run deploy.R")
  m <- read_csv(MANIFEST, show_col_types = FALSE)

  # pubmed_candidates.csv is ~130 MB and gitignored, so its checksum is recorded
  # but unverifiable in CI. That is a known limit, and it must stay visible
  # rather than being quietly dropped from the manifest.
  unchecked <- m |> filter(!git_tracked)
  expect_true(all(!is.na(unchecked$md5) | !file.exists(here(unchecked$source))),
              info = "an untracked source was recorded with no checksum")
  if (nrow(unchecked) > 0) {
    expect_true(all(unchecked$source %in% "data/processed/pubmed_candidates.csv"),
                info = paste("a new untracked source entered the bundle:",
                             paste(setdiff(unchecked$source,
                                           "data/processed/pubmed_candidates.csv"),
                                   collapse = ", ")))
  }
})

# Resolve the file a bundle-content assertion should read.
#
# Locally: the bundle exists, so read it, and the assertion tests the real
# artifact. In CI: the bundle is absent, so establish from the manifest that
# the source has not moved since the bundle was built, then read the source.
# Because deploy.R copies these files verbatim, the two are byte-identical
# whenever that holds, and the assertion answers the same question.
#
# A source that no longer matches the manifest FAILS here rather than skipping.
# That drift is precisely the defect this file exists to catch, and a skip
# would report it as coverage.
bundle_or_source <- function(rel) {
  bun <- bundle_path(rel)
  if (file.exists(bun)) return(bun)

  src <- here(rel)
  skip_if_not(file.exists(src), paste("neither bundle nor source present:", rel))
  skip_if_not(file.exists(MANIFEST), "no bundle_manifest.csv - run deploy.R")

  m <- read_csv(MANIFEST, show_col_types = FALSE)
  row <- m[m$source == rel, , drop = FALSE]
  skip_if(nrow(row) != 1, paste("bundle_manifest.csv does not record", rel))
  # An untracked source cannot be verified from a clean checkout, so standing
  # in the source for the bundle would assert nothing. Skip honestly instead.
  skip_if(!isTRUE(row$git_tracked[1]),
          paste(rel, "is not git-tracked; CI cannot verify it against the manifest"))

  expect_identical(
    unname(tools::md5sum(src)), row$md5[1],
    label = paste0(rel, " has changed since the bundle was built on ",
                   row$bundle_built_utc[1], " UTC, so the app would serve older ",
                   "data than the analysis. Run: Rscript shiny/adjudication_app/deploy.R")
  )
  src
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
  # With the bundle present this compares the real copies. Without it, each
  # source is checked against the md5 and byte count deploy.R recorded when it
  # made those copies, which establishes the same identity from committed data.
  m <- if (file.exists(MANIFEST)) read_csv(MANIFEST, show_col_types = FALSE) else NULL
  checked <- 0L
  for (pair in VERBATIM) {
    src <- here(pair[1])
    bun <- bundle_path(pair[2])
    skip_if_not(file.exists(src), paste("source file absent:", src))

    if (file.exists(bun)) {
      expect_equal(
        unname(tools::md5sum(bun)), unname(tools::md5sum(src)),
        label = paste0(basename(pair[2]), " in the bundle differs from ", pair[1],
                       ". Run: Rscript shiny/adjudication_app/deploy.R")
      )
      checked <- checked + 1L
      next
    }

    skip_if(is.null(m), "no bundle and no bundle_manifest.csv - run deploy.R")
    row <- m[m$source == pair[1], , drop = FALSE]
    expect_equal(nrow(row), 1L,
                 label = paste("bundle_manifest.csv does not record", pair[1]))
    if (nrow(row) != 1) next
    expect_identical(
      unname(tools::md5sum(src)), row$md5[1],
      label = paste0(pair[1], " no longer matches the copy deployed on ",
                     row$bundle_built_utc[1], " UTC. ",
                     "Run: Rscript shiny/adjudication_app/deploy.R")
    )
    expect_identical(
      as.numeric(file.size(src)), as.numeric(row$bytes[1]),
      label = paste0(pair[1], " matches the recorded checksum but not the ",
                     "recorded size, so the manifest describes a different file")
    )
    checked <- checked + 1L
  }
  # Never report success on an empty loop.
  expect_equal(checked, length(VERBATIM),
               label = "not every verbatim file could be checked")
})

test_that("the bundle cohort is the analytical cohort", {
  rel <- "data/processed/abstracts_cleaned.csv"
  bun <- bundle_or_source(rel)
  src <- here(rel)

  b <- read_csv(bun, show_col_types = FALSE)
  s <- read_csv(src, show_col_types = FALSE)

  # Only meaningful when the two are genuinely different files. In CI they are
  # the same path, and the identity they would assert has already been
  # established by bundle_or_source() against the manifest. Comparing a file to
  # itself would pass unconditionally and read as coverage it is not.
  if (!identical(normalizePath(bun), normalizePath(src))) {
    expect_equal(nrow(b), nrow(s))
    expect_setequal(b$abstract_id, s$abstract_id)
  }

  # A real invariant either way: the reviewer queue is built from the Oral
  # cohort, and a video reaching it means the exclusion at
  # 02_clean_abstracts.R:34 did not hold.
  expect_true(all(b$session_type == "Oral"),
              info = "a video presentation reached the reviewer queue")
})

test_that("the bundle carries every candidate the app must display", {
  # Asserts COVERAGE against match_scores.csv, not currency, so the manifest
  # cannot stand in: it needs the rows themselves. candidate_pool() reads the
  # bundle's 130 MB pool when present and output/candidate_pool_index.csv
  # otherwise, which carries exactly the two columns this compares on.
  scores_path <- here("data", "processed", "match_scores.csv")
  skip_if_not(file.exists(scores_path))
  cands <- candidate_pool()
  skip_if(is.null(cands), "no candidate pool or index available")
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
  # app.R:1310-1317 joins candidates to score_details by `pmid`. A type
  # mismatch between the two would join nothing and silently render an
  # unsorted, score-free candidate table. The join key is pmid, which the
  # committed index carries, so this runs without the 130 MB pool.
  sd_p <- bundle_or_source("data/processed/match_scores_detailed.rds")
  # "infer", not the default character coercion. app.R:203 reads the pool with
  # readr's inference and joins it to score_details, which carries a numeric
  # pmid. Coercing here would make both sides agree by construction and the
  # test would assert nothing, which is how it briefly passed 0 assertions.
  cands <- candidate_pool(typed = "infer")
  skip_if(is.null(cands), "no candidate pool or index available")
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
  rel <- "output/manual_review_decisions.csv"
  bun <- bundle_or_source(rel)
  src <- here(rel)

  b <- read_csv(bun, show_col_types = FALSE)
  s <- read_csv(src, show_col_types = FALSE)
  if (!identical(normalizePath(bun), normalizePath(src))) {
    expect_equal(nrow(b), nrow(s))
    expect_equal(sort(table(b$manual_decision)), sort(table(s$manual_decision)))
  }
  # In CI this test reduces to the currency check bundle_or_source() performed,
  # which is the contract it was written for: reviewers must not adjudicate
  # against a decision log older than the analysis.
  expect_gt(nrow(b), 0)
})

test_that("the bundle review queue matches what 05_adjudicate.R emitted", {
  # Not tautological when read from the source: the expectation is derived from
  # match_scores.csv, a different file, so this stays a real cross-artifact
  # invariant in CI.
  bun <- bundle_or_source("output/manual_review_queue.csv")
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
