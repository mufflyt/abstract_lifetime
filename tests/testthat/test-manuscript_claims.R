# Every number the manuscript asserts must still be recomputable from output/.
#
# docs/RESULTS_PROVENANCE.md already maps reported numbers to the files that
# produce them, but it is prose: nothing recomputes it, so it goes stale exactly
# when it matters -- when the pipeline is re-run and a value moves. That has
# happened repeatedly here (the publication dates, the covariate re-derivation,
# the gender waterfall), and each time the prose lagged the outputs.
#
# docs/manuscript_claims.csv is the machine-readable version: one row per
# quantitative claim, carrying the value, the file it comes from, and an
# expression that recovers it. These tests evaluate that expression against the
# committed outputs and compare. A claim that drifts fails by name.
#
# Adding a claim here is cheap; the cost of not having it is a manuscript that
# quotes a number no file produces any more.

library(testthat)
suppressPackageStartupMessages(library(readr))

repo_root   <- here::here()
claims_path <- file.path(repo_root, "docs", "manuscript_claims.csv")

test_that("the claims registry is well-formed", {
  expect_true(file.exists(claims_path))
  cl <- read_csv(claims_path, show_col_types = FALSE)

  expect_true(all(c("claim_id", "claim", "value", "unit", "source_file",
                    "source_expr", "tolerance", "appears_in") %in% names(cl)))
  expect_gt(nrow(cl), 0)
  expect_equal(anyDuplicated(cl$claim_id), 0)

  for (i in seq_len(nrow(cl))) {
    expect_true(nzchar(trimws(cl$claim[i])),
                label = sprintf("claim %s has no description", cl$claim_id[i]))
    expect_true(nzchar(trimws(cl$appears_in[i])),
                label = sprintf(paste("claim %s does not say where it appears, so nobody",
                                      "can tell what to fix when it drifts"), cl$claim_id[i]))
    expect_false(is.na(cl$value[i]),
                 label = sprintf("claim %s has no value", cl$claim_id[i]))
  }
})

test_that("every claim still matches the output it came from", {
  cl <- read_csv(claims_path, show_col_types = FALSE)

  drifted <- character(0)
  for (i in seq_len(nrow(cl))) {
    src <- file.path(repo_root, cl$source_file[i])
    if (!file.exists(src)) {
      drifted <- c(drifted, sprintf("%s: source %s is missing",
                                    cl$claim_id[i], cl$source_file[i]))
      next
    }
    d <- suppressWarnings(read_csv(src, show_col_types = FALSE))
    got <- tryCatch(eval(parse(text = cl$source_expr[i]), envir = list(d = d)),
                    error = function(e) NULL)

    if (is.null(got) || length(got) != 1 || is.na(got)) {
      drifted <- c(drifted, sprintf(
        "%s: `%s` against %s did not return one value",
        cl$claim_id[i], cl$source_expr[i], cl$source_file[i]))
      next
    }
    tol <- if (is.na(cl$tolerance[i])) 0 else cl$tolerance[i]
    if (abs(as.numeric(got) - cl$value[i]) > tol) {
      drifted <- c(drifted, sprintf(
        "%s: registry says %s, %s now yields %s (tolerance %s). Appears in: %s",
        cl$claim_id[i], cl$value[i], cl$source_file[i], got, tol, cl$appears_in[i]))
    }
  }

  expect_equal(
    length(drifted), 0,
    label = paste0(
      "manuscript claims no longer match the outputs they came from. Either the ",
      "pipeline moved and the prose needs updating, or a result regressed:\n  ",
      paste(drifted, collapse = "\n  ")))
})

test_that("the cohort accounting in the registry adds up", {
  cl <- read_csv(claims_path, show_col_types = FALSE)
  v <- setNames(cl$value, cl$claim_id)
  need <- c("cohort_total", "cohort_evaluated", "cohort_unresolved",
            "cohort_not_published", "n_published")
  skip_if_not(all(need %in% names(v)), "accounting claims not all registered")

  # Held out abstracts must leave the denominator, not vanish from the cohort.
  expect_equal(unname(v["cohort_evaluated"] + v["cohort_unresolved"]),
               unname(v["cohort_total"]),
               label = "evaluated + unresolved does not equal the cohort")

  # And the denominator must split cleanly into the two outcomes.
  expect_equal(unname(v["n_published"] + v["cohort_not_published"]),
               unname(v["cohort_evaluated"]),
               label = "published + not published does not equal the evaluated denominator")
})

test_that("the headline rate is the ratio the registry claims it is", {
  cl <- read_csv(claims_path, show_col_types = FALSE)
  v <- setNames(cl$value, cl$claim_id)
  skip_if_not(all(c("n_published", "cohort_evaluated", "publication_rate") %in% names(v)))

  expect_equal(unname(round(100 * v["n_published"] / v["cohort_evaluated"], 1)),
               unname(v["publication_rate"]), tolerance = 0.05,
               label = "the reported rate is not published / evaluated")
})
