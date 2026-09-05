# The row-level data contract must hold, and the checker must actually work.
#
# Every other check on this dataset is an aggregate -- a row count, a rate, a
# median -- and aggregates hide row-level corruption. A dataset can carry the
# right number of rows, the right publication rate and the right median time to
# publication while individual rows contradict themselves.
#
# "Zero violations" is only meaningful if the checker can find one, so half the
# tests below corrupt a copy of the data and require it to be caught. A
# validator that always returns clean looks identical to a healthy dataset.

library(testthat)
suppressPackageStartupMessages({library(dplyr); library(readr)})

repo_root <- here::here()
source(file.path(repo_root, "R", "utils_data_contract.R"))
contract_path <- file.path(repo_root, "config", "data_contract.yml")

test_that("the contract file is well-formed and points at real data", {
  expect_true(file.exists(contract_path))
  ct <- yaml::yaml.load_file(contract_path)
  expect_gt(length(ct$datasets), 0)
  for (ds in ct$datasets) {
    expect_true(nzchar(ds$path %||% ""))
    expect_true(nzchar(ds$key %||% ""))
    expect_true(file.exists(file.path(repo_root, ds$path)),
                label = sprintf("contract names %s, which does not exist", ds$path))
    for (r in ds$rules %||% list()) {
      expect_true(nzchar(r$id %||% ""), label = "a rule has no id")
      expect_true(nzchar(r$description %||% ""),
                  label = sprintf("rule %s has no description", r$id))
    }
  }
})

test_that("the committed analytical dataset satisfies the contract row by row", {
  v <- validate_data_contract(contract_path, root = repo_root)
  expect_equal(
    nrow(v), 0,
    label = paste0(
      "row-level contract violations. Run `Rscript scripts/check_data_contract.R` ",
      "for the full report:\n  ",
      paste(utils::head(sprintf("%s %s [%s]: %s",
                                v$key_value, v$column, v$check, v$detail), 10),
            collapse = "\n  ")))
})

# ---- the checker must detect corruption -------------------------------------
# Each block writes a deliberately broken copy into a temp dir and requires the
# named check to fire. Without these, a validator that silently returns clean
# would pass the test above forever.

with_corrupted <- function(mutate_fn) {
  ct <- yaml::yaml.load_file(contract_path)
  ds <- ct$datasets[[1]]
  df <- read_csv(file.path(repo_root, ds$path), show_col_types = FALSE)
  df <- mutate_fn(df)

  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, dirname(ds$path)), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, file.path(tmp, ds$path))

  ct$datasets[[1]] <- ds
  cp <- file.path(tmp, "contract.yml")
  yaml::write_yaml(ct, cp)
  validate_data_contract(cp, root = tmp)
}

test_that("a congress_year that contradicts its abstract_id is caught", {
  v <- with_corrupted(function(d) { d$congress_year[3] <- 1999L; d })
  expect_true("congress_year_matches_id" %in% v$check | "min" %in% v$check)
  expect_gt(nrow(v), 0)
})

test_that("a malformed abstract_id is caught", {
  v <- with_corrupted(function(d) { d$abstract_id[5] <- "NOT_AN_ID"; d })
  expect_true("regex" %in% v$check)
})

test_that("a duplicated key is caught", {
  v <- with_corrupted(function(d) { d$abstract_id[2] <- d$abstract_id[1]; d })
  expect_true("unique" %in% v$check)
})

test_that("a published row with no interval is caught", {
  v <- with_corrupted(function(d) {
    i <- which(d$final_published %in% TRUE)[1]
    d$months_to_pub[i] <- NA_real_
    d
  })
  expect_true("published_has_interval" %in% v$check)
})

test_that("a published row with no PMID is caught", {
  v <- with_corrupted(function(d) {
    i <- which(d$final_published %in% TRUE)[1]
    d$final_pmid[i] <- NA
    d
  })
  expect_true("published_has_pmid" %in% v$check)
})

test_that("a non-numeric PMID is caught", {
  v <- with_corrupted(function(d) { d$final_pmid[1] <- "PMID-abc"; d })
  expect_true("regex" %in% v$check)
})

test_that("an author count above the ingest ceiling is caught", {
  v <- with_corrupted(function(d) { d$n_authors[1] <- 99; d })
  expect_true("max" %in% v$check)
})

test_that("a missing authors_truncated value is caught", {
  v <- with_corrupted(function(d) { d$authors_truncated[1] <- NA; d })
  expect_true("required" %in% v$check)
})

test_that("collapsing authors_truncated to a constant trips its rule", {
  # This is the regression that matters. The column was computed and then
  # dropped from the R/05_adjudicate.R select once already; with it gone,
  # nothing downstream could distinguish a censored author list from a short
  # one. A flag that silently becomes constant is the same defect in a
  # different form, so it has to fail rather than pass quietly.
  v <- with_corrupted(function(d) { d$authors_truncated <- FALSE; d })
  expect_true("truncation_flag_stays_informative" %in% v$check)
})

test_that("conflating truncation with the display cap trips its rule", {
  # Truncated rows parse to author_count 4 because the ellipsis consumes a
  # slot; capped rows show 5 and are not flagged. If a future parser change
  # made the two coincide, every author-count result would quietly be measured
  # over a different population.
  v <- with_corrupted(function(d) {
    d$author_count[d$authors_truncated] <- 5
    d
  })
  expect_true("truncation_and_cap_are_distinct_signals" %in% v$check)
})

test_that("clearing the candidate PMIDs trips the semantics rule", {
  # final_pmid is a best-scoring CANDIDATE, present on 869 unpublished rows.
  # Someone "tidying" that apparent inconsistency would change what the column
  # means without touching a single published row, so the contract asserts the
  # candidates are there.
  v <- with_corrupted(function(d) {
    d$final_pmid[d$final_published %in% FALSE] <- NA
    d
  })
  expect_true("pmid_is_a_candidate_not_a_publication" %in% v$check)
})
