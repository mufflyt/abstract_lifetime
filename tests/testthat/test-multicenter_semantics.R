# is_multicenter is a claim the abstract makes, not a design fact.
#
# PI decision, 2026-09-05: retained as the design claim rather than redefined as
# `n_affiliations > 1`, which would measure collaboration breadth under the same
# name. See appendix A22.
#
# The pull toward the larger number is real: affiliation data now covers 98.7%
# of the cohort and would give 679 instead of 65. These tests make the decision
# visible to whoever notices that gap next, and fail if the variable is quietly
# switched to the other construct.

library(testthat)
suppressPackageStartupMessages({library(readr); library(dplyr)})

repo_root <- here::here()
CLEAN <- file.path(repo_root, "data", "processed", "abstracts_cleaned.csv")
FINAL <- file.path(repo_root, "output", "final_analytical_dataset.csv")

test_that("is_multicenter is derived from abstract text, not affiliation count", {
  p <- file.path(repo_root, "R", "02d_rederive_predictors.R")
  skip_if_not(file.exists(p))
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")

  expect_match(txt, "is_multicenter = str_detect", fixed = TRUE,
               label = paste("is_multicenter is no longer a text match. If it was",
                             "redefined as n_affiliations > 1 it measures",
                             "collaboration breadth, not study design; that is an",
                             "estimand change and needs a decision (appendix A22)"))
  expect_false(
    grepl("is_multicenter\\s*=\\s*[^\\n]*n_affiliations", txt),
    label = "is_multicenter is defined from n_affiliations; see appendix A22")
})

test_that("the two measures remain distinguishable in the data", {
  skip_if_not(file.exists(CLEAN))
  c <- read_csv(CLEAN, show_col_types = FALSE)
  skip_if_not(all(c("is_multicenter", "n_affiliations") %in% names(c)))

  n_text <- sum(c$is_multicenter, na.rm = TRUE)
  n_aff  <- sum(c$n_affiliations > 1, na.rm = TRUE)

  # If these ever coincide, the variable has been switched to the other
  # construct without the decision being revisited.
  expect_lt(
    n_text, n_aff,
    label = sprintf(paste("is_multicenter (%d) is no longer smaller than the count",
                          "of abstracts naming several affiliations (%d). The design",
                          "claim should be the narrower of the two"), n_text, n_aff))
  expect_lt(n_text, 200L,
            label = sprintf(paste("is_multicenter is TRUE for %d abstracts, far above the",
                                  "65 the text supports; check it has not been redefined"),
                            n_text))
})

test_that("n_affiliations is not carried into the analytical dataset", {
  # Under the retained definition nothing needs it, and propagating it would be
  # a schema change serving a definition that was not adopted. If a later
  # decision does adopt it, this test is the reminder to document the column.
  skip_if_not(file.exists(FINAL))
  f <- read_csv(FINAL, show_col_types = FALSE, n_max = 1)
  expect_false(
    "n_affiliations" %in% names(f),
    label = paste("n_affiliations reached the analytical dataset. That is fine, but",
                  "it needs a data-dictionary entry and appendix A22 needs updating"))
})
