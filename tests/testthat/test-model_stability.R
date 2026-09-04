# test-model_stability.R — the variable screen and the stability diagnostics.
#
# The screen replaced an inline "< 50% missing and >= 2 levels" rule written out
# twice, which docs/STATISTICAL_ANALYSIS.md flagged as a data-dependent
# specification: the model changed with the data and nothing recorded what was
# dropped. These tests assert the screen is complete, auditable, and actually
# governs the fitted models.

suppressPackageStartupMessages({
  library(testthat); library(readr); library(dplyr); library(here)
})

skip_if_no_file <- function(p) skip_if_not(file.exists(p), paste("missing:", p))

SCREEN <- here("output", "model_variable_screen.csv")
STAB   <- here("output", "model_predictor_stability.csv")
LOO    <- here("output", "model_leave_one_congress_out.csv")
LOGIT  <- here("data", "processed", "logistic_model.rds")
COX    <- here("data", "processed", "cox_model.rds")

test_that("the screen records a decision and a reason for every candidate", {
  skip_if_no_file(SCREEN)
  s <- read_csv(SCREEN, show_col_types = FALSE)

  expect_true(all(c("model", "variable", "kept", "pct_missing",
                    "n_levels", "reason") %in% names(s)))
  expect_setequal(unique(s$model), c("cox", "logistic"))
  expect_true(all(!is.na(s$reason) & nchar(s$reason) > 0),
              info = "a screening decision was recorded without a reason")
  expect_true(all(s$kept == (s$reason == "kept")),
              info = "kept and reason disagree")
  expect_equal(anyDuplicated(paste(s$model, s$variable)), 0L)
})

test_that("the screen's kept set is exactly what the models were fitted on", {
  skip_if_no_file(SCREEN); skip_if_no_file(LOGIT); skip_if_no_file(COX)
  s <- read_csv(SCREEN, show_col_types = FALSE)

  # The four core logistic terms are pre-specified and bypass the screen; the
  # extras are whatever the screen kept. Their union must be the fitted model.
  core <- c("is_rct", "log_sample_size", "is_academic", "is_us_based")
  kept_logit <- s$variable[s$model == "logistic" & s$kept]
  fitted_logit <- setdiff(all.vars(formula(readRDS(LOGIT))), "published_int")
  expect_setequal(fitted_logit, union(core, kept_logit))

  kept_cox <- s$variable[s$model == "cox" & s$kept]
  fitted_cox <- setdiff(all.vars(formula(readRDS(COX))), c("Surv", "time", "event"))
  expect_setequal(fitted_cox, kept_cox)
})

test_that("a near-zero-variance term is excluded, with that named as the reason", {
  skip_if_no_file(SCREEN)
  s <- read_csv(SCREEN, show_col_types = FALSE)

  # has_funding is TRUE for 7 of 1,051 evaluated abstracts - a frequency ratio
  # of about 149:1 against the conventional 19:1 cutoff. Reporting an odds
  # ratio from seven events is what this rule exists to prevent.
  hf <- s |> filter(variable == "has_funding")
  skip_if(nrow(hf) == 0, "has_funding is not a candidate in this run")
  expect_true(all(!hf$kept))
  expect_true(all(grepl("near-zero variance", hf$reason)))
})

test_that("a candidate that never exists is recorded, not silently dropped", {
  skip_if_no_file(SCREEN)
  s <- read_csv(SCREEN, show_col_types = FALSE)
  # log_sample_size is listed as a Cox candidate but is only created inside the
  # Aim 3 block, so it has never entered the Cox model. Before the screen, the
  # candidate list was pre-intersected with names(km_data) and this vanished.
  lss <- s |> filter(model == "cox", variable == "log_sample_size")
  skip_if(nrow(lss) == 0, "log_sample_size is not a Cox candidate in this run")
  expect_false(lss$kept)
  expect_match(lss$reason, "absent")
})

test_that("predictor stability is reported for every fitted term", {
  skip_if_no_file(STAB); skip_if_no_file(LOGIT)
  st <- read_csv(STAB, show_col_types = FALSE)

  expect_true(all(c("predictor", "retention_count", "retention_frequency",
                    "n_boot", "interpretation") %in% names(st)))
  expect_true(all(st$retention_frequency >= 0 & st$retention_frequency <= 100))
  expect_true(all(st$retention_count <= st$n_boot))

  fitted <- setdiff(all.vars(formula(readRDS(LOGIT))), "published_int")
  expect_setequal(st$predictor, fitted)

  # The label must follow the number, or it is decoration.
  expect_true(all(
    (st$retention_frequency >= 90 & st$interpretation == "robust") |
    (st$retention_frequency >= 70 & st$retention_frequency < 90 &
       grepl("moderate", st$interpretation)) |
    (st$retention_frequency < 70 & grepl("unstable", st$interpretation))
  ))
})

test_that("leave-one-congress-out refits every congress for every term", {
  skip_if_no_file(LOO); skip_if_no_file(LOGIT)
  fad <- here("output", "final_analytical_dataset.csv")
  skip_if_no_file(fad)

  l <- read_csv(LOO, show_col_types = FALSE)
  d <- read_csv(fad, show_col_types = FALSE)
  n_congress <- dplyr::n_distinct(d$congress_year)

  expect_true(all(c("term", "group_excluded", "ratio", "p_value",
                    "converged") %in% names(l)))
  expect_true(all(l$converged), info = "a leave-one-out refit failed to converge")

  per_term <- l |> count(term, name = "n_refits")
  expect_true(all(per_term$n_refits == n_congress),
              info = "not every congress was dropped for every term")

  # Every congress must appear, so no year is quietly skipped.
  expect_setequal(as.character(unique(l$group_excluded)),
                  as.character(unique(d$congress_year)))
})

test_that("the headline predictors survive dropping any single congress", {
  skip_if_no_file(LOO)
  l <- read_csv(LOO, show_col_types = FALSE)

  # is_rct and n_authors are the two terms the bootstrap calls robust. If
  # either depended on one congress - 2017 and 2018 have no abstract text at
  # all - that would be an artefact rather than a finding.
  for (tm in c("is_rctTRUE", "n_authors")) {
    sub <- l |> filter(term == tm)
    skip_if(nrow(sub) == 0, paste(tm, "not in this model"))
    expect_true(all(sub$ratio > 1),
                info = paste(tm, "changes direction on a leave-one-out refit"))
    expect_true(all(sub$p_value < 0.05),
                info = paste(tm, "loses significance when a congress is dropped"))
  }
})
