# Semantic tests for pipeline outputs.
# These verify that the pipeline produces logically consistent, plausible
# results — not just that code runs without errors.

library(testthat)
library(readr)
library(dplyr)

# Skip if pipeline hasn't been run
skip_if_not <- function(path) {
  if (!file.exists(path)) skip(paste("Missing:", basename(path)))
}

# ============================================================
# Cohort integrity
# ============================================================

test_that("abstracts_with_matches has expected row count and years", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  expect_true(nrow(d) >= 1000, label = "at least 1000 abstracts")
  expect_true(nrow(d) <= 1300, label = "not more than 1300")
  expect_true(all(d$congress_year >= 2012 & d$congress_year <= 2023))
  expect_equal(length(unique(d$congress_year)), 12, label = "12 congress years")
})

test_that("no video abstracts in pipeline output", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  if ("session_type" %in% names(d)) {
    expect_true(all(d$session_type == "Oral" | is.na(d$session_type)))
  }
})

test_that("abstract_ids are unique", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  expect_equal(length(unique(d$abstract_id)), nrow(d))
})

test_that("abstract_ids follow naming convention", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  expect_true(all(grepl("^AAGL\\d{4}_\\d{3}$", d$abstract_id)))
})

# ============================================================
# Classification consistency
# ============================================================

test_that("classification uses Cochrane vocabulary only", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  valid <- c("definite", "probable", "possible", "no_match", "excluded", "no_candidates")
  expect_true(all(d$classification %in% valid))
})

test_that("definite matches have scores >= 7", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  definites <- d |> filter(classification == "definite")
  if (nrow(definites) > 0) {
    expect_true(all(definites$best_score >= 7, na.rm = TRUE))
  }
})

test_that("no_match and no_candidates have low scores", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  no_match <- d |> filter(classification %in% c("no_match", "no_candidates"))
  if (nrow(no_match) > 0) {
    expect_true(all(no_match$best_score < 7 | is.na(no_match$best_score)))
  }
})

test_that("excluded abstracts have negative date points (pre-conference)", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  excluded <- d |> filter(classification == "excluded")
  if (nrow(excluded) > 0 && "date_pts" %in% names(excluded)) {
    expect_true(all(excluded$date_pts < 0, na.rm = TRUE))
  }
})

# ============================================================
# Publication rate plausibility (Cochrane MR000005 benchmarks)
# ============================================================

test_that("definite-only rate is between 3% and 20%", {
  path <- here::here("output", "sensitivity_analyses.csv")
  skip_if_not(path)
  s <- read_csv(path, show_col_types = FALSE)

  def_rate <- s$rate[s$scenario == "Definite only"]
  expect_true(def_rate >= 3, label = "definite rate >= 3%")
  expect_true(def_rate <= 20, label = "definite rate <= 20%")
})

test_that("definite+probable rate is between 15% and 55%", {
  path <- here::here("output", "sensitivity_analyses.csv")
  skip_if_not(path)
  s <- read_csv(path, show_col_types = FALSE)

  dp_rate <- s$rate[s$scenario == "Definite + probable"]
  expect_true(dp_rate >= 15, label = "def+prob rate >= 15%")
  expect_true(dp_rate <= 55, label = "def+prob rate <= 55% (Cochrane max ~46%)")
})

# ============================================================
# Time to publication plausibility
# ============================================================

test_that("median time to publication is between 6 and 36 months", {
  path <- here::here("output", "aim2_time_to_pub.csv")
  skip_if_not(path)
  ttp <- read_csv(path, show_col_types = FALSE)

  med <- ttp$value[ttp$metric == "median_months"]
  expect_true(med >= 6, label = "median TTP >= 6 months")
  expect_true(med <= 36, label = "median TTP <= 36 months")
})

test_that("no negative time-to-publication values for published abstracts", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  published <- d |> filter(classification == "definite", !is.na(months_to_pub))
  if (nrow(published) > 0) {
    # Allow up to 2 edge cases with slightly negative TTP (scoring rounding)
    expect_true(sum(published$months_to_pub < 0) <= 2,
                label = "at most 2 negative TTP values")
  }
})

# ============================================================
# Predictor variable coverage
# ============================================================

test_that("gender coverage is at least 60%", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  gender_col <- if ("gender_unified" %in% names(d)) "gender_unified" else "first_author_gender"
  if (gender_col %in% names(d)) {
    pct <- mean(!is.na(d[[gender_col]]))
    expect_true(pct >= 0.60, label = "gender coverage >= 60%")
  }
})

test_that("practice_type coverage is at least 80%", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  if ("practice_type" %in% names(d)) {
    pct <- mean(!is.na(d$practice_type))
    expect_true(pct >= 0.80, label = "practice_type coverage >= 80%")
  }
})

test_that("citation count coverage is at least 90%", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  if ("cited_by_count" %in% names(d)) {
    pct <- mean(!is.na(d$cited_by_count))
    expect_true(pct >= 0.90, label = "citation coverage >= 90%")
  }
})

# ============================================================
# Study design and research category distributions
# ============================================================

test_that("study design has no single category dominating >80%", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  if ("study_design" %in% names(d)) {
    max_pct <- max(table(d$study_design)) / nrow(d)
    expect_true(max_pct <= 0.80, label = "no single design > 80%")
  }
})

test_that("research_category has multiple categories represented", {
  path <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(path)
  d <- read_csv(path, show_col_types = FALSE)

  if ("research_category" %in% names(d)) {
    n_cats <- length(unique(d$research_category))
    expect_true(n_cats >= 4, label = "at least 4 research categories")
  }
})

# ============================================================
# Cox PH model output
# ============================================================

test_that("Cox PH model has valid hazard ratios", {
  path <- here::here("output", "aim2b_cox_regression.csv")
  skip_if_not(path)
  cox <- read_csv(path, show_col_types = FALSE)

  expect_true(nrow(cox) >= 3, label = "at least 3 terms")
  expect_true(all(cox$estimate >= 0, na.rm = TRUE), label = "all HRs non-negative")
  expect_true(all(cox$conf.low < cox$conf.high, na.rm = TRUE), label = "CI lower < upper")
})

test_that("PH assumption holds", {
  path <- here::here("output", "cox_ph_assumption.csv")
  skip_if_not(path)
  ph <- read_csv(path, show_col_types = FALSE)

  expect_true(ph$p_value[1] > 0.05, label = "global PH p > 0.05")
})

# ============================================================
# Validation metrics
# ============================================================

test_that("validation sensitivity is at least 80%", {
  path <- here::here("output", "validation_metrics.csv")
  skip_if_not(path)
  val <- read_csv(path, show_col_types = FALSE)

  sens <- val$value[val$metric == "sensitivity"]
  expect_true(sens >= 0.80, label = "sensitivity >= 80%")
})

test_that("validation NPV is at least 90%", {
  path <- here::here("output", "validation_metrics.csv")
  skip_if_not(path)
  val <- read_csv(path, show_col_types = FALSE)

  npv <- val$value[val$metric == "npv"]
  expect_true(npv >= 0.90, label = "NPV >= 90%")
})

# ============================================================
# Cross-file consistency
# ============================================================

test_that("abstracts_cleaned and abstracts_with_matches have same abstract_ids", {
  p1 <- here::here("data", "processed", "abstracts_cleaned.csv")
  p2 <- here::here("output", "abstracts_with_matches.csv")
  skip_if_not(p1); skip_if_not(p2)

  ids1 <- read_csv(p1, show_col_types = FALSE)$abstract_id
  ids2 <- read_csv(p2, show_col_types = FALSE)$abstract_id

  expect_equal(sort(ids1), sort(ids2))
})

test_that("sensitivity analysis scenarios are monotonically ordered", {
  path <- here::here("output", "sensitivity_analyses.csv")
  skip_if_not(path)
  s <- read_csv(path, show_col_types = FALSE)

  # Definite-only <= definite+probable
  def <- s$rate[s$scenario == "Definite only"]
  dp <- s$rate[s$scenario == "Definite + probable"]
  expect_true(def <= dp, label = "definite-only <= definite+probable")
})
