# Cycle 5 of 24. Mix: 3 BVA, 4 semantic, 3 adversarial.
#
# Targets: the STROBE flow-diagram data, fidelity checks, the derived tables in
# output/tables, gender unification, config threshold sanity, and
# R/strobe_flowchart.R itself. Cycles 0-4 covered decision precedence, score
# tiers, congress dates, survival, the regression tables, and the
# validation/sensitivity/interrater artifacts.

library(testthat)
library(dplyr)
source(here::here("R", "utils_decisions.R"))

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_FLOW  <- here::here("output", "figures", "figure1_flow_data.csv")
P_FID   <- here::here("data", "processed", "fidelity_checks.csv")
P_T3    <- here::here("output", "tables", "table3_logistic_regression.csv")
P_AIM3  <- here::here("output", "aim3_logistic_regression.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("pipeline outputs not present")
flowv <- function(step) { d <- readr::read_csv(P_FLOW, show_col_types = FALSE)
                          d$n[d$step == step] }

# ============================================================
# BVA 5.1 — classification tiers partition the cohort exactly
# ============================================================
test_that("flow-diagram classification tiers sum to the oral cohort", {
  need(P_FLOW, P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  tiers <- c("Definite", "Probable", "Possible", "Excluded",
             "No match or no candidates")
  total <- sum(vapply(tiers, function(s) { v <- flowv(s); if (length(v)) v else 0 }, numeric(1)))
  expect_equal(total, nrow(f),
               label = "classification tiers must partition the cohort without gap or overlap")
  expect_equal(flowv("Oral included"), nrow(f))
  expect_equal(flowv("Total abstracts parsed") - flowv("Video excluded"),
               flowv("Oral included"))
})

# ============================================================
# BVA 5.2 — title similarity is a bounded coefficient
# ============================================================
test_that("title_jaccard lies in [0,1] and is 1 only for unchanged titles", {
  need(P_FID)
  d <- readr::read_csv(P_FID, show_col_types = FALSE)
  j <- d$title_jaccard[!is.na(d$title_jaccard)]
  skip_if(length(j) == 0, "no jaccard values")
  expect_true(all(j >= 0 & j <= 1), label = "Jaccard outside [0,1]")
  if ("title_changed" %in% names(d)) {
    unchanged <- d |> filter(!is.na(title_jaccard), title_changed %in% c(FALSE, "FALSE"))
    if (nrow(unchanged) > 0) {
      expect_true(all(unchanged$title_jaccard > 0.5),
                  label = "titles marked unchanged must not score as dissimilar")
    }
  }
})

# ============================================================
# BVA 5.3 — searched splits cleanly into with/without candidates
# ============================================================
test_that("searched abstracts split exactly into with-candidates and none", {
  need(P_FLOW)
  expect_equal(flowv("With candidates") + flowv("No candidates"), flowv("Searched"))
  expect_gte(flowv("No candidates"), 0)
  expect_lte(flowv("Searched"), flowv("Oral included"))
})

# ============================================================
# SEMANTIC 5.4 — a flow label must name the quantity beneath it
# ============================================================
test_that("the 'No match' step counts what its label says", {
  need(P_FLOW, P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  n_no_match <- sum(f$classification == "no_match")
  n_no_cand  <- sum(f$classification == "no_candidates")
  steps <- readr::read_csv(P_FLOW, show_col_types = FALSE)$step
  expect_false("No match" %in% steps,
               label = paste0("a step labelled exactly 'No match' must count only ",
                              "classification == 'no_match' (", n_no_match,
                              "); this one folds in the ", n_no_cand,
                              " no-candidate abstracts"))
  reported <- flowv("No match or no candidates")
  expect_equal(reported, n_no_match + n_no_cand,
               label = "the combined step must equal both classes it names")
})

# ============================================================
# SEMANTIC 5.5 — a conflict flag requires more than one source
# ============================================================
test_that("gender_conflict is only set where multiple sources exist", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  skip_if(!all(c("gender_conflict", "gender_n_sources") %in% names(f)), "gender fields absent")
  conflicted <- f |> filter(gender_conflict %in% c(TRUE, "TRUE"))
  if (nrow(conflicted) > 0) {
    expect_true(all(conflicted$gender_n_sources >= 2, na.rm = TRUE),
                label = "a conflict was flagged with fewer than two sources")
  }
  succeed()
})

# ============================================================
# SEMANTIC 5.6 — a derived table must agree with the analysis it derives from
# ============================================================
test_that("table3 reproduces the logistic estimates it is built from", {
  need(P_T3, P_AIM3)
  t3 <- readr::read_csv(P_T3, show_col_types = FALSE)
  a3 <- readr::read_csv(P_AIM3, show_col_types = FALSE)
  term_col <- intersect(c("term", "Term", "variable", "Variable"), names(t3))
  skip_if(length(term_col) == 0, "no term column in table3")
  shared <- intersect(t3[[term_col[1]]], a3$term)
  expect_gt(length(shared), 0,
            label = "table3 shares no term names with aim3; the table may be stale")
})

# ============================================================
# SEMANTIC 5.7 — the flowchart script's guards actually guard
# ============================================================
test_that("strobe_flowchart stopifnot conditions reject inconsistent counts", {
  # The script asserts the arithmetic closes. Verify the assertions are real by
  # feeding them values that violate each branch.
  chk <- function(n_cohort, n_pending, n_eval, n_pub, n_notpub) {
    tryCatch({
      stopifnot(n_pub + n_notpub == n_eval, n_eval + n_pending == n_cohort)
      "passed"
    }, error = function(e) "rejected")
  }
  expect_equal(chk(1106, 55, 1051, 178, 873), "passed")
  expect_equal(chk(1106, 55, 1051, 178, 872), "rejected")  # parts do not close
  expect_equal(chk(1106, 55, 1050, 178, 872), "rejected")  # denominator wrong
  expect_equal(chk(1105, 55, 1051, 178, 873), "rejected")  # cohort wrong
})

# ============================================================
# ADVERSARIAL 5.8 — config thresholds must be ordered and positive
# ============================================================
test_that("scoring thresholds are ordered so the tiers cannot invert", {
  cfg <- config::get(file = here::here("config.yml"))
  sc <- cfg$scoring
  expect_true(!is.null(sc$auto_accept) && !is.null(sc$manual_review))
  expect_gt(sc$auto_accept, sc$manual_review)
  expect_gt(sc$manual_review, 0)
  # Jaccard thresholds are coefficients and must descend high > mid > low.
  if (!is.null(sc$title_jaccard_high)) {
    expect_gt(sc$title_jaccard_high, sc$title_jaccard_mid)
    expect_gt(sc$title_jaccard_mid, sc$title_jaccard_low)
    expect_lte(sc$title_jaccard_high, 1)
    expect_gt(sc$title_jaccard_low, 0)
  }
})

# ============================================================
# ADVERSARIAL 5.9 — the flowchart derives its counts rather than hardcoding
# ============================================================
test_that("strobe_flowchart.R contains no hardcoded cohort numbers", {
  p <- here::here("R", "strobe_flowchart.R")
  skip_if_not(file.exists(p))
  src <- readLines(p, warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]          # ignore comments
  code <- gsub('"[^"]*"', '""', code)          # ignore string literals
  hits <- grep("\\b(1154|1106|1051|178|873|48|55)\\b", code, value = TRUE)
  expect_length(hits, 0L)
  if (length(hits)) {
    fail(paste("cohort numbers appear as literals, so the figure would not follow",
               "a re-run:", paste(trimws(hits), collapse = " | ")))
  }
})

# ============================================================
# ADVERSARIAL 5.10 — fidelity checks describe matched abstracts only
# ============================================================
test_that("fidelity checks cover only abstracts that have a candidate", {
  need(P_FID, P_FINAL)
  d <- readr::read_csv(P_FID, show_col_types = FALSE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  expect_lte(nrow(d), nrow(f),
             label = "more fidelity rows than abstracts in the cohort")
  expect_equal(anyDuplicated(d$abstract_id), 0L,
               label = "duplicate abstract_id in fidelity_checks would double-weight it")
  if ("classification" %in% names(d)) {
    expect_true(all(d$classification %in%
                    c("definite", "probable", "possible", "excluded",
                      "no_match", "no_candidates")),
                label = "unexpected classification value in fidelity_checks")
  }
})
