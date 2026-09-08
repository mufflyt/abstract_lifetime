# The bug these exist for: the diagnostics layer assumed one numeric violator.
# It built two sensitivity fits, reported the global p of the first, and
# labelled it with a description of the second. The committed artifact said
# remediation = "time_varying:n_authors;strata:is_us_based" beside
# remediated_global_p = 0.607, where 0.607 came from a model stratifying on
# both. Nothing checked that the label and the number described one model.

source(testthat::test_path("..", "..", "R", "utils_ph.R"))
suppressPackageStartupMessages(library(survival))

fake_zph <- function(p) {
  tab <- cbind(chisq = rep(1, length(p)), df = rep(1, length(p)), p = unname(p))
  rownames(tab) <- names(p)
  structure(list(table = tab), class = "cox.zph")
}
DAT <- data.frame(
  a = rnorm(50), b = rnorm(50),
  flag = rep(c(TRUE, FALSE), 25),
  grp = factor(rep(c("x", "y"), 25)),
  odd = as.Date("2020-01-01") + 1:50,
  stringsAsFactors = FALSE)

test_that("zero violators is a clean, distinguishable state", {
  r <- ph_identify_violators(fake_zph(c(a = .9, b = .8, GLOBAL = .7)), c("a", "b"))
  expect_equal(r$violators, character(0))
  expect_equal(r$global_p, 0.7)
  expect_equal(nrow(ph_plan_remediation(r$violators, DAT)), 0L)
})

test_that("a missing PH result is not mistaken for a passing one", {
  r <- ph_identify_violators(NULL, c("a", "b"))
  expect_true(is.na(r$global_p))
  expect_equal(r$violators, character(0))
})

test_that("one numeric violator gets a re-testable production rule and a tt sensitivity", {
  r <- ph_identify_violators(fake_zph(c(a = .001, b = .8, GLOBAL = .01)), c("a", "b"))
  p <- ph_plan_remediation(r$violators, DAT)
  expect_equal(p$variable, "a")
  expect_equal(p$variable_type, "numeric")
  expect_equal(p$remediation, "strata_binned")
  expect_equal(p$sensitivity, "time_varying")
})

test_that("one categorical violator is stratified and has no tt sensitivity", {
  r <- ph_identify_violators(fake_zph(c(grp = .001, a = .8, GLOBAL = .01)), c("grp", "a"))
  p <- ph_plan_remediation(r$violators, DAT)
  expect_equal(p$variable_type, "factor")
  expect_equal(p$remediation, "strata")
  expect_true(is.na(p$sensitivity))
})

test_that("two violators of different types are BOTH detected and BOTH remediated", {
  # The exact shape of the production defect: is_us_based (logical) and
  # n_authors (numeric) violating together.
  r <- ph_identify_violators(
    fake_zph(c(flag = .005, a = .003, b = .9, GLOBAL = .01)), c("flag", "a", "b"))
  expect_setequal(r$violators, c("flag", "a"))
  p <- ph_plan_remediation(r$violators, DAT)
  expect_equal(nrow(p), 2L)
  expect_setequal(p$variable_type, c("logical", "numeric"))
  expect_true(all(nzchar(p$remediation)))
  # Both leave the linear predictor: neither is silently retained.
  f <- ph_production_formula(c("flag", "a", "b"), p)
  expect_true(grepl("strata(flag)", deparse1(f), fixed = TRUE))
  expect_true(grepl("strata(a)", deparse1(f), fixed = TRUE))
  expect_true(grepl("b", deparse1(f), fixed = TRUE))
})

test_that("many numeric violators are all remediated, not just the first", {
  r <- ph_identify_violators(
    fake_zph(c(a = .001, b = .002, GLOBAL = .001)), c("a", "b"))
  p <- ph_plan_remediation(r$violators, DAT)
  expect_equal(nrow(p), 2L)
  tv <- ph_timevarying_formula(c("a", "b"), p)
  expect_true(grepl("tt(a)", deparse1(tv), fixed = TRUE))
  expect_true(grepl("tt(b)", deparse1(tv), fixed = TRUE))
})

test_that("an unregistered variable type fails closed rather than passing silently", {
  r <- ph_identify_violators(fake_zph(c(odd = .001, GLOBAL = .01)), "odd")
  expect_error(ph_plan_remediation(r$violators, DAT), "No registered PH remediation rule")
  # A violator absent from the data is equally refused.
  expect_error(ph_plan_remediation("ghost", DAT), "No registered PH remediation rule")
})

test_that("cox.zph is undefined for tt() models, and that is reported not hidden", {
  # This is the constraint the old code walked into: it is impossible to obtain
  # a post-remediation global p from a tt() fit, so any reported value came
  # from a different model.
  set.seed(1)
  d <- data.frame(time = rexp(120, .1) + .1, event = rbinom(120, 1, .6),
                  x = rnorm(120), z = rnorm(120))
  m_tt <- suppressWarnings(coxph(Surv(time, event) ~ x + z + tt(x), data = d,
                                 tt = function(x, t, ...) x * log(t)))
  out <- ph_retest(m_tt)
  expect_true(is.na(out$global_p))
  expect_equal(out$status, "undefined_for_tt_models")

  m_ok <- suppressWarnings(coxph(Surv(time, event) ~ x + z, data = d))
  expect_equal(ph_retest(m_ok)$status, "ok")
  expect_false(is.na(ph_retest(m_ok)$global_p))

  expect_equal(ph_retest(NULL)$status, "model_not_fitted")
})

test_that("model comparison is produced with two violators, not silently skipped", {
  set.seed(2)
  d <- data.frame(time = rexp(200, .1) + .1, event = rbinom(200, 1, .6),
                  x = rnorm(200), z = rnorm(200), g = rep(c(TRUE, FALSE), 100))
  m0 <- suppressWarnings(coxph(Surv(time, event) ~ x + z + g, data = d))
  m1 <- suppressWarnings(coxph(Surv(time, event) ~ z + strata(g) + strata(x), data = d))
  cmp <- ph_model_comparison(list(original = m0, production = m1, missing = NULL))
  expect_setequal(cmp$model, c("original", "production"))
  expect_false(any(is.na(cmp$aic)))
  expect_equal(cmp$delta_aic_vs_original[cmp$model == "original"], 0)
})

test_that("remediation that still fails the PH gate is visible as a value, not an omission", {
  # A production fit can be re-tested and still violate. The artifact must carry
  # the number so the failure is legible rather than absent.
  out <- ph_retest(NULL)
  expect_equal(out$status, "model_not_fitted")
  r <- ph_identify_violators(fake_zph(c(a = .001, GLOBAL = .002)), "a")
  expect_lt(r$global_p, 0.05)
})

test_that("production formula never implies a single violator by position", {
  p <- ph_plan_remediation(c("flag", "grp"), DAT)
  f <- deparse1(ph_production_formula(c("flag", "grp", "a"), p))
  expect_true(grepl("strata(flag)", f, fixed = TRUE))
  expect_true(grepl("strata(grp)", f, fixed = TRUE))
  # Stratifying the only covariate leaves a fit that estimates nothing, which
  # is refused rather than returned as a valid model.
  expect_error(ph_production_formula(c("flag"), ph_plan_remediation("flag", DAT)),
               "removed every covariate")
})
