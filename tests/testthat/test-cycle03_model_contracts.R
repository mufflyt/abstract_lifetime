# Cycle 3 of 24. Mix: 3 BVA, 3 semantic, 4 adversarial.
#
# Targets: the logistic and Cox regression outputs, complete-case attrition,
# the >=50% missing exclusion rule the code applies to candidate predictors,
# determinism, and artifact vintage. Cycles 0-2 covered decision precedence,
# score tiers, congress-date contracts, survival censoring and per-year
# denominators; none touched the model tables.

library(testthat)
library(dplyr)
source(here::here("R", "utils_decisions.R"))

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_AIM3  <- here::here("output", "aim3_logistic_regression.csv")
P_COX   <- here::here("output", "aim2b_cox_regression.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("pipeline outputs not present")

# ============================================================
# BVA 3.1 — odds/hazard ratios are positive, finite, and bracketed by their CI
# ============================================================
test_that("every ratio is positive and finite and lies inside its own interval", {
  need(P_AIM3, P_COX)
  for (p in c(P_AIM3, P_COX)) {
    m <- readr::read_csv(p, show_col_types = FALSE)
    expect_true(all(m$estimate > 0),
                label = paste(basename(p), "- exponentiated ratios must be > 0"))
    expect_true(all(is.finite(m$estimate)),
                label = paste(basename(p), "- non-finite estimate signals separation"))
    if (all(c("conf.low", "conf.high") %in% names(m))) {
      ok <- is.na(m$conf.low) | is.na(m$conf.high) |
            (m$conf.low <= m$estimate & m$estimate <= m$conf.high)
      expect_true(all(ok),
                  label = paste(basename(p), "- estimate outside its confidence interval"))
    }
  }
})

# ============================================================
# BVA 3.2 — interval width flags sparse or separated terms
# ============================================================
# PRESERVED FAILING TEST — open scientific decision, see tests/loop/LEDGER.md.
# has_funding is TRUE for 3 of 1,051 evaluated abstracts (2 unpublished, 1
# published). Its odds ratio 2.609 (0.117 to 29.04) is estimated from a single
# event and carries no information, yet the draft abstract reports declared
# funding as a non-significant predictor. "Not significant" and "not estimable"
# are different claims. Resolving this means either dropping the term from the
# model specification or reporting it as not estimable; both change what the
# manuscript may say, so neither is chosen here.
test_that("no term is reported with an uninterpretably wide interval", {
  need(P_AIM3)
  m <- readr::read_csv(P_AIM3, show_col_types = FALSE) |>
    filter(term != "(Intercept)", !is.na(conf.low), !is.na(conf.high), conf.low > 0)
  m$ratio_width <- m$conf.high / m$conf.low
  worst <- m[which.max(m$ratio_width), ]
  # A 100-fold span means the data cannot locate the effect at all. Reporting
  # such a term alongside tight ones invites reading it as a real null.
  expect_lt(worst$ratio_width, 100,
            label = paste0("term '", worst$term, "' has CI ", worst$conf.low,
                           " to ", worst$conf.high, " (", round(worst$ratio_width),
                           "-fold). Too sparse to report as estimated."))
})

# ============================================================
# BVA 3.3 — p-values lie in [0, 1] and a printed 0 means "below precision"
# ============================================================
test_that("p-values are within range and rounded zeros are not literal zeros", {
  need(P_AIM3, P_COX)
  for (p in c(P_AIM3, P_COX)) {
    m <- readr::read_csv(p, show_col_types = FALSE)
    expect_true(all(m$p.value >= 0 & m$p.value <= 1),
                label = paste(basename(p), "- p.value outside [0,1]"))
    # Values are rounded to 3 dp on export, so 0 means < 0.0005. Any consumer
    # printing "p = 0" is misreporting; assert the rounding is what produced it.
    zeros <- m[m$p.value == 0, ]
    if (nrow(zeros) > 0) expect_true(all(abs(zeros$statistic) > 2),
                                     label = "a p-value of 0 with a small statistic is implausible")
  }
})

# ============================================================
# SEMANTIC 3.4 — the tables report ratios, not log-odds
# ============================================================
test_that("estimates are exponentiated ratios, not coefficients on the log scale", {
  need(P_AIM3)
  m <- readr::read_csv(P_AIM3, show_col_types = FALSE)
  icpt <- m$estimate[m$term == "(Intercept)"]
  # On the log scale an intercept for a ~17% outcome would be about -1.6.
  # On the ratio scale it is a small positive number. Negative values here would
  # mean the exponentiate=TRUE contract silently changed.
  expect_true(all(m$estimate > 0))
  expect_lt(icpt, 1)
  expect_gt(icpt, 0)
})

# ============================================================
# SEMANTIC 3.5 — a reported effect table must carry the N it was fitted on
# ============================================================
test_that("the logistic table reports the sample size it was estimated from", {
  need(P_AIM3)
  m <- readr::read_csv(P_AIM3, show_col_types = FALSE)
  # Must be a COLUMN carrying the fitted sample size. An earlier draft of this
  # test also matched m$term, which passed spuriously on the predictor
  # "n_authors" — a coefficient, not an N.
  has_n <- any(names(m) %in% c("n_obs", "nobs", "n_model", "n"))
  expect_true(has_n,
              label = paste("aim3_logistic_regression.csv reports odds ratios with no N.",
                            "The model is complete-case, so its N is smaller than the",
                            "denominator and cannot be recovered from the file."))
})

# ============================================================
# SEMANTIC 3.6 — shared predictors agree in direction across the two models
# ============================================================
test_that("logistic and Cox agree on the sign of shared predictors", {
  need(P_AIM3, P_COX)
  a <- readr::read_csv(P_AIM3, show_col_types = FALSE)
  c_ <- readr::read_csv(P_COX, show_col_types = FALSE)
  shared <- intersect(a$term, c_$term)
  skip_if(length(shared) == 0, "no shared terms")
  for (t in shared) {
    ae <- a$estimate[a$term == t]; ce <- c_$estimate[c_$term == t]
    # Both are ratios, so "same side of 1" is the direction contract. A predictor
    # that raises the odds of ever publishing should not lower the hazard of
    # publishing sooner.
    expect_equal(sign(ae - 1), sign(ce - 1),
                 label = paste0("term '", t, "' points opposite ways: logistic ",
                                ae, " vs Cox ", ce))
  }
})

# ============================================================
# ADVERSARIAL 3.7 — the >=50% missing exclusion rule actually holds
# ============================================================
test_that("no model term comes from a variable that is >=50% missing", {
  need(P_FINAL, P_AIM3)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  m <- readr::read_csv(P_AIM3, show_col_types = FALSE) |> filter(term != "(Intercept)")
  # Map "is_rctTRUE" / "gender_unifiedmale" back to a column by longest prefix.
  base_of <- function(term) {
    hits <- names(f)[startsWith(term, names(f))]
    if (length(hits) == 0) return(NA_character_)
    hits[which.max(nchar(hits))]
  }
  bases <- vapply(m$term, base_of, character(1))
  known <- bases[!is.na(bases)]
  skip_if(length(known) == 0, "could not map any term to a column")
  miss <- vapply(known, function(v) mean(is.na(f[[v]])), numeric(1))
  bad <- names(miss)[miss >= 0.5]
  expect_length(bad, 0L)
  if (length(bad)) fail(paste("model includes variables that are >=50% missing:",
                              paste(sprintf("%s (%.0f%%)", bad, 100 * miss[bad]),
                                    collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 3.8 — complete-case attrition must be visible, not silent
# ============================================================
test_that("complete-case attrition does not quietly halve the model cohort", {
  need(P_FINAL, P_AIM3)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  s <- publication_rate_summary(f)
  m <- readr::read_csv(P_AIM3, show_col_types = FALSE) |> filter(term != "(Intercept)")
  vars <- unique(na.omit(vapply(m$term, function(term) {
    hits <- names(f)[startsWith(term, names(f))]
    if (length(hits) == 0) NA_character_ else hits[which.max(nchar(hits))]
  }, character(1))))
  skip_if(length(vars) == 0, "no mappable terms")
  eval_rows <- f[!is.na(f$final_published), , drop = FALSE]
  complete <- sum(stats::complete.cases(eval_rows[, vars, drop = FALSE]))
  # Not a hard scientific threshold: a model fitted on under half the evaluated
  # cohort is a different population and must be reported as such.
  expect_gte(complete / s$n_evaluated, 0.5,
             label = sprintf("model fits on %d of %d evaluated abstracts (%.0f%%)",
                             complete, s$n_evaluated, 100 * complete / s$n_evaluated))
})

# ============================================================
# ADVERSARIAL 3.9 — model fitting is deterministic
# ============================================================
test_that("glm coefficients are identical across repeated fits and RNG states", {
  # Overlapping groups on purpose: perfect separation would make glm fail to
  # converge and the determinism claim would be tested on a degenerate fit.
  d <- data.frame(
    y  = rep(c(1L, 0L, 0L, 1L, 0L), 20),
    x1 = rep(c(TRUE, TRUE, FALSE, FALSE, TRUE), 20),
    x2 = rep(seq_len(20) / 20, each = 5))
  set.seed(1);   f1 <- stats::glm(y ~ x1 + x2, data = d, family = binomial())
  set.seed(999); f2 <- stats::glm(y ~ x1 + x2, data = d, family = binomial())
  f3 <- stats::glm(y ~ x1 + x2, data = d[sample(nrow(d)), ], family = binomial())
  expect_equal(unname(coef(f1)), unname(coef(f2)), tolerance = 0)
  expect_equal(unname(coef(f1)), unname(coef(f3)), tolerance = 1e-8,
               label = "row order changed the fitted coefficients")
})

# ============================================================
# ADVERSARIAL 3.10 — model artifacts share the vintage of the analytic dataset
# ============================================================
test_that("model outputs are not staler than the dataset they claim to describe", {
  need(P_FINAL, P_AIM3, P_COX)
  t_final <- file.info(P_FINAL)$mtime
  for (p in c(P_AIM3, P_COX)) {
    # Content vintage cannot be checked without re-fitting; mtime ordering is
    # the weak signal available. A model artifact OLDER than the dataset is
    # unambiguously stale regardless of clock skew within a single run.
    expect_gte(as.numeric(difftime(file.info(p)$mtime, t_final, units = "secs")), -60,
               label = paste(basename(p), "predates final_analytical_dataset.csv"))
  }
})
