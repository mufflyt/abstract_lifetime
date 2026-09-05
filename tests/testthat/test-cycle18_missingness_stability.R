# Cycle 18 of 24. Mix: 3 BVA, 3 semantic, 4 adversarial.
#
# Target: the two diagnostics that tell the reader how much to trust the
# regression: R/06b_missingness.R and R/06d_model_stability.R. Both write
# artefacts that go straight into the manuscript's limitations, and neither had
# a test. Cycle 3 covered the model's own contracts and cycle 12 the covariates;
# nobody had asked whether the DIAGNOSTICS about them are internally coherent.
#
# The adversarial weighting is deliberate. These artefacts are the ones most
# likely to be stale: they are expensive to regenerate (n_boot resamples,
# leave-one-congress-out refits), so they survive changes to the dataset they
# describe.
#
# Vocabularies below are read out of the artefacts and the source, not invented.

library(testthat)
library(dplyr)

P_MISS  <- here::here("output", "missingness_by_variable.csv")
P_UNRES <- here::here("output", "unresolved_vs_evaluated.csv")
P_STAB  <- here::here("output", "model_predictor_stability.csv")
P_LOO   <- here::here("output", "model_leave_one_congress_out.csv")
P_SCR   <- here::here("output", "model_variable_screen.csv")
P_FINAL <- here::here("output", "final_analytical_dataset.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

# ============================================================
# BVA 18.1 - missingness percentages are bounded and match their counts
# ============================================================
test_that("pct_missing is a percentage and reconciles with n_missing over n_total", {
  need(P_MISS)
  m <- readr::read_csv(P_MISS, show_col_types = FALSE)
  expect_gt(nrow(m), 0)
  expect_true(all(m$pct_missing >= 0 & m$pct_missing <= 100),
              label = "a missingness percentage outside 0-100")
  expect_true(all(m$n_missing >= 0), label = "a negative missing count")
  expect_true(all(m$n_missing <= m$n_total),
              label = "more missing values than rows")
  # The reported percentage must be the one its own counts imply. A percentage
  # computed against a different denominator is how a limitation section
  # understates a gap.
  recomputed <- round(m$n_missing / m$n_total * 100, 1)
  bad <- which(abs(recomputed - m$pct_missing) > 0.15)
  expect_true(length(bad) == 0,
              label = sprintf("pct_missing does not reconcile for: %s",
                              paste(m$variable[bad], collapse = ", ")))
})

# ============================================================
# BVA 18.2 - retention frequency is a proportion of the resamples run
# ============================================================
test_that("bootstrap retention frequency is a proportion of n_boot", {
  need(P_STAB)
  s <- readr::read_csv(P_STAB, show_col_types = FALSE)
  skip_if(nrow(s) == 0, "no stability rows")
  # retention_frequency is a PERCENTAGE, not a proportion: 494 of 500 is
  # recorded as 98.8. My first version of this test asserted [0, 1] and failed
  # on every row. The column name is ambiguous, but the artefact is
  # unambiguous, so the contract asserted here is the one the file actually
  # holds. A misread is self-announcing rather than silent, since a proportion
  # above 1 is obviously wrong, so this is not registered as a finding.
  expect_true(all(s$retention_frequency >= 0 & s$retention_frequency <= 100),
              label = "retention_frequency is outside 0-100")
  expect_true(all(s$retention_count >= 0 & s$retention_count <= s$n_boot),
              label = "a predictor was retained more often than there were resamples")
  # count/n_boot must be the percentage. If not, one of the two is from a
  # different run, and "robust" would be asserted on the wrong denominator.
  expect_true(all(abs(s$retention_count / s$n_boot * 100 - s$retention_frequency) < 0.1),
              label = "retention_frequency does not equal 100 * retention_count / n_boot")
  expect_equal(length(unique(s$n_boot)), 1L,
               label = "predictors were bootstrapped different numbers of times")
})

# ============================================================
# BVA 18.3 - p-values are p-values wherever they appear
# ============================================================
test_that("every reported p-value lies in the unit interval", {
  offenders <- character(0)
  for (p in c(P_UNRES, P_LOO)) {
    if (!file.exists(p)) next
    d <- readr::read_csv(p, show_col_types = FALSE)
    if (!"p_value" %in% names(d)) next
    v <- d$p_value[!is.na(d$p_value)]
    if (length(v) && (any(v < 0) || any(v > 1))) {
      offenders <- c(offenders, sprintf("%s (range %.3g to %.3g)",
                                        basename(p), min(v), max(v)))
    }
  }
  expect_true(length(offenders) == 0,
              label = paste("p-values outside [0, 1] in:",
                            paste(offenders, collapse = ", ")))
})

# ============================================================
# SEMANTIC 18.4 - "unstable" and "robust" mean what the threshold says
# ============================================================
test_that("the stability label follows the retention frequency it describes", {
  need(P_STAB)
  s <- readr::read_csv(P_STAB, show_col_types = FALSE)
  skip_if(nrow(s) == 0 || !"interpretation" %in% names(s), "no interpretation column")
  rob <- s |> filter(grepl("^robust", interpretation))
  uns <- s |> filter(grepl("^unstable", interpretation))
  skip_if(nrow(rob) == 0 || nrow(uns) == 0, "only one label present")
  # Whatever the cutoff is, a predictor labelled robust must not be retained
  # LESS often than one labelled unstable. A label that does not order with the
  # quantity it summarises is worse than no label.
  expect_gt(min(rob$retention_frequency), max(uns$retention_frequency),
            label = sprintf(paste("the labels do not separate: lowest 'robust' is",
                                  "%.3f and highest 'unstable' is %.3f"),
                            min(rob$retention_frequency), max(uns$retention_frequency)))
})

# ============================================================
# SEMANTIC 18.5 - the screen's stated reason matches the evidence it records
# ============================================================
test_that("a variable dropped for missingness really is above the threshold", {
  need(P_SCR)
  scr <- readr::read_csv(P_SCR, show_col_types = FALSE)
  # The reason string at 06's screen is "more than 50% missing". If a variable
  # carries that reason while sitting below 50%, the screen and its audit trail
  # disagree and the exclusion cannot be defended.
  dropped <- scr |> filter(grepl("more than 50% missing", reason))
  skip_if(nrow(dropped) == 0, "nothing dropped for missingness")
  expect_true(all(dropped$pct_missing > 50, na.rm = TRUE),
              label = sprintf("variable(s) dropped for >50%% missing that are not: %s",
                              paste(dropped$variable[dropped$pct_missing <= 50],
                                    collapse = ", ")))
  kept <- scr |> filter(kept %in% c(TRUE, "TRUE"))
  if (nrow(kept) > 0) {
    expect_true(all(kept$pct_missing <= 50, na.rm = TRUE),
                label = sprintf("variable(s) kept despite >50%% missing: %s",
                                paste(kept$variable[kept$pct_missing > 50], collapse = ", ")))
  }
})

# ============================================================
# SEMANTIC 18.6 - the definitional/substantive split is applied as written
# ============================================================
test_that("only the two definitional variables carry the definitional label", {
  need(P_UNRES)
  u <- readr::read_csv(P_UNRES, show_col_types = FALSE)
  # 06b_missingness.R:155 fixes DEFINITIONAL to best_score and n_candidates:
  # unresolved abstracts ARE the mid-score band by construction, so a
  # difference in those two is meaningless while a difference in any other
  # covariate is a real signal about who fails to get adjudicated. Mislabelling
  # a substantive difference as definitional would explain away that signal.
  defn <- u |> filter(grepl("^definitional", interpretation))
  expect_setequal(defn$variable, c("best_score", "n_candidates"))
})

# ============================================================
# ADVERSARIAL 18.7 - the diagnostics describe the dataset shipped beside them
# ============================================================
test_that("missingness counts match the analytical dataset they describe", {
  need(P_MISS, P_FINAL)
  m <- readr::read_csv(P_MISS, show_col_types = FALSE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  # These artefacts are expensive to regenerate and cheap to leave behind, so
  # the realistic failure is a diagnostic describing a previous vintage of the
  # dataset. n_total is the cheapest place that shows.
  expect_true(all(m$n_total == nrow(f)),
              label = sprintf(paste("missingness_by_variable.csv reports n_total",
                                    "%s while the dataset has %d rows, so the",
                                    "diagnostic describes an older vintage"),
                              paste(unique(m$n_total), collapse = "/"), nrow(f)))
})

# ============================================================
# ADVERSARIAL 18.8 - a recomputed missing count still matches
# ============================================================
test_that("recomputing missingness from the dataset reproduces the report", {
  need(P_MISS, P_FINAL)
  m <- readr::read_csv(P_MISS, show_col_types = FALSE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  shared <- intersect(m$variable, names(f))
  skip_if(length(shared) == 0, "no shared variables")
  drift <- character(0)
  for (v in shared) {
    actual <- sum(is.na(f[[v]]))
    reported <- m$n_missing[m$variable == v][1]
    if (!is.na(reported) && actual != reported) {
      drift <- c(drift, sprintf("%s (reported %d, actual %d)", v, reported, actual))
    }
  }
  # Stronger than the row-count check: a dataset can keep its size while a
  # column's missingness changes completely, which is exactly what the gender
  # and country corrections did.
  expect_true(length(drift) == 0,
              label = paste("missingness counts no longer match the dataset:",
                            paste(utils::head(drift, 5), collapse = "; ")))
})

# ============================================================
# ADVERSARIAL 18.9 - leave-one-congress-out covers the congresses that exist
# ============================================================
test_that("the leave-one-out diagnostic excluded real congresses, all of them", {
  need(P_LOO, P_FINAL)
  l <- readr::read_csv(P_LOO, show_col_types = FALSE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  skip_if(!"group_excluded" %in% names(l), "no group_excluded column")
  excluded <- unique(as.character(l$group_excluded))
  present  <- unique(as.character(f$congress_year))
  # A congress in the diagnostic that is not in the data means the diagnostic
  # predates a cohort change; a congress in the data that was never left out
  # means the sensitivity analysis is incomplete and its claim of robustness
  # covers less than it says.
  expect_true(all(excluded %in% present),
              label = paste("congress(es) left out that are not in the dataset:",
                            paste(setdiff(excluded, present), collapse = ", ")))
  expect_true(all(present %in% excluded),
              label = paste("congress(es) never left out, so the robustness claim",
                            "does not cover them:",
                            paste(setdiff(present, excluded), collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 18.10 - a non-converged refit must not be read as evidence
# ============================================================
test_that("non-converged leave-one-out refits are flagged, not silently kept", {
  need(P_LOO)
  l <- readr::read_csv(P_LOO, show_col_types = FALSE)
  skip_if(!"converged" %in% names(l), "no converged column")
  is_true <- function(x) x %in% c(TRUE, "TRUE")
  bad <- l |> filter(!is_true(converged))
  # A refit that did not converge produces an estimate that means nothing. If
  # such a row carries a finite estimate and a p-value, anything reading this
  # table downstream will treat it as a result.
  if (nrow(bad) > 0) {
    expect_true(all(is.na(bad$estimate)) && all(is.na(bad$p_value)),
                label = sprintf(paste("%d leave-one-out refit(s) did not converge",
                                      "but still report an estimate or p-value"),
                                nrow(bad)))
  } else {
    succeed("every leave-one-out refit converged")
  }
})
