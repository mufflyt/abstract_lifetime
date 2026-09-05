# Cycle 23 of 24. Mix: 3 BVA, 4 semantic, 3 adversarial.
#
# Target: the two artefacts that tell a reader how good the matching is.
#   output/validation_metrics.csv   from R/validation_gold_standard.R
#   data/processed/fidelity_checks.csv from R/09e_fidelity_checks.R
#
# These are the study's self-assessment. Sensitivity 1.00, PPV 0.50 and accuracy
# 0.735 are quoted numbers, and the fidelity table is the evidence for whether a
# matched publication is really the same work as the abstract. Cycle 4 touched
# validation sensitivity; neither artefact's internal arithmetic had been
# checked, and the fidelity table had never been tested at all.
#
# Contracts read from the source and the artefacts:
#   validation_gold_standard.R:74  sensitivity = tp / (tp + fn), etc.
#   09e_fidelity_checks.R:48       title_changed is title_jaccard < 0.7
#   Jaccard is a similarity in [0, 1]

library(testthat)
library(dplyr)

P_VAL <- here::here("output", "validation_metrics.csv")
P_FID <- here::here("data", "processed", "fidelity_checks.csv")
P_GS  <- here::here("data", "validation", "gold_standard.csv")
P_FINAL <- here::here("output", "final_analytical_dataset.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

vmetrics <- function() {
  v <- readr::read_csv(P_VAL, show_col_types = FALSE)
  setNames(as.numeric(v$value), v$metric)
}

# ============================================================
# BVA 23.1 - the confusion matrix adds up to the sample it describes
# ============================================================
test_that("the validation counts reconcile with n_classified", {
  need(P_VAL)
  m <- vmetrics()
  req <- c("n", "n_classified", "true_positives", "false_positives",
           "false_negatives", "true_negatives")
  skip_if(!all(req %in% names(m)), "metrics missing")
  cells <- m[["true_positives"]] + m[["false_positives"]] +
           m[["false_negatives"]] + m[["true_negatives"]]
  # Cycle 4 found the accuracy denominator had been 0.720 against n rather than
  # 0.735 against n_classified. The four cells must sum to the CLASSIFIED count,
  # not the sampled one, or every rate below is computed on the wrong base.
  expect_equal(cells, m[["n_classified"]],
               label = sprintf("the confusion matrix sums to %g against n_classified %g",
                               cells, m[["n_classified"]]))
  expect_lte(m[["n_classified"]], m[["n"]])
  expect_true(all(m[req] >= 0), label = "a negative count in the confusion matrix")
})

# ============================================================
# BVA 23.2 - every rate is in [0, 1] and equals its own definition
# ============================================================
test_that("sensitivity, specificity, PPV, NPV and accuracy match their cells", {
  need(P_VAL)
  m <- vmetrics()
  skip_if(!all(c("true_positives", "false_positives", "false_negatives",
                 "true_negatives") %in% names(m)), "cells missing")
  tp <- m[["true_positives"]]; fp <- m[["false_positives"]]
  fn <- m[["false_negatives"]]; tn <- m[["true_negatives"]]
  safe <- function(a, b) if (b == 0) NA_real_ else a / b
  expected <- c(sensitivity = safe(tp, tp + fn), specificity = safe(tn, tn + fp),
                ppv = safe(tp, tp + fp), npv = safe(tn, tn + fn),
                accuracy = safe(tp + tn, tp + fp + fn + tn))
  for (k in names(expected)) {
    if (!k %in% names(m) || is.na(expected[[k]])) next
    expect_true(m[[k]] >= 0 && m[[k]] <= 1, label = paste(k, "is not in [0, 1]"))
    # Rounded to three places in the artefact.
    expect_lt(abs(m[[k]] - expected[[k]]), 0.0015,
              label = sprintf("%s is reported as %.4f but its cells give %.4f",
                              k, m[[k]], expected[[k]]))
  }
})

# ============================================================
# BVA 23.3 - Jaccard similarity is bounded
# ============================================================
test_that("title_jaccard is a similarity in the unit interval", {
  need(P_FID)
  d <- readr::read_csv(P_FID, show_col_types = FALSE)
  j <- d$title_jaccard[!is.na(d$title_jaccard)]
  skip_if(length(j) == 0, "no jaccard values")
  expect_true(all(j >= 0 & j <= 1),
              label = sprintf("title_jaccard outside [0, 1]: range %.3f to %.3f",
                              min(j), max(j)))
  # A table where every title matched perfectly would mean the comparison is
  # comparing a string with itself.
  expect_lt(mean(j == 1), 0.95,
            label = sprintf("%.0f%% of jaccard values are exactly 1", 100 * mean(j == 1)))
})

# ============================================================
# SEMANTIC 23.4 - the changed flag follows the threshold it names
# ============================================================
test_that("title_changed is exactly title_jaccard below 0.7", {
  need(P_FID)
  d <- readr::read_csv(P_FID, show_col_types = FALSE)
  ev <- d |> filter(!is.na(title_jaccard))
  skip_if(nrow(ev) == 0, "nothing evaluable")
  is_true <- function(x) x %in% c(TRUE, "TRUE")
  # 09e_fidelity_checks.R:48 defines the flag as jaccard < 0.7. A flag that
  # drifted from its own threshold would misreport how many matched papers are
  # a different piece of work from the abstract.
  disagree <- sum(is_true(ev$title_changed) != (ev$title_jaccard < 0.7))
  expect_equal(disagree, 0L,
               label = sprintf("%d rows where title_changed does not equal jaccard < 0.7",
                               disagree))
  # And the boundary is exclusive: exactly 0.7 is NOT changed.
  at <- ev |> filter(abs(title_jaccard - 0.7) < 1e-9)
  if (nrow(at) > 0) {
    expect_true(all(!is_true(at$title_changed)),
                label = "a title at exactly 0.7 is flagged as changed")
  }
})

# ============================================================
# SEMANTIC 23.5 - the fidelity table covers the matches it should
# ============================================================
test_that("fidelity checks cover the published abstracts, and only those", {
  need(P_FID, P_FINAL)
  d <- readr::read_csv(P_FID, show_col_types = FALSE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  pub <- f$abstract_id[f$final_published %in% c(TRUE, "TRUE")]
  skip_if(length(pub) == 0, "nothing published")
  # A fidelity row for an abstract with no publication has nothing to compare
  # against; a published abstract with no fidelity row is an unchecked match.
  extra <- setdiff(d$abstract_id, f$abstract_id)
  expect_true(length(extra) == 0,
              label = sprintf("%d fidelity rows name abstracts outside the cohort",
                              length(extra)))
  unchecked <- setdiff(pub, d$abstract_id)
  expect_true(length(unchecked) == 0,
              label = sprintf(paste("%d of %d published abstracts have no fidelity",
                                    "check, so whether the matched paper is the",
                                    "same work was never assessed for them"),
                              length(unchecked), length(pub)))
})

# ============================================================
# SEMANTIC 23.6 - a perfect sensitivity needs a real negative class
# ============================================================
test_that("the gold standard contains both classes in usable numbers", {
  need(P_VAL)
  m <- vmetrics()
  skip_if(!all(c("true_positives", "false_negatives", "true_negatives",
                 "false_positives") %in% names(m)), "cells missing")
  pos <- m[["true_positives"]] + m[["false_negatives"]]
  neg <- m[["true_negatives"]] + m[["false_positives"]]
  # Sensitivity of 1.00 is only meaningful against a positive class large
  # enough to have failed. With 13 positives, the 95% interval on sensitivity
  # runs from roughly 0.75 to 1.00, which is worth stating rather than reading
  # 1.00 as certainty.
  expect_gt(pos, 5, label = sprintf("only %g positives in the gold standard", pos))
  expect_gt(neg, 5, label = sprintf("only %g negatives in the gold standard", neg))
})

# ============================================================
# SEMANTIC 23.7 - the validation sample is drawn from the cohort
# ============================================================
test_that("gold-standard abstracts outside the cohort do not enter the metrics", {
  need(P_GS, P_FINAL, P_VAL)
  g <- readr::read_csv(P_GS, show_col_types = FALSE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  m <- vmetrics()
  skip_if(!"abstract_id" %in% names(g), "no abstract_id in the gold standard")

  orphans <- setdiff(g$abstract_id, f$abstract_id)
  # AAGL2023_081 is in the gold standard and not in the cohort because it is a
  # Video presentation, excluded by 02_clean_abstracts.R:34. My first version
  # of this test simply forbade that, which is too strong: whoever drew the
  # sample can label whatever they like. What must hold is that a labelled
  # abstract the study does not analyse cannot contribute to a reported rate.
  #
  # n is 50 and n_classified is 49, so exactly one labelled abstract did not
  # reach the confusion matrix. This asserts the count reconciles.
  skip_if(!all(c("n", "n_classified") %in% names(m)), "metrics missing")
  expect_lte(m[["n_classified"]], m[["n"]] - length(orphans),
             label = sprintf(paste("%d gold-standard abstract(s) are outside the",
                                   "analysed cohort (%s) yet n_classified is %g of",
                                   "%g, so at least one of them was scored into",
                                   "the reported rates"),
                             length(orphans), paste(orphans, collapse = ", "),
                             m[["n_classified"]], m[["n"]]))
})

# ============================================================
# ADVERSARIAL 23.8 - the metrics are not from an older gold standard
# ============================================================
test_that("the reported n matches the gold standard file beside it", {
  need(P_VAL, P_GS)
  m <- vmetrics()
  g <- readr::read_csv(P_GS, show_col_types = FALSE)
  skip_if(!"n" %in% names(m), "no n metric")
  # These are cheap to regenerate and easy to leave behind. If the file has
  # grown since the metrics were computed, every quoted rate is from a smaller
  # sample than the one shipped.
  expect_equal(m[["n"]], nrow(g),
               label = sprintf("validation_metrics reports n=%g while gold_standard.csv holds %d rows",
                               m[["n"]], nrow(g)))
})

# ============================================================
# ADVERSARIAL 23.9 - fidelity flags are not degenerate
# ============================================================
test_that("the fidelity flags distinguish something", {
  need(P_FID)
  d <- readr::read_csv(P_FID, show_col_types = FALSE)
  is_true <- function(x) x %in% c(TRUE, "TRUE")
  t_ev <- d$title_changed[!is.na(d$title_changed)]
  skip_if(length(t_ev) == 0, "no evaluable titles")
  p <- mean(is_true(t_ev))
  # A flag that is constant gives the same answer as no flag. A title-change
  # rate of 0 would mean every matched paper carries the abstract's exact
  # title, which would itself suggest the comparison is trivial.
  expect_true(p > 0 && p < 1,
              label = sprintf("title_changed is constant at %s over %d evaluable rows",
                              as.character(p == 1), length(t_ev)))
})

# ============================================================
# ADVERSARIAL 23.10 - no duplicated abstract in either artefact
# ============================================================
test_that("the validation and fidelity tables hold one row per abstract", {
  offenders <- character(0)
  for (p in c(P_FID, P_GS)) {
    if (!file.exists(p)) next
    d <- readr::read_csv(p, show_col_types = FALSE)
    if (!"abstract_id" %in% names(d)) next
    if (anyDuplicated(d$abstract_id) != 0) {
      offenders <- c(offenders, sprintf("%s (%d duplicates)", basename(p),
                                        sum(duplicated(d$abstract_id))))
    }
  }
  # A duplicated abstract in the gold standard double-counts one label in every
  # cell of the confusion matrix.
  expect_true(length(offenders) == 0,
              label = paste("duplicated abstract_id in:", paste(offenders, collapse = ", ")))
})
