# Cycle 4 of 24. Mix: 4 BVA, 3 semantic, 3 adversarial.
#
# Targets: validation metrics, sensitivity-analysis scenarios, interrater
# agreement, search-strategy yield, and publication-bias direction. Cycles 0-3
# covered decision precedence, score tiers, congress dates, survival, and the
# regression tables; none of these five artifacts had any test.

library(testthat)
library(dplyr)
source(here::here("R", "utils_decisions.R"))

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_VAL   <- here::here("output", "validation_metrics.csv")
P_SENS  <- here::here("output", "sensitivity_analyses.csv")
P_IRR   <- here::here("output", "interrater_agreement.csv")
P_AIM4  <- here::here("output", "aim4_strategy_performance.csv")
P_AIM5  <- here::here("output", "aim5_publication_bias.csv")
P_DEC   <- here::here("output", "manual_review_decisions.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("pipeline outputs not present")
kv <- function(p) { d <- readr::read_csv(p, show_col_types = FALSE)
                    function(m) d$value[d$metric == m] }

# ============================================================
# BVA 4.1 — the confusion matrix closes and its rates are proportions
# ============================================================
test_that("validation confusion matrix sums to n and rates lie in [0,1]", {
  need(P_VAL)
  v <- kv(P_VAL)
  # na.rm drops rows with NA truth/predicted from all four cells, so the matrix
  # rests on n_classified. n_classified must be exported and must not exceed n.
  expect_true("n_classified" %in% readr::read_csv(P_VAL, show_col_types = FALSE)$metric,
              label = "validation must expose the population its cells were measured on")
  expect_equal(v("true_positives") + v("false_positives") +
               v("false_negatives") + v("true_negatives"), v("n_classified"),
               label = "confusion cells must partition n_classified")
  expect_lte(v("n_classified"), v("n"))
  expect_equal(v("accuracy"),
               round((v("true_positives") + v("true_negatives")) / v("n_classified"), 3),
               label = "accuracy must divide by the rows it was measured on")
  for (m in c("sensitivity", "specificity")) {
    x <- v(m)
    if (length(x) == 1 && !is.na(x)) {
      expect_gte(x, 0); expect_lte(x, 1)
    }
  }
  # Sensitivity of exactly 1 is the upper boundary and must coincide with zero
  # false negatives, not merely round to it.
  if (isTRUE(all.equal(v("sensitivity"), 1))) expect_equal(v("false_negatives"), 0)
})

# ============================================================
# BVA 4.2 — every sensitivity scenario's rate matches its own counts
# ============================================================
test_that("each sensitivity scenario recomputes from its own numerator and denominator", {
  need(P_SENS)
  s <- readr::read_csv(P_SENS, show_col_types = FALSE)
  expect_equal(s$rate, round(s$n_published / s$n * 100, 1),
               label = "a scenario rate does not match its own n_published / n")
  expect_true(all(s$n_published <= s$n))
  expect_true(all(s$rate >= 0 & s$rate <= 100))
})

# ============================================================
# BVA 4.3 — agreement is a percentage on a real population
# ============================================================
test_that("interrater agreement is bounded and its population is plausible", {
  need(P_IRR, P_FINAL)
  v <- kv(P_IRR)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  n_multi <- v("n_multi_reviewed")
  expect_gte(n_multi, 0)
  expect_lte(n_multi, nrow(f),
             label = "more multi-reviewed abstracts than exist in the cohort")
  pa <- v("pct_agreement")
  if (length(pa) == 1 && !is.na(pa)) { expect_gte(pa, 0); expect_lte(pa, 100) }
})

# ============================================================
# BVA 4.4 — search-strategy yields are internally consistent percentages
# ============================================================
test_that("strategy yield percentages recompute from their own counts", {
  need(P_AIM4)
  a <- readr::read_csv(P_AIM4, show_col_types = FALSE)
  expect_equal(a$yield_pct, round(a$n_with_hits / a$n_searched * 100, 1),
               label = "yield_pct does not equal n_with_hits / n_searched")
  expect_true(all(a$n_with_hits <= a$n_searched))
  ok <- is.na(a$pct_found) | (a$pct_found >= 0 & a$pct_found <= 100)
  expect_true(all(ok))
})

# ============================================================
# SEMANTIC 4.5 — scenarios that do not restrict follow-up share one denominator
# ============================================================
test_that("sensitivity scenarios use denominators consistent with their definition", {
  need(P_SENS, P_FINAL)
  s <- readr::read_csv(P_SENS, show_col_types = FALSE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  summ <- publication_rate_summary(f)

  # An earlier draft of this test demanded one denominator across all
  # match-definition scenarios. That premise was wrong: "Definite only" is
  # decidable for every abstract because classification is always present,
  # whereas "Definite + reviewer-confirmed" needs a reviewer and so is only
  # decidable for the evaluated cohort. The denominators differ for a principled
  # reason. What matters is that each scenario exposes its own denominator and
  # that none silently exceeds the cohort.
  expect_true(all(s$n <= nrow(f)),
              label = "a scenario claims a denominator larger than the cohort")
  conf <- s |> filter(grepl("reviewer-confirmed", scenario))
  if (nrow(conf) == 1) {
    expect_equal(conf$n, summ$n_evaluated,
                 label = "the reviewer-confirmed scenario must use the evaluated denominator")
    expect_equal(conf$n_published, summ$n_published)
  }
})

# ============================================================
# SEMANTIC 4.6 — publication-bias odds ratio points the way the rates do
# ============================================================
test_that("publication-bias OR direction agrees with the underlying rates", {
  need(P_AIM5)
  b <- readr::read_csv(P_AIM5, show_col_types = FALSE)
  or_row <- b |> filter(grepl("OR$", result_positivity))
  skip_if(nrow(or_row) != 1, "no OR row")
  pos <- b$rate[b$result_positivity == "positive"]
  neg <- b$rate[b$result_positivity == "negative"]
  skip_if(length(pos) != 1 || length(neg) != 1, "missing rate rows")
  # OR > 1 must mean positive results publish MORE often than negative ones.
  expect_equal(sign(or_row$rate - 1), sign(pos - neg),
               label = sprintf("OR %.3f but positive rate %.1f%% vs negative %.1f%%",
                               or_row$rate, pos, neg))
})

# ============================================================
# SEMANTIC 4.7 — validation is a sample of the human-reviewed population
# ============================================================
test_that("the validation sample is drawn from human-reviewed abstracts", {
  need(P_VAL, P_DEC)
  v <- kv(P_VAL)
  d <- readr::read_csv(P_DEC, show_col_types = FALSE)
  n_human <- length(unique(d$abstract_id[!is.na(d$reviewer) & d$reviewer != "AUTO"]))
  expect_lte(v("n"), n_human,
             label = "validation sample larger than the human-reviewed population it claims to validate")
  expect_gt(v("n"), 0)
})

# ============================================================
# ADVERSARIAL 4.8 — agreement without kappa is an incomplete claim
# ============================================================
test_that("kappa is reported whenever agreement is computed on a real sample", {
  need(P_IRR)
  v <- kv(P_IRR)
  n_multi <- v("n_multi_reviewed")
  skip_if(length(n_multi) != 1 || n_multi == 0, "no multi-reviewed abstracts")
  k <- v("cohens_kappa")
  # Raw agreement is inflated when one category dominates, which is why the
  # Cochrane methodology asks for kappa. Reporting 98.1% agreement with kappa
  # absent overstates reliability.
  expect_true(length(k) == 1 && !is.na(k),
              label = paste0("pct_agreement reported on ", n_multi,
                             " abstracts but cohens_kappa is NA"))
})

# ============================================================
# ADVERSARIAL 4.9 — a search strategy returning almost nothing is broken
# ============================================================
# PRESERVED FAILING TEST — tracks an un-executed fix, see tests/loop/LEDGER.md.
# Technical appendix A12.4 records that stopword removal broke the title phrase
# search. The fix was written 2026-04-28; the PubMed candidate pool was last
# retrieved 2026-04-19 and has not been rebuilt since. This test goes green when
# the re-run happens and is the cheapest available signal that it has.
test_that("no search strategy is silently contributing nothing", {
  need(P_AIM4)
  a <- readr::read_csv(P_AIM4, show_col_types = FALSE) |> filter(n_searched > 100)
  worst <- a[which.min(a$yield_pct), ]
  # Technical appendix A12.4 records that stopword removal broke the title
  # phrase search. If a strategy searched over a thousand abstracts and hit
  # almost none, the documented fix has not reached the data.
  expect_gt(worst$yield_pct, 1,
            label = paste0("strategy '", worst$strategy, "' searched ",
                           worst$n_searched, " and hit ", worst$n_with_hits,
                           " (", worst$yield_pct, "%). A12.4 predicts exactly this."))
})

# ============================================================
# ADVERSARIAL 4.10 — longer follow-up windows cannot grow the cohort
# ============================================================
test_that("follow-up windows shrink the cohort monotonically", {
  need(P_SENS)
  s <- readr::read_csv(P_SENS, show_col_types = FALSE) |>
    filter(grepl("within", scenario, ignore.case = TRUE)) |>
    mutate(months = as.numeric(gsub("\\D+", "", scenario))) |>
    arrange(months)
  skip_if(nrow(s) < 2, "fewer than two window scenarios")
  expect_true(all(diff(s$n) <= 0),
              label = "a longer follow-up window reported a LARGER eligible cohort")
  expect_true(all(s$n_published <= s$n))
})
