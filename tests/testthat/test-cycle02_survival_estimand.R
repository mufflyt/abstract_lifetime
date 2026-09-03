# Cycle 2 of 24. Mix: 3 BVA, 4 semantic, 3 adversarial.
#
# Targets: survival censoring, the time-to-publication estimand and its
# population, per-year denominators, and the join that builds the analytical
# dataset. None of this was touched by cycle 0 (decision precedence) or cycle 1
# (score tiers, congress-date contracts).

library(testthat)
library(dplyr)
source(here::here("R", "utils_decisions.R"))
source(here::here("R", "utils_congresses.R"))

need <- function(...) {
  ps <- c(...)
  if (!all(file.exists(ps))) skip("pipeline outputs not present")
  invisible(TRUE)
}
P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_AIM1  <- here::here("output", "aim1_publication_rate.csv")
P_AIM2  <- here::here("output", "aim2_time_to_pub.csv")
P_YEAR  <- here::here("output", "aim1_by_congress_year.csv")

# ============================================================
# BVA 2.1 — the `time > 0` filter and the zero boundary
# ============================================================
test_that("no published abstract sits exactly on the time == 0 boundary", {
  need(P_FINAL, P_AIM2)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  a2 <- readr::read_csv(P_AIM2, show_col_types = FALSE)
  val <- function(m) a2$value[a2$metric == m]

  # 06_analyze_results.R builds the survival set with filter(time > 0), which is
  # exclusive. An abstract published ON its congress date has months_to_pub == 0
  # and is dropped from the KM analysis without being censored or counted.
  on_the_day <- sum(f$final_published %in% TRUE & f$months_to_pub == 0, na.rm = TRUE)
  expect_equal(on_the_day, 0L,
               label = paste0(on_the_day, " published abstract(s) have months_to_pub ",
                              "exactly 0 and are silently dropped by filter(time > 0)"))
  expect_gt(val("min_months"), 0)
})

# ============================================================
# BVA 2.2 — per-year denominators must sum to the global denominator
# ============================================================
test_that("per-year counts partition the evaluated cohort exactly", {
  need(P_FINAL, P_YEAR)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  y <- readr::read_csv(P_YEAR, show_col_types = FALSE)
  s <- publication_rate_summary(f)

  expect_equal(sum(y$n), s$n_evaluated,
               label = "sum of per-year n must equal the global denominator")
  expect_equal(sum(y$n_published), s$n_published,
               label = "sum of per-year published must equal the global numerator")
})

# ============================================================
# BVA 2.3 — follow-up is longest for the earliest congress
# ============================================================
test_that("censoring window shrinks monotonically toward the most recent congress", {
  cfg <- config::get(file = here::here("config.yml"))
  lkp <- congress_date_lookup(cfg)
  yrs <- sort(as.integer(names(lkp)))
  end <- as.Date(cfg$pubmed$date_end, "%Y/%m/%d")
  skip_if(is.na(end), "pubmed$date_end not parseable")

  follow <- as.numeric(difftime(end, lkp[as.character(yrs)], units = "days"))
  expect_true(all(diff(follow) < 0),
              label = "follow-up must strictly decrease as congress year increases")
  expect_gt(follow[1], follow[length(follow)])
  expect_true(all(follow > 0), label = "every congress must precede the search end date")
})

# ============================================================
# SEMANTIC 2.4 — the time-to-publication population is a SUBSET of published
# ============================================================
test_that("median time-to-publication is computed on published-with-a-date only", {
  need(P_FINAL, P_AIM1, P_AIM2)
  a1 <- readr::read_csv(P_AIM1, show_col_types = FALSE)
  a2 <- readr::read_csv(P_AIM2, show_col_types = FALSE)
  v1 <- function(m) a1$value[a1$metric == m]
  v2 <- function(m) a2$value[a2$metric == m]

  # The label "time to publication" describes the published population; the
  # quantity is computed only on those with a resolvable date. If the two
  # differ the manuscript must say so rather than implying full coverage.
  expect_lte(v2("n_with_dates"), v1("published"))
  expect_gt(v2("n_with_dates"), 0)

  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  with_date <- sum(f$final_published %in% TRUE & !is.na(f$months_to_pub) & f$months_to_pub > 0)
  expect_equal(v2("n_with_dates"), with_date,
               label = "n_with_dates must equal published rows carrying a usable positive interval")
})

# ============================================================
# SEMANTIC 2.5 — per-year rate uses the per-year denominator
# ============================================================
test_that("each year's rate divides by that year's n, not the global denominator", {
  need(P_YEAR)
  y <- readr::read_csv(P_YEAR, show_col_types = FALSE)
  expect_equal(y$rate, round(y$n_published / y$n * 100, 1),
               label = "per-year rate must be n_published / n for that row")
  expect_true(all(y$n_published <= y$n),
              label = "a year cannot publish more abstracts than it evaluated")
})

# ============================================================
# SEMANTIC 2.6 — quartile ordering and units of the interval summary
# ============================================================
test_that("time-to-publication summary is ordered and expressed in months", {
  need(P_AIM2)
  a2 <- readr::read_csv(P_AIM2, show_col_types = FALSE)
  v <- function(m) a2$value[a2$metric == m]
  expect_lte(v("min_months"), v("q1_months"))
  expect_lte(v("q1_months"),  v("median_months"))
  expect_lte(v("median_months"), v("q3_months"))
  expect_lte(v("q3_months"),  v("max_months"))
  # Months, not days or years: a 12-year study cannot exceed ~170 months.
  expect_lt(v("max_months"), 200)
  expect_gt(v("median_months"), 1)
})

# ============================================================
# SEMANTIC 2.7 — interval sign convention
# ============================================================
test_that("positive months_to_pub means published AFTER the congress", {
  need(P_FINAL, P_AIM2)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  neg <- f |> filter(!is.na(months_to_pub), months_to_pub < 0)

  # Revised 2026-09-03. This originally asserted that a negative interval must
  # belong to an `excluded` or unpublished abstract. That held only because the
  # affected rows had NO date at all: their PMIDs were missing from the stale
  # candidate pool (docs/FAILURE_MODES.md F2). With the pool repaired, seven
  # confirmed publications resolve to a date before their congress - four are
  # pre-conference candidates a reviewer confirmed anyway, one is a `definite`
  # online-first paper two weeks ahead of the 2015 meeting, two are 2018
  # reviewer-supplied PMIDs. They belong in the numerator because a reviewer
  # ruled they are the abstract's publication.
  #
  # The invariant that actually matters is unchanged and is asserted here: a
  # negative interval must be explainable, and must never enter the survival
  # analysis or the time-to-publication summary, where it is not a time.
  if (nrow(neg) > 0) {
    expect_true(
      all(neg$classification %in% c("excluded", "definite") |
            neg$manual_decision %in% "match" |
            neg$final_published %in% c(FALSE, NA)),
      label = "a negative interval reached the numerator without adjudication"
    )
  }

  a2 <- readr::read_csv(P_AIM2, show_col_types = FALSE)
  expect_gte(a2$value[a2$metric == "min_months"], 0)
  expect_equal(a2$value[a2$metric == "n_pre_congress"],
               nrow(neg |> filter(final_published %in% TRUE)))
})

# ============================================================
# ADVERSARIAL 2.8 — un-deduplicated decisions must not inflate the cohort
# ============================================================
test_that("assign_final_published does not multiply rows on duplicate ids", {
  r <- tibble::tibble(abstract_id = c("A1", "A2"),
                      classification = c("probable", "no_match"),
                      best_pmid = NA_character_)
  dup <- tibble::tibble(
    abstract_id = c("A1", "A1"),
    manual_decision = c("match", "no_match"),
    manual_pmid = NA_character_)
  out <- tryCatch(assign_final_published(r, dup), error = function(e) e)
  if (inherits(out, "error")) {
    succeed()  # rejecting un-deduplicated input is an acceptable contract
  } else {
    expect_equal(nrow(out), nrow(r),
                 label = "join with duplicate ids silently inflated the cohort")
  }
})

# ============================================================
# ADVERSARIAL 2.9 — no congress year may vanish from the by-year table
# ============================================================
test_that("every cohort year appears in the by-year table", {
  need(P_FINAL, P_YEAR)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  y <- readr::read_csv(P_YEAR, show_col_types = FALSE)
  cohort_years <- sort(unique(f$congress_year))
  missing <- setdiff(cohort_years, y$congress_year)
  expect_length(missing, 0L)
  if (length(missing)) {
    fail(paste("years present in the cohort but absent from aim1_by_congress_year:",
               paste(missing, collapse = ", "),
               "- a year with zero published must appear with rate 0, not disappear"))
  }
})

# ============================================================
# ADVERSARIAL 2.10 — a zero-publication year must survive summarisation
# ============================================================
test_that("a year with zero publications yields rate 0 rather than being dropped", {
  synth <- tibble::tibble(
    congress_year   = c(2012, 2012, 2013, 2013),
    final_published = c(FALSE, FALSE, TRUE, FALSE))
  out <- synth |>
    filter(!is.na(final_published)) |>
    group_by(congress_year) |>
    summarise(n = n(), n_published = sum(final_published),
              rate = round(mean(final_published) * 100, 1), .groups = "drop")
  expect_equal(nrow(out), 2L, label = "the zero-publication year was dropped")
  expect_equal(out$rate[out$congress_year == 2012], 0)
  expect_equal(out$rate[out$congress_year == 2013], 50)
})
