# Cycle 12 of 24. Mix: 3 BVA, 3 semantic, 4 adversarial.
#
# Cycle 11 established that one model covariate, n_authors, is censored at a
# display cap while being reported as a significant predictor. This cycle asks
# the same question of every OTHER term in aim3: is the variable behind the
# coefficient what the coefficient claims it is.
#
# Not duplicated here: the concurrent suite's model-stability tests cover the
# variable screen and leave-one-congress-out refits; cycle 3 covers interval
# width and the missing N.

library(testthat)
library(dplyr)

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_AIM3  <- here::here("output", "aim3_logistic_regression.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")
BINARY <- c("is_rct", "is_academic", "is_us_based", "is_multicenter", "has_funding")

evaluated <- function() {
  readr::read_csv(P_FINAL, show_col_types = FALSE) |> filter(!is.na(final_published))
}

# ============================================================
# BVA 12.1 - sample size is a positive count safe to log
# ============================================================
test_that("sample_size is a positive finite count wherever present", {
  need(P_FINAL)
  ev <- evaluated()
  skip_if(!"sample_size" %in% names(ev), "sample_size absent")
  x <- ev$sample_size[!is.na(ev$sample_size)]
  skip_if(length(x) == 0, "no sample sizes")
  expect_true(all(x > 0),
              label = paste(sum(x <= 0), "abstracts have a non-positive sample_size;",
                            "log() of those is -Inf or NaN and would drop them from",
                            "the model without appearing as missing"))
  expect_true(all(is.finite(x)))
  expect_true(all(x == floor(x)),
              label = "a fractional sample size means the parser captured the wrong token")
})

# ============================================================
# BVA 12.2 - a binary covariate needs both cells populated to be estimable
# ============================================================
test_that("every binary covariate has enough minority events to estimate", {
  need(P_FINAL)
  ev <- evaluated()
  # Scope to covariates the model actually fits. The variable screen drops thin
  # terms (has_funding is no longer in aim3), and flagging a covariate that is
  # not estimated would be noise rather than a finding.
  aim3 <- if (file.exists(P_AIM3)) readr::read_csv(P_AIM3, show_col_types = FALSE)$term else character(0)
  in_model <- function(v) any(startsWith(aim3, v))
  thin <- character(0)
  for (v in intersect(BINARY, names(ev))) {
    if (length(aim3) && !in_model(v)) next
    x <- ev[[v]]; x <- x[!is.na(x)]
    if (!length(x)) next
    minority <- min(sum(x %in% c(TRUE, "TRUE")), sum(x %in% c(FALSE, "FALSE")))
    # Ten in the minority cell is the conventional floor for one binary term.
    # Below it the coefficient is driven by a handful of rows and its interval
    # spans orders of magnitude, which is how has_funding behaves (cycle 3).
    if (minority < 10) {
      thin <- c(thin, sprintf("%s (minority cell n=%d)", v, minority))
    }
  }
  expect_true(length(thin) == 0,
              label = paste("covariates too thin to estimate:",
                            paste(thin, collapse = ", ")))
})

# ============================================================
# BVA 12.3 - the gender vocabulary is closed and well covered
# ============================================================
test_that("gender_unified draws from a closed vocabulary with usable coverage", {
  need(P_FINAL)
  ev <- evaluated()
  skip_if(!"gender_unified" %in% names(ev), "gender_unified absent")
  vals <- unique(ev$gender_unified[!is.na(ev$gender_unified)])
  expect_true(all(vals %in% c("female", "male", "unknown")),
              label = paste("unexpected gender value(s):",
                            paste(setdiff(vals, c("female", "male", "unknown")),
                                  collapse = ", ")))
  # A covariate missing on most rows silently shrinks the model cohort through
  # complete-case deletion rather than appearing as a limitation.
  expect_lt(mean(is.na(ev$gender_unified)), 0.5,
            label = sprintf("gender_unified is %.1f%% missing",
                            100 * mean(is.na(ev$gender_unified))))
})

# ============================================================
# SEMANTIC 12.4 - the reported model must be reproducible from the export
# ============================================================
test_that("every aim3 term maps to a column in the exported dataset", {
  need(P_FINAL, P_AIM3)
  ev <- evaluated()
  m <- readr::read_csv(P_AIM3, show_col_types = FALSE) |> filter(term != "(Intercept)")
  base_of <- function(term) {
    hits <- names(ev)[startsWith(term, names(ev))]
    if (length(hits) == 0) NA_character_ else hits[which.max(nchar(hits))]
  }
  unmapped <- m$term[is.na(vapply(m$term, base_of, character(1)))]
  # A term with no column behind it cannot be checked, replicated or corrected
  # by anyone holding the published dataset.
  expect_true(length(unmapped) == 0,
              label = paste("aim3 reports term(s) with no column in the exported",
                            "dataset:", paste(unmapped, collapse = ", "),
                            "- the model cannot be reproduced from what is published"))
})

# ============================================================
# SEMANTIC 12.5 - the US indicator agrees with the country it is derived from
# ============================================================
test_that("is_us_based agrees with first_author_country", {
  need(P_FINAL)
  # Verify the fix where it landed first. parse_affiliation() in
  # 09_enrich_authors.R took the last comma-delimited token of the affiliation as
  # the country, which yields a US state for domestic addresses. Fixed at the
  # root and re-derived through parse_country() in 09c. The final analytical
  # dataset is written by 06_analyze_results.R and will not carry the correction
  # until that stage next runs, so the assertion below tracks that staleness.
  awm <- here::here("output", "abstracts_with_matches.csv")
  if (file.exists(awm)) {
    a <- readr::read_csv(awm, show_col_types = FALSE)
    if ("first_author_country" %in% names(a)) {
      v <- a$first_author_country[!is.na(a$first_author_country)]
      expect_false(any(grepl("^(Arizona|Illinois|Massachusetts|California|Texas|Florida|New York|Ohio|Michigan|Colorado|Pennsylvania)\\.?$",
                             v, ignore.case = TRUE)),
                   label = "a US state is still being written into the country field")
    }
  }
  ev <- evaluated()
  skip_if(!all(c("is_us_based", "first_author_country") %in% names(ev)), "columns absent")
  both <- ev |> filter(!is.na(is_us_based), !is.na(first_author_country))
  skip_if(nrow(both) == 0, "no rows with both")
  us_like <- grepl("^(US|USA|United States)$", both$first_author_country, ignore.case = TRUE)
  disagree <- sum((both$is_us_based %in% c(TRUE, "TRUE")) != us_like)
  states <- sum(grepl("^(Arizona|Illinois|Massachusetts|California|Texas|Florida|New York|Ohio|Michigan|Colorado)\\.?$",
                      both$first_author_country, ignore.case = TRUE))
  expect_equal(disagree, 0L,
               label = paste0(disagree, " of ", nrow(both),
                              " rows disagree between is_us_based and ",
                              "first_author_country; ", states,
                              " of those carry a US STATE in the country field"))
})

# ============================================================
# SEMANTIC 12.6 - the RCT flag agrees with the design classification
# ============================================================
test_that("is_rct agrees with study_design", {
  need(P_FINAL)
  ev <- evaluated()
  skip_if(!all(c("is_rct", "study_design") %in% names(ev)), "columns absent")
  flagged <- ev |> filter(is_rct %in% c(TRUE, "TRUE"))
  skip_if(nrow(flagged) == 0, "no RCTs flagged")
  # Both describe the same property of the same abstract. If they disagree, one
  # of them is wrong and the model is using whichever happened to be selected.
  expect_true(all(grepl("rct|randomi", flagged$study_design, ignore.case = TRUE) |
                  is.na(flagged$study_design)),
              label = paste(sum(!grepl("rct|randomi", flagged$study_design,
                                       ignore.case = TRUE) & !is.na(flagged$study_design)),
                            "abstracts are flagged is_rct but classified as another design"))
})

# ============================================================
# ADVERSARIAL 12.7 - covariate missingness must not track congress year
# ============================================================
test_that("model covariate missingness does not vary sharply by congress year", {
  need(P_FINAL)
  ev <- evaluated()
  vars <- intersect(c("sample_size", "gender_unified", "is_academic", "is_us_based"),
                    names(ev))
  skip_if(length(vars) == 0, "no covariates")
  offenders <- character(0)
  for (v in vars) {
    by_yr <- ev |> group_by(congress_year) |>
      summarise(miss = mean(is.na(.data[[v]])), .groups = "drop")
    spread <- max(by_yr$miss) - min(by_yr$miss)
    # Complete-case deletion removes rows with any missing covariate. If
    # missingness concentrates in particular congresses, those years are
    # under-represented in the model and the year-over-year comparison the
    # manuscript reports is confounded by data availability.
    if (spread > 0.5) {
      offenders <- c(offenders, sprintf("%s (%.0f%% spread across years)", v, 100 * spread))
    }
  }
  expect_true(length(offenders) == 0,
              label = paste("missingness concentrates by congress year:",
                            paste(offenders, collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 12.8 - implausible sample-size magnitudes
# ============================================================
test_that("sample sizes are plausible for the studies they describe", {
  need(P_FINAL)
  ev <- evaluated()
  skip_if(!"sample_size" %in% names(ev), "sample_size absent")
  x <- ev$sample_size[!is.na(ev$sample_size)]
  skip_if(length(x) == 0, "no sample sizes")
  # Large values are legitimate for national database studies, so this is not a
  # ceiling test. It catches a parser that captured a year, a dollar figure or a
  # PMID instead of an enrolment count.
  huge <- x[x > 5e6]
  expect_true(length(huge) == 0,
              label = paste("sample_size values above 5 million:",
                            paste(head(huge, 3), collapse = ", ")))
  expect_lt(mean(x >= 1900 & x <= 2030) , 0.10,
            label = "an implausible share of sample sizes fall in the range of a year")
})

# ============================================================
# ADVERSARIAL 12.9 - complete-case attrition is attributable
# ============================================================
test_that("model attrition is explained by named covariates, not an unknown column", {
  need(P_FINAL, P_AIM3)
  ev <- evaluated()
  m <- readr::read_csv(P_AIM3, show_col_types = FALSE)
  skip_if(!"n_obs" %in% names(m), "n_obs absent from aim3")
  n_model <- unique(m$n_obs)[1]
  vars <- unique(na.omit(vapply(m$term[m$term != "(Intercept)"], function(term) {
    hits <- names(ev)[startsWith(term, names(ev))]
    if (length(hits) == 0) NA_character_ else hits[which.max(nchar(hits))]
  }, character(1))))
  skip_if(length(vars) == 0, "no mappable terms")
  complete <- sum(stats::complete.cases(ev[, vars, drop = FALSE]))
  # The fitted N must be reachable from the exported columns. If it is smaller,
  # something outside the reported terms is dropping rows.
  expect_lte(n_model, complete,
             label = sprintf(paste("model fitted %d rows but only %d are complete on the",
                                   "reported terms; an unreported column is dropping rows"),
                             n_model, complete))
})

# ============================================================
# ADVERSARIAL 12.10 - no covariate is effectively constant
# ============================================================
test_that("no model covariate is constant across the evaluated cohort", {
  need(P_FINAL, P_AIM3)
  ev <- evaluated()
  m <- readr::read_csv(P_AIM3, show_col_types = FALSE) |> filter(term != "(Intercept)")
  constant <- character(0)
  for (term in m$term) {
    hits <- names(ev)[startsWith(term, names(ev))]
    if (!length(hits)) next
    v <- hits[which.max(nchar(hits))]
    x <- ev[[v]][!is.na(ev[[v]])]
    if (length(x) && length(unique(x)) < 2) constant <- c(constant, v)
  }
  expect_true(length(constant) == 0,
              label = paste("constant covariate(s) carry a coefficient anyway:",
                            paste(constant, collapse = ", ")))
})
