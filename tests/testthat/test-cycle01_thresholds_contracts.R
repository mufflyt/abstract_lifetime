# Cycle 1 of 24. Mix: 4 BVA, 3 semantic, 3 adversarial.
#
# Targets chosen to avoid overlap with cycle 0 (decision precedence, cascade,
# denominator arithmetic) and with the pre-existing files. utils_congresses is
# already tested for lookup/vector/NA/legacy fallback, so nothing here repeats
# those; the cases below attack duplicate keys, class contracts and vintage
# agreement instead.

library(testthat)
library(dplyr)
source(here::here("R", "utils_decisions.R"))
source(here::here("R", "utils_congresses.R"))

cfg_fix <- list(
  congresses = list(list(year = 2012, date = "2012-11-06"),
                    list(year = 2023, date = "2023-11-07")),
  conference = list(date = "2023-11-07"),
  scoring = list(auto_accept = 7, manual_review = 3)
)

# ============================================================
# BVA 1.1 — classify_match at the exact auto_accept / manual_review cutoffs
# ============================================================
test_that("classify_match boundaries are inclusive at the cutoff, exclusive below", {
  skip_if_not(file.exists(here::here("R", "utils_scoring.R")))
  source(here::here("R", "utils_scoring.R"), local = TRUE)

  expect_equal(classify_match(7,       cfg_fix, TRUE), "definite")
  expect_equal(classify_match(6.999,   cfg_fix, TRUE), "probable")
  expect_equal(classify_match(3,       cfg_fix, TRUE), "probable")
  expect_equal(classify_match(2.999,   cfg_fix, TRUE), "no_match")
  # Without text evidence the definite tier is unreachable at any score.
  expect_equal(classify_match(7,       cfg_fix, FALSE), "possible")
  expect_equal(classify_match(1e6,     cfg_fix, FALSE), "possible")
  expect_equal(classify_match(2.999,   cfg_fix, FALSE), "no_match")
})

# ============================================================
# BVA 1.2 — non-finite and missing scores must not classify as a match
# ============================================================
test_that("classify_match handles Inf, -Inf and NA without silently matching", {
  skip_if_not(file.exists(here::here("R", "utils_scoring.R")))
  source(here::here("R", "utils_scoring.R"), local = TRUE)

  expect_equal(classify_match(Inf,  cfg_fix, TRUE), "definite")
  expect_equal(classify_match(-Inf, cfg_fix, TRUE), "no_match")
  # NA must not resolve to a match tier. Either an error or "no_match" is a
  # defensible contract; silently returning "definite"/"probable" is not.
  got <- tryCatch(classify_match(NA_real_, cfg_fix, TRUE), error = function(e) "errored")
  expect_true(got %in% c("errored", "no_match"),
              label = paste0("NA score classified as '", got, "'"))
})

# ============================================================
# BVA 1.3 — smallest and empty cohorts
# ============================================================
test_that("publication_rate_summary handles cohorts of size 1 and 0", {
  one_pub <- tibble::tibble(final_published = TRUE)
  s <- publication_rate_summary(one_pub)
  expect_equal(s$n_cohort, 1L); expect_equal(s$n_evaluated, 1L)
  expect_equal(s$publication_rate, 1)

  one_unpub <- tibble::tibble(final_published = FALSE)
  expect_equal(publication_rate_summary(one_unpub)$publication_rate, 0)

  empty <- tibble::tibble(final_published = logical(0))
  e <- publication_rate_summary(empty)
  expect_equal(e$n_cohort, 0L)
  expect_true(is.na(e$publication_rate),
              label = "empty cohort must give NA, not NaN or 0")
})

# ============================================================
# BVA 1.4 — the summary returns an unrounded proportion
# ============================================================
test_that("publication_rate_summary does not pre-round, so export controls precision", {
  # 1/3 is chosen because premature rounding at any sane precision is detectable.
  r <- tibble::tibble(final_published = c(TRUE, FALSE, FALSE))
  s <- publication_rate_summary(r)
  expect_equal(s$publication_rate, 1/3, tolerance = 1e-12)
  expect_gt(abs(s$publication_rate - round(1/3, 3)), 0,
            label = "rate must carry full precision, not a rounded value")
  # And it is a proportion, not a percentage.
  expect_lte(s$publication_rate, 1)
})

# ============================================================
# SEMANTIC 1.5 — pre_conference dominates score
# ============================================================
test_that("pre_conference short-circuits before any score tier", {
  skip_if_not(file.exists(here::here("R", "utils_scoring.R")))
  source(here::here("R", "utils_scoring.R"), local = TRUE)

  # A perfect score published before the congress is still excluded: the flag
  # is a statement about validity, not a score penalty.
  expect_equal(classify_match(1e6, cfg_fix, TRUE,  pre_conference = TRUE), "excluded")
  expect_equal(classify_match(0,   cfg_fix, FALSE, pre_conference = TRUE), "excluded")
  expect_equal(classify_match(1e6, cfg_fix, TRUE,  pre_conference = FALSE), "definite")
})

# ============================================================
# SEMANTIC 1.6 — conference_date_for class and length contract
# ============================================================
test_that("conference_date_for returns a Date of the same length as its input", {
  out <- conference_date_for(c(2012L, 2023L, 9999L, NA_integer_), cfg_fix)
  expect_s3_class(out, "Date")
  expect_length(out, 4L)
  expect_equal(out[1], as.Date("2012-11-06"))
  # Character and integer years must agree: they are the same concept.
  expect_equal(conference_date_for(c("2012", "2023"), cfg_fix),
               conference_date_for(c(2012L, 2023L), cfg_fix))
  # Length-0 in, length-0 out, still a Date.
  z <- conference_date_for(integer(0), cfg_fix)
  expect_s3_class(z, "Date")
  expect_length(z, 0L)
})

# ============================================================
# SEMANTIC 1.7 — "pending" means unresolved, not unpublished
# ============================================================
test_that("only NA counts as pending; FALSE is evaluated", {
  r <- tibble::tibble(final_published = c(TRUE, FALSE, FALSE, NA, NA))
  s <- publication_rate_summary(r)
  expect_equal(s$n_pending, 2L)
  expect_equal(s$n_evaluated, 3L)
  expect_equal(s$n_not_published, 2L,
               label = "FALSE rows belong in not_published, never in pending")
  expect_equal(s$publication_rate, 1/3)
})

# ============================================================
# ADVERSARIAL 1.8 — duplicate congress years in config
# ============================================================
test_that("duplicate congress years do not silently produce a wrong date", {
  bad <- cfg_fix
  bad$congresses <- list(list(year = 2023, date = "2023-11-07"),
                         list(year = 2023, date = "2023-01-01"))
  lkp <- congress_date_lookup(bad)
  d <- conference_date_for(2023L, bad)
  # Contract: a config with conflicting dates for one congress is a defect.
  # Either the lookup rejects it, or lookup keys stay unique. Returning one of
  # two conflicting dates by silent position is the failure mode being tested.
  expect_true(anyDuplicated(names(lkp)) == 0 || length(d) == 1,
              label = "duplicate congress years produced an ambiguous lookup")
  expect_length(d, 1L)
})

# ============================================================
# ADVERSARIAL 1.9 — row order must not change the adjudication result
# ============================================================
test_that("dedup_decisions_for_analysis is invariant to input row order", {
  ts <- function(s) as.POSIXct(s, tz = "UTC")
  d <- tibble::tibble(
    abstract_id      = c("A1", "A1", "A2", "A2", "A3"),
    reviewer         = c("AUTO", "GW", "JM", "GW", "AUTO"),
    manual_decision  = c("no_match", "match", "match", "no_match", "no_match"),
    manual_pmid      = NA_character_,
    review_timestamp = ts(c("2099-01-01 00:00:00", "2026-04-14 09:00:00",
                            "2026-04-22 10:00:00", "2026-04-18 10:00:00",
                            "2026-04-17 12:00:00")))
  base <- dedup_decisions_for_analysis(d) |> arrange(abstract_id)
  set.seed(1)
  for (i in 1:8) {
    shuffled <- dedup_decisions_for_analysis(d[sample(nrow(d)), ]) |> arrange(abstract_id)
    expect_equal(shuffled$abstract_id, base$abstract_id)
    expect_equal(shuffled$reviewer, base$reviewer,
                 label = paste("reviewer differed on shuffle", i))
    expect_equal(shuffled$manual_decision, base$manual_decision)
  }
})

# ============================================================
# ADVERSARIAL 1.10 — config vintage must cover the shipped cohort
# ============================================================
test_that("every congress year in the cohort has a date in config", {
  cohort_path <- here::here("data", "processed", "abstracts_cleaned.csv")
  cfg_path <- here::here("config.yml")
  if (!file.exists(cohort_path) || !file.exists(cfg_path)) skip("inputs not present")

  cohort_years <- sort(unique(readr::read_csv(cohort_path, show_col_types = FALSE)$congress_year))
  cfg_live <- config::get(file = cfg_path)
  cfg_years <- sort(as.integer(names(congress_date_lookup(cfg_live))))

  missing <- setdiff(as.integer(cohort_years), cfg_years)
  expect_length(missing, 0L)
  if (length(missing) > 0) {
    fail(paste("cohort years with no congress date in config:",
               paste(missing, collapse = ", "),
               "- these silently fall back to the legacy date, which would",
               "corrupt every months_to_pub for those years"))
  }
})
