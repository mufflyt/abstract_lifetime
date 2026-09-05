# Cycle 13 of 24. Mix: 4 BVA, 3 semantic, 3 adversarial.
#
# Target: the enrichment layer that produces the demographic covariates. Cycle
# 12 showed one of those covariates (first_author_country) was being populated
# by a wrong rule. This cycle asks whether the OTHER enrichment signals are
# doing anything at all, or are present-but-inert.
#
# Not duplicated: the concurrent suite's gender NPPES tier tests cover the
# gender resolution policy specifically.

library(testthat)
library(dplyr)

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")
dat <- function() readr::read_csv(P_FINAL, show_col_types = FALSE)

# ============================================================
# BVA 13.1 - the NPI score is bounded and tracks its own confidence tiers
# ============================================================
test_that("npi_match_score is non-negative and ordered by confidence tier", {
  need(P_FINAL)
  f <- dat()
  skip_if(!all(c("npi_match_score", "npi_match_confidence") %in% names(f)), "columns absent")
  x <- f$npi_match_score[!is.na(f$npi_match_score)]
  skip_if(length(x) == 0, "no scores")
  expect_true(all(x >= 0), label = "a negative NPI match score has no meaning")
  expect_true(all(is.finite(x)))
  # A tier label that does not track the score it was derived from would let a
  # weak match be recorded as high confidence.
  med <- f |> filter(!is.na(npi_match_confidence), !is.na(npi_match_score)) |>
    group_by(npi_match_confidence) |>
    summarise(m = median(npi_match_score), .groups = "drop")
  if (all(c("high", "low") %in% med$npi_match_confidence)) {
    expect_gt(med$m[med$npi_match_confidence == "high"],
              med$m[med$npi_match_confidence == "low"],
              label = "median score for 'high' confidence is not above 'low'")
  }
})

# ============================================================
# BVA 13.2 - citation counts are non-negative integers
# ============================================================
test_that("cited_by_count is a non-negative integral count", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"cited_by_count" %in% names(f), "column absent")
  x <- f$cited_by_count[!is.na(f$cited_by_count)]
  skip_if(length(x) == 0, "no citation counts")
  expect_true(all(x >= 0))
  expect_true(all(x == floor(x)), label = "a fractional citation count is a parse error")
  expect_true(all(is.finite(x)))
})

# ============================================================
# BVA 13.3 - the impact proxy is a non-negative bounded quantity
# ============================================================
test_that("journal_impact_proxy is non-negative and finite", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"journal_impact_proxy" %in% names(f), "column absent")
  x <- f$journal_impact_proxy[!is.na(f$journal_impact_proxy)]
  skip_if(length(x) == 0, "no values")
  expect_true(all(x >= 0))
  expect_true(all(is.finite(x)))
})

# ============================================================
# BVA 13.4 - classifier outputs come from closed vocabularies
# ============================================================
test_that("practice_type and career_stage emit only documented values", {
  need(P_FINAL)
  f <- dat()
  vocab <- list(
    practice_type = c("academic", "community", "private_practice",
                      "research_institute", "military_va"),
    career_stage  = c("student", "resident", "fellow", "early_faculty",
                      "senior_faculty", "other")
  )
  # Check the fresh enrichment output as well as the analytical dataset. The
  # latter is written by 06_analyze_results.R and can lag; a vocabulary drift
  # introduced upstream would otherwise stay invisible until that stage reruns.
  ac <- here::here("data", "processed", "author_characteristics.csv")
  frames <- list(final = f)
  if (file.exists(ac)) frames$author_characteristics <- readr::read_csv(ac, show_col_types = FALSE)

  problems <- character(0)
  for (nm in names(frames)) {
    d <- frames[[nm]]
    for (v in names(vocab)) {
      if (!v %in% names(d)) next
      seen <- unique(d[[v]][!is.na(d[[v]])])
      extra <- setdiff(seen, vocab[[v]])
      if (length(extra)) {
        problems <- c(problems, sprintf("%s$%s: %s", nm, v, paste(extra, collapse = ", ")))
      }
    }
  }
  # career_stage and orcid_career_stage describe the same concept. If one says
  # "faculty_senior" and the other "senior_faculty", any code joining or
  # comparing them silently treats them as different levels.
  expect_true(length(problems) == 0,
              label = paste("undocumented classifier value(s):",
                            paste(problems, collapse = "; ")))
})

# ============================================================
# SEMANTIC 13.5 - an NPI is recorded only when the match is high confidence
# ============================================================
test_that("npi_number is assigned only at high confidence", {
  need(P_FINAL)
  f <- dat()
  skip_if(!all(c("npi_number", "npi_match_confidence") %in% names(f)), "columns absent")
  assigned <- f |> filter(!is.na(npi_number))
  skip_if(nrow(assigned) == 0, "no NPIs assigned")
  # 10_npi_matching.R:441 assigns the NPI only when confidence == "high". If a
  # lower tier carries one, every demographic derived from it inherits that
  # weaker evidence without saying so.
  expect_true(all(assigned$npi_match_confidence == "high"),
              label = paste(sum(assigned$npi_match_confidence != "high"),
                            "NPIs are recorded at a confidence below 'high'"))
})

# ============================================================
# SEMANTIC 13.6 - a classifier must be able to reach every class it documents
# ============================================================
test_that("practice_type reaches the community class its rules describe", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"practice_type" %in% names(f), "column absent")
  seen <- unique(f$practice_type[!is.na(f$practice_type)])
  # utils_affiliation.R documents rule 6: "Remaining hospital/clinic
  # affiliations are 'community' for US and 'academic' for non-US". If no US
  # hospital affiliation in 1,106 abstracts reaches that branch, the rule is
  # unreachable and every such affiliation is being labelled academic instead.
  expect_true("community" %in% seen,
              label = paste("practice_type never emits 'community'; observed classes are",
                            paste(sort(seen), collapse = ", "),
                            "- the documented US hospital branch is unreachable"))
})

# ============================================================
# SEMANTIC 13.7 - a quality flag that never fires is not a check
# ============================================================
test_that("orcid_false_positive is a live flag rather than a constant", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"orcid_false_positive" %in% names(f), "column absent")
  x <- f$orcid_false_positive[!is.na(f$orcid_false_positive)]
  skip_if(length(x) == 0, "no values")
  # A flag that is FALSE on every row it covers gives the same answer as no
  # flag at all. Either the detector never triggers, or it is not being written.
  expect_gt(length(unique(x)), 1,
            label = sprintf(paste("orcid_false_positive is %s on all %d rows;",
                                  "the check cannot distinguish anything"),
                            as.character(x[1]), length(x)))
})

# ============================================================
# ADVERSARIAL 13.8 - a classifier resolving 3 rows in 1,106 is not working
# ============================================================
test_that("career_stage resolves for a usable share of the cohort", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"career_stage" %in% names(f), "column absent")
  resolved <- sum(!is.na(f$career_stage))
  # This is not a demand for completeness. It is a demand that the stage does
  # something: at 3 of 1,106 the column cannot support any analysis and its
  # presence in the dataset implies a coverage it does not have.
  expect_gt(resolved / nrow(f), 0.05,
            label = sprintf("career_stage resolves %d of %d rows (%.1f%%)",
                            resolved, nrow(f), 100 * resolved / nrow(f)))
})

# ============================================================
# ADVERSARIAL 13.9 - enrichment coverage must not track congress year
# ============================================================
test_that("enrichment coverage does not concentrate in particular congresses", {
  need(P_FINAL)
  f <- dat()
  vars <- intersect(c("npi_number", "cited_by_count", "practice_type"), names(f))
  skip_if(length(vars) == 0, "no enrichment columns")
  offenders <- character(0)
  for (v in vars) {
    by_yr <- f |> group_by(congress_year) |>
      summarise(cov = mean(!is.na(.data[[v]])), .groups = "drop")
    spread <- max(by_yr$cov) - min(by_yr$cov)
    # Cycle 12 found sample_size missingness spanning 13% to 93% across years,
    # which biases any year-stratified comparison. The same trap applies here.
    if (spread > 0.6) {
      offenders <- c(offenders, sprintf("%s (%.0f%% spread)", v, 100 * spread))
    }
  }
  expect_true(length(offenders) == 0,
              label = paste("enrichment coverage concentrates by congress year:",
                            paste(offenders, collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 13.10 - no shipped column is entirely absent or entirely constant
# ============================================================
test_that("no enrichment column is wholly missing or single-valued", {
  need(P_FINAL)
  f <- dat()
  enrich <- grep("^(npi_|orcid_|gender_|first_author_)", names(f), value = TRUE)
  skip_if(length(enrich) == 0, "no enrichment columns")
  dead <- character(0)
  for (v in enrich) {
    x <- f[[v]]
    if (all(is.na(x))) { dead <- c(dead, paste0(v, " (all NA)")); next }
    u <- unique(x[!is.na(x)])
    if (length(u) == 1) dead <- c(dead, sprintf("%s (constant '%s')", v, as.character(u)))
  }
  expect_true(length(dead) == 0,
              label = paste("column(s) carry no information:",
                            paste(dead, collapse = ", ")))
})
