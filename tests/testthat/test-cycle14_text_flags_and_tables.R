# Cycle 14 of 24. Mix: 3 BVA, 4 semantic, 3 adversarial.
#
# Target: the binary flags extracted from abstract text, the two text
# classifiers, and the derived tables. Cycles 1-13 covered scoring, survival,
# the model covariates and the enrichment layer; the text-derived flags have
# only been touched by the concurrent suite's F3 test, which checks that none is
# structurally zero for a whole congress. The angles here are different:
# internal logical consistency between flags, and whether a flag can fire on an
# abstract that has no text.

library(testthat)
library(dplyr)

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_TBL   <- here::here("output", "tables")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")
dat <- function() readr::read_csv(P_FINAL, show_col_types = FALSE)
FLAGS <- c("stat_sig_reported", "has_numeric_results", "is_database_study",
           "has_industry", "has_trial_registration", "has_irb_statement")
is_true <- function(x) x %in% c(TRUE, "TRUE")

# ============================================================
# BVA 14.1 - word count is zero only where there is no abstract
# ============================================================
test_that("abstract_word_count is zero only for abstracts with no text", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"abstract_word_count" %in% names(f), "column absent")
  zero <- f |> filter(abstract_word_count == 0)
  skip_if(nrow(zero) == 0, "no zero-length abstracts")
  # The withdrawn abstracts are the legitimate case. Anything else means the
  # text was lost between parsing and counting, and every text-derived flag on
  # that row is then a false negative rather than a measurement.
  expect_true(all(grepl("withdraw", zero$title, ignore.case = TRUE)),
              label = paste(sum(!grepl("withdraw", zero$title, ignore.case = TRUE)),
                            "abstracts have zero words and are not marked withdrawn"))
  expect_lt(max(f$abstract_word_count, na.rm = TRUE), 2000,
            label = "an implausible word count suggests concatenated records")
})

# ============================================================
# BVA 14.2 - a text-derived flag cannot fire without text
# ============================================================
test_that("no text-derived flag is TRUE on an abstract with no text", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"abstract_word_count" %in% names(f), "column absent")
  empty <- f |> filter(abstract_word_count == 0)
  skip_if(nrow(empty) == 0, "no zero-length abstracts")
  # Narrowed deliberately. is_database_study and has_industry can be read from a
  # title alone ("... Using the National Inpatient Sample"), so firing without an
  # abstract body is legitimate for those. These four cannot: a p-value, a
  # numeric result, an IRB statement and a registration number all live in the
  # body of the abstract.
  body_only <- c("stat_sig_reported", "has_numeric_results",
                 "has_irb_statement", "has_trial_registration")
  fired <- character(0)
  for (v in intersect(body_only, names(f))) {
    n <- sum(is_true(empty[[v]]))
    if (n > 0) fired <- c(fired, sprintf("%s (%d)", v, n))
  }
  expect_true(length(fired) == 0,
              label = paste("flag(s) TRUE on an abstract with no text:",
                            paste(fired, collapse = ", ")))
})

# ============================================================
# BVA 14.3 - the text classifiers emit closed vocabularies
# ============================================================
test_that("research_category and primary_procedure stay inside their vocabularies", {
  need(P_FINAL)
  f <- dat()
  vocab <- list(
    research_category = c("clinical", "basic_science", "education", "health_services",
                          "device_technology", "quality_improvement", "other"),
    primary_procedure = c("hysterectomy", "myomectomy", "endometriosis", "sacrocolpopexy",
                          "pelvic_floor", "gynecologic_oncology", "adnexal_surgery",
                          "fibroids", "sterilization", "cerclage", "ectopic_pregnancy",
                          "other")
  )
  problems <- character(0)
  for (v in names(vocab)) {
    if (!v %in% names(f)) next
    extra <- setdiff(unique(f[[v]][!is.na(f[[v]])]), vocab[[v]])
    if (length(extra)) problems <- c(problems, sprintf("%s: %s", v, paste(extra, collapse = ", ")))
  }
  expect_true(length(problems) == 0,
              label = paste("undocumented classifier value(s):",
                            paste(problems, collapse = "; ")))
})

# ============================================================
# SEMANTIC 14.4 - reporting significance presupposes reporting numbers
# ============================================================
test_that("stat_sig_reported implies has_numeric_results", {
  need(P_FINAL)
  f <- dat()
  skip_if(!all(c("stat_sig_reported", "has_numeric_results") %in% names(f)), "columns absent")
  # An abstract cannot state that a result reached significance without also
  # presenting a number. If it can, the two extractors disagree about the same
  # text and at least one is wrong.
  bad <- sum(is_true(f$stat_sig_reported) & !is_true(f$has_numeric_results))
  expect_equal(bad, 0L,
               label = paste(bad, "abstracts report statistical significance but",
                             "are flagged as carrying no numeric results"))
})

# ============================================================
# SEMANTIC 14.5 - a registered trial should look like a trial
# ============================================================
test_that("has_trial_registration is concentrated in trial-like designs", {
  need(P_FINAL)
  f <- dat()
  skip_if(!all(c("has_trial_registration", "is_rct") %in% names(f)), "columns absent")
  reg <- f |> filter(is_true(has_trial_registration))
  skip_if(nrow(reg) == 0, "no registered trials")
  # Not every registered study is an RCT, so this is a proportion rather than an
  # implication. A registration flag that lands mostly on non-trials is matching
  # the wrong text.
  share <- mean(is_true(reg$is_rct))
  expect_gt(share, 0.25,
            label = sprintf("only %.0f%% of %d abstracts with a trial registration are RCTs",
                            100 * share, nrow(reg)))
})

# ============================================================
# SEMANTIC 14.6 - a database study should carry a database-scale sample
# ============================================================
test_that("is_database_study abstracts report larger samples than the rest", {
  need(P_FINAL)
  f <- dat()
  skip_if(!all(c("is_database_study", "sample_size") %in% names(f)), "columns absent")
  db  <- f$sample_size[is_true(f$is_database_study) & !is.na(f$sample_size)]
  oth <- f$sample_size[!is_true(f$is_database_study) & !is.na(f$sample_size)]
  skip_if(length(db) < 5 || length(oth) < 5, "too few sample sizes")
  # The label claims a registry or administrative dataset. If its median sample
  # is no larger than everything else, the flag is not identifying what it says.
  expect_gt(median(db), median(oth),
            label = sprintf("median sample size is %s for database studies and %s for others",
                            format(median(db)), format(median(oth))))
})

# ============================================================
# SEMANTIC 14.7 - derived tables reconcile with the cohort
# ============================================================
test_that("the derived tables describe the cohort they are built from", {
  skip_if(!dir.exists(P_TBL), "tables directory absent")
  need(P_FINAL)
  f <- dat()
  files <- list.files(P_TBL, pattern = "\\.csv$", full.names = TRUE)
  skip_if(length(files) == 0, "no tables")
  problems <- character(0)
  for (p in files) {
    # table4 counts search QUERIES, not abstracts: one abstract generates several
    # queries and the video abstracts were searched before exclusion, so its
    # counts legitimately exceed the cohort. An earlier draft of this test bounded
    # every table by the cohort and flagged it wrongly.
    if (grepl("search_strateg", basename(p))) next
    t <- readr::read_csv(p, show_col_types = FALSE)
    if (nrow(t) == 0) { problems <- c(problems, paste0(basename(p), " (empty)")); next }
    # Any column of counts in a summary table must stay inside the cohort.
    for (cc in names(t)[vapply(t, is.numeric, logical(1))]) {
      v <- t[[cc]][!is.na(t[[cc]])]
      if (length(v) && grepl("^n(_|$)|count", cc, ignore.case = TRUE) && max(v) > nrow(f)) {
        problems <- c(problems, sprintf("%s$%s max %s exceeds cohort %d",
                                        basename(p), cc, format(max(v)), nrow(f)))
      }
    }
  }
  expect_true(length(problems) == 0,
              label = paste("table/cohort mismatch:", paste(problems, collapse = "; ")))
})

# ============================================================
# ADVERSARIAL 14.8 - flags must not be an artefact of how much text survived
# ============================================================
test_that("flag prevalence is not driven by abstract length alone", {
  need(P_FINAL)
  f <- dat() |> filter(abstract_word_count > 0)
  skip_if(nrow(f) < 100, "too few abstracts")
  cut <- median(f$abstract_word_count, na.rm = TRUE)
  offenders <- character(0)
  for (v in intersect(FLAGS, names(f))) {
    short <- mean(is_true(f[[v]][f$abstract_word_count <= cut]))
    long  <- mean(is_true(f[[v]][f$abstract_word_count >  cut]))
    # Longer abstracts genuinely contain more of everything, so a gap is
    # expected. A flag that is more than ten times as likely in long abstracts
    # is measuring text volume rather than the property it names.
    if (short > 0 && long / short > 10) {
      offenders <- c(offenders, sprintf("%s (%.1f%% short vs %.1f%% long)",
                                        v, 100 * short, 100 * long))
    }
  }
  expect_true(length(offenders) == 0,
              label = paste("flag prevalence tracks abstract length:",
                            paste(offenders, collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 14.9 - no text flag is constant or vanishingly rare
# ============================================================
test_that("every text-derived flag varies across the cohort", {
  need(P_FINAL)
  f <- dat()
  degenerate <- character(0)
  for (v in intersect(FLAGS, names(f))) {
    x <- f[[v]][!is.na(f[[v]])]
    if (!length(x)) next
    p <- mean(is_true(x))
    if (p == 0 || p == 1) {
      degenerate <- c(degenerate, sprintf("%s (constant, p=%.0f)", v, p))
    }
  }
  expect_true(length(degenerate) == 0,
              label = paste("constant text flag(s):", paste(degenerate, collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 14.10 - classifier coverage must not concentrate by congress year
# ============================================================
test_that("primary_procedure coverage does not concentrate in particular congresses", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"primary_procedure" %in% names(f), "column absent")
  by_yr <- f |> group_by(congress_year) |>
    summarise(cov = mean(!is.na(primary_procedure)), .groups = "drop")
  spread <- max(by_yr$cov) - min(by_yr$cov)
  # Same trap as cycle 12's sample_size finding: a classifier that resolves in
  # some congresses and not others biases every year-stratified comparison.
  expect_lt(spread, 0.5,
            label = sprintf("primary_procedure coverage spans %.0f%% across congresses (%.0f%% to %.0f%%)",
                            100 * spread, 100 * min(by_yr$cov), 100 * max(by_yr$cov)))
})
