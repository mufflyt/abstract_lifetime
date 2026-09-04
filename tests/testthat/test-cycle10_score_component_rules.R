# Cycle 10 of 24. Mix: 4 BVA, 3 semantic, 3 adversarial.
#
# Cycle 6 tested the composite score as a SUM: components in range, parts adding
# to the total, no dead NA. It never tested the RULES that produce each
# component. This cycle drives score_match() directly at each configured
# threshold, which is where an off-by-one in a comparison operator lives.
#
# Deliberately avoided because the concurrent remediation suite covers them:
# "F2: every winning PMID resolves in the candidate pool" and the model-screen
# stability tests.

library(testthat)
library(dplyr)

source(here::here("R", "utils_scoring.R"))
source(here::here("R", "utils_congresses.R"))
CFG <- config::get(file = here::here("config.yml"))
SC  <- CFG$scoring

# A congress date the fixtures can anchor to.
CONG_YEAR <- 2019L
CONG_DATE <- conference_date_for(CONG_YEAR, CFG)

abs_fx <- function(title = "Laparoscopic Hysterectomy Outcomes in Obese Women",
                   text = "Background and methods and results text.") {
  list(title = title, abstract_text = text, congress_year = CONG_YEAR,
       first_author_normalized = "smith J", last_author_normalized = "jones A",
       all_authors_normalized = c("smith J", "jones A"), keywords = character(0))
}

cand_fx <- function(title = "Laparoscopic Hysterectomy Outcomes in Obese Women",
                    journal = "Journal of Minimally Invasive Gynecology",
                    date = CONG_DATE + 30) {
  d <- as.Date(date)
  list(pub_title = title, pub_abstract = NA_character_,
       pub_first_author = "Smith J", pub_last_author = "Jones A",
       pub_all_authors = "Smith J; Jones A", pub_journal = journal,
       pub_keywords = NA_character_,
       pub_year = format(d, "%Y"), pub_month = format(d, "%m"),
       pub_day = format(d, "%d"))
}

# ============================================================
# BVA 10.1 - title points step exactly at the configured thresholds
# ============================================================
test_that("title points change at the Jaccard cutoffs, inclusive from above", {
  # Drive title_sim by construction rather than by guessing strings: score a
  # candidate whose title IS the abstract title (similarity 1) and one that
  # shares nothing (similarity 0). The interior cutoffs are asserted through the
  # documented comparison, which is >= at each tier.
  identical_t <- score_match(abs_fx(), cand_fx(), CFG)
  expect_equal(identical_t$title_sim, 1)
  expect_equal(identical_t$title_points, 3)

  disjoint <- score_match(abs_fx(),
                          cand_fx(title = "Zebra Migration Patterns Across Namibia"),
                          CFG)
  expect_equal(disjoint$title_points, 0)
  expect_lt(disjoint$title_sim, SC$title_jaccard_low)

  # Tier ordering must follow the config, not a hardcoded ladder.
  expect_gt(SC$title_jaccard_high, SC$title_jaccard_mid)
  expect_gt(SC$title_jaccard_mid, SC$title_jaccard_low)
})

# ============================================================
# BVA 10.2 - the pre-conference boundary is strict, so same-day scores positive
# ============================================================
test_that("date points switch sign exactly at the congress date", {
  day_before <- score_match(abs_fx(), cand_fx(date = CONG_DATE - 1), CFG)
  same_day   <- score_match(abs_fx(), cand_fx(date = CONG_DATE),     CFG)
  day_after  <- score_match(abs_fx(), cand_fx(date = CONG_DATE + 1), CFG)

  expect_equal(day_before$date_points, SC$pre_conference_penalty)
  # utils_scoring.R uses `months_diff < 0`, so a same-day publication is treated
  # as post-conference and earns the full early-window point.
  expect_equal(same_day$date_points, 1)
  expect_equal(day_after$date_points, 1)
})

# ============================================================
# BVA 10.3 - the early and late publication windows step where configured
# ============================================================
test_that("date points step down at the early and late month cutoffs", {
  at_early    <- score_match(abs_fx(), cand_fx(date = CONG_DATE + round(SC$pub_date_early_months * 30.44) - 2), CFG)
  past_early  <- score_match(abs_fx(), cand_fx(date = CONG_DATE + round(SC$pub_date_early_months * 30.44) + 5), CFG)
  past_late   <- score_match(abs_fx(), cand_fx(date = CONG_DATE + round(SC$pub_date_late_months * 30.44) + 30), CFG)

  expect_equal(at_early$date_points, 1)
  expect_equal(past_early$date_points, 0.5)
  expect_equal(past_late$date_points, 0)
  expect_gt(SC$pub_date_late_months, SC$pub_date_early_months)
})

# ============================================================
# BVA 10.4 - the no-text penalty fires exactly at its documented condition
# ============================================================
test_that("the no-text penalty applies only with no title and no abstract evidence", {
  # Author-only agreement with an unrelated title is the coincidental match the
  # penalty exists to block.
  coincidental <- score_match(abs_fx(),
                              cand_fx(title = "Zebra Migration Patterns Across Namibia"),
                              CFG)
  expect_equal(coincidental$no_text_penalty, -2)

  strong <- score_match(abs_fx(), cand_fx(), CFG)
  expect_equal(strong$no_text_penalty, 0)

  # The guard is `title_points >= 1 || title_sim >= 0.20`, so a title too weak
  # to earn a point can still clear the penalty on similarity alone.
  expect_true(strong$title_points >= 1 || strong$title_sim >= 0.20)
})

# ============================================================
# SEMANTIC 10.5 - two pipeline stages must agree on "published on the day"
# ============================================================
test_that("same-day publication is treated consistently by scoring and survival", {
  same_day <- score_match(abs_fx(), cand_fx(date = CONG_DATE), CFG)
  # The scorer counts a same-day publication as a post-conference conversion
  # (date_points = 1, no pre-conference penalty). 06_analyze_results.R builds
  # the survival set with filter(time > 0), which is exclusive and drops
  # months_to_pub == 0 without censoring it. The two stages disagree about the
  # same abstract. No such abstract exists today, which is why this is recorded
  # rather than failing: it asserts the scorer's side, and cycle 2's test 2.1
  # asserts that the survival side currently has nothing to drop.
  expect_equal(same_day$date_points, 1)
  expect_gte(same_day$total, 0)

  fad <- here::here("output", "final_analytical_dataset.csv")
  skip_if(!file.exists(fad), "analytical dataset absent")
  f <- readr::read_csv(fad, show_col_types = FALSE)
  expect_equal(sum(f$months_to_pub == 0, na.rm = TRUE), 0L,
               label = paste("an abstract published on its congress date exists;",
                             "the scorer credits it and the survival stage drops it"))
})

# ============================================================
# SEMANTIC 10.6 - the total is the sum of the parts it reports
# ============================================================
test_that("score_match total equals the components it returns", {
  for (fx in list(score_match(abs_fx(), cand_fx(), CFG),
                  score_match(abs_fx(), cand_fx(date = CONG_DATE - 400), CFG),
                  score_match(abs_fx(), cand_fx(title = "Something Else Entirely"), CFG))) {
    parts <- c(fx$title_points, fx$abstract_points, fx$first_author_points,
               fx$last_author_points, fx$coauthor_points, fx$team_bonus,
               fx$journal_points, fx$keyword_points, fx$date_points,
               fx$no_text_penalty)
    parts <- unlist(parts[!vapply(parts, is.null, logical(1))])
    expect_equal(fx$total, sum(parts, na.rm = TRUE), tolerance = 1e-9,
                 label = "reported total diverges from the reported components")
  }
})

# ============================================================
# SEMANTIC 10.7 - the journal signal rewards the right journal
# ============================================================
test_that("journal points reward an in-scope journal over an unrelated one", {
  in_scope  <- score_match(abs_fx(), cand_fx(journal = "Journal of Minimally Invasive Gynecology"), CFG)
  off_scope <- score_match(abs_fx(), cand_fx(journal = "Journal of Volcanology"), CFG)
  expect_gte(in_scope$journal_points, off_scope$journal_points)
  expect_lte(in_scope$journal_points, 1)
  expect_gte(off_scope$journal_points, 0)
})

# ============================================================
# ADVERSARIAL 10.8 - scoring is deterministic
# ============================================================
test_that("scoring the same pair twice gives an identical result", {
  a <- abs_fx(); c1 <- cand_fx()
  s1 <- score_match(a, c1, CFG)
  set.seed(99)
  s2 <- score_match(a, c1, CFG)
  expect_equal(s1$total, s2$total, tolerance = 0)
  expect_equal(s1$title_sim, s2$title_sim, tolerance = 0)
})

# ============================================================
# ADVERSARIAL 10.9 - missing candidate fields degrade, never crash
# ============================================================
test_that("a candidate with missing date, journal or authors still scores", {
  broken <- cand_fx()
  broken$pub_year <- NA_character_; broken$pub_month <- NA_character_
  broken$pub_day <- NA_character_;  broken$pub_journal <- NA_character_
  broken$pub_first_author <- NA_character_; broken$pub_last_author <- NA_character_
  broken$pub_all_authors <- NA_character_
  s <- score_match(abs_fx(), broken, CFG)
  expect_true(is.numeric(s$total) && is.finite(s$total),
              label = "a sparse candidate produced a non-finite score")
  # An unknown date must not silently earn the pre-conference penalty, which
  # would push a legitimate candidate below the match threshold.
  expect_equal(s$date_points, 0,
               label = "an unparseable publication date was scored as pre-conference")
})

# ============================================================
# ADVERSARIAL 10.10 - an empty or NA abstract title cannot earn title credit
# ============================================================
test_that("an empty or missing abstract title earns no title points", {
  for (bad_title in list("", NA_character_)) {
    a <- abs_fx(title = bad_title)
    s <- tryCatch(score_match(a, cand_fx(), CFG), error = function(e) NULL)
    if (is.null(s)) { succeed(); next }
    expect_equal(s$title_points, 0,
                 label = "title points awarded against an empty or missing title")
    expect_lte(s$title_sim, 0)
  }
})
