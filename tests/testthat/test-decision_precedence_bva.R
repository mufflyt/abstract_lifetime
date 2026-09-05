# Boundary value analysis for adjudication precedence, publication-status
# assignment, and the publication-rate denominator.
#
# Every case here corresponds to a defect that reached the analysis outputs.
# The boundary in each case is the exact point where behaviour flips, because
# that is where the original code was wrong.

library(testthat)
library(dplyr)
source(here::here("R", "utils_decisions.R"))

ts <- function(s) as.POSIXct(s, tz = "UTC")

dec <- function(id, reviewer, decision, when, pmid = NA_character_) {
  tibble::tibble(abstract_id = id, reviewer = reviewer,
                 manual_decision = decision, manual_pmid = pmid,
                 review_timestamp = ts(when))
}

# months_to_pub is part of the contract since the pre-congress exclusion became
# the first branch (PI decision, 2026-09-05). The default is a plainly
# post-congress interval so the existing precedence cases below still exercise
# what they were written to exercise; the pre-congress boundary gets its own
# block at BVA 5.
res <- function(id, classification, best_pmid = NA_character_,
                months_to_pub = 12) {
  tibble::tibble(abstract_id = id, classification = classification,
                 best_pmid = best_pmid, months_to_pub = months_to_pub)
}

# ============================================================
# BVA 1: human/AUTO precedence across the timestamp boundary
#
# The original code decided precedence by timestamp. The boundary is the
# instant an AUTO row becomes newer than the human row it competes with.
# Behaviour must be identical on both sides of it.
# ============================================================

test_that("human beats AUTO when AUTO is one second older", {
  d <- bind_rows(dec("A1", "AUTO", "no_match", "2026-04-17 12:00:00"),
                 dec("A1", "R01",   "match",    "2026-04-17 12:00:01"))
  out <- dedup_decisions_for_analysis(d)
  expect_equal(nrow(out), 1L)
  expect_equal(out$reviewer, "R01")
})

test_that("human beats AUTO at the exact same timestamp (the tie boundary)", {
  d <- bind_rows(dec("A1", "AUTO", "no_match", "2026-04-17 12:00:00"),
                 dec("A1", "R01",   "match",    "2026-04-17 12:00:00"))
  expect_equal(dedup_decisions_for_analysis(d)$reviewer, "R01")
})

test_that("human beats AUTO when AUTO is one second NEWER", {
  # This is the regression. Under timestamp-only precedence AUTO wins here.
  d <- bind_rows(dec("A1", "R01",   "match",    "2026-04-17 12:00:00"),
                 dec("A1", "AUTO", "no_match", "2026-04-17 12:00:01"))
  out <- dedup_decisions_for_analysis(d)
  expect_equal(out$reviewer, "R01")
  expect_equal(out$manual_decision, "match")
})

test_that("human survives an AUTO pass re-run far in the future", {
  # The re-run scenario: adjudication re-executes and stamps AUTO with today.
  d <- bind_rows(dec("A1", "R01",   "match",    "2026-04-27 02:15:55"),
                 dec("A1", "AUTO", "no_match", "2099-01-01 00:00:00"))
  expect_equal(dedup_decisions_for_analysis(d)$reviewer, "R01")
})

test_that("AUTO is retained when it is the only decision for an abstract", {
  # Removing AUTO outright strands probable/possible abstracts at NA and
  # silently shrinks the denominator.
  d <- dec("A1", "AUTO", "no_match", "2026-04-17 12:00:00")
  out <- dedup_decisions_for_analysis(d)
  expect_equal(nrow(out), 1L)
  expect_equal(out$reviewer, "AUTO")
})

test_that("AUTO dropped only for the abstract that has a human decision", {
  d <- bind_rows(dec("A1", "AUTO", "no_match", "2026-04-17 12:00:00"),
                 dec("A1", "R01",   "match",    "2026-04-14 09:00:00"),
                 dec("A2", "AUTO", "no_match", "2026-04-17 12:00:00"))
  out <- dedup_decisions_for_analysis(d)
  expect_equal(nrow(out), 2L)
  expect_equal(out$reviewer[out$abstract_id == "A1"], "R01")
  expect_equal(out$reviewer[out$abstract_id == "A2"], "AUTO")
})

test_that("latest human wins among humans; one row per abstract always", {
  d <- bind_rows(dec("A1", "R01",  "no_match", "2026-04-20 10:00:00"),
                 dec("A1", "R02",  "match",    "2026-04-21 10:00:00"),
                 dec("A1", "R03", "skip",     "2026-04-19 10:00:00"))
  out <- dedup_decisions_for_analysis(d)
  expect_equal(nrow(out), 1L)
  expect_equal(out$reviewer, "R02")
})

test_that("rows with NA reviewer are excluded entirely", {
  d <- bind_rows(dec("A1", NA_character_, "match", "2099-01-01 00:00:00"),
                 dec("A1", "R01",          "no_match", "2026-04-14 09:00:00"))
  expect_equal(dedup_decisions_for_analysis(d)$reviewer, "R01")
})

test_that("empty decision log yields zero rows, not an error", {
  d <- dec(character(0), character(0), character(0), character(0))
  expect_equal(nrow(dedup_decisions_for_analysis(d)), 0L)
})

test_that("missing required columns fail loudly", {
  expect_error(dedup_decisions_for_analysis(tibble::tibble(abstract_id = "A1")),
               "missing columns")
})

# ============================================================
# BVA 2: the final_published cascade, one case per branch
#
# Branch order is asserted as-is. Reordering is a methodological decision and
# must break these tests rather than pass silently.
# ============================================================

fp <- function(classification, decision) {
  d <- if (is.na(decision)) dec(character(0), character(0), character(0), character(0))
       else dec("A1", "R01", decision, "2026-04-20 10:00:00")
  assign_final_published(res("A1", classification),
                         dedup_decisions_for_analysis(d))$final_published
}

test_that("definite overrides a human no_match (branch 1 before branch 3)", {
  # 4 abstracts in the real cohort. Documented, not endorsed.
  expect_true(fp("definite", "no_match"))
})

test_that("definite overrides a reviewer skip", {
  # 44 abstracts in the real cohort.
  expect_true(fp("definite", "skip"))
})

test_that("human match promotes a non-definite candidate", {
  expect_true(fp("probable", "match"))
  expect_true(fp("possible", "match"))
  expect_true(fp("excluded", "match"))   # the 4 pre-congress human matches
})

test_that("human no_match demotes a non-definite candidate", {
  expect_false(fp("probable", "no_match"))
  expect_false(fp("possible", "no_match"))
})

test_that("skip on probable or possible is the ONLY route to NA", {
  expect_true(is.na(fp("probable", "skip")))
  expect_true(is.na(fp("possible", "skip")))
  expect_true(is.na(fp("probable", NA)))
  expect_true(is.na(fp("possible", NA)))
})

test_that("no_match, no_candidates and excluded resolve FALSE without a reviewer", {
  expect_false(fp("no_match", NA))
  expect_false(fp("no_candidates", NA))
  expect_false(fp("excluded", NA))
})

test_that("skip on a resolved classification does NOT reach NA", {
  expect_false(fp("no_match", "skip"))
  expect_false(fp("excluded", "skip"))
})

# ============================================================
# BVA 3: denominator arithmetic and its boundaries
# ============================================================

test_that("denominator is cohort minus pending, and the parts close", {
  r <- bind_rows(res("A1","definite"), res("A2","no_match"),
                 res("A3","probable"), res("A4","possible"))
  d <- bind_rows(dec("A3","R01","skip","2026-04-20 10:00:00"),
                 dec("A4","R01","skip","2026-04-20 10:00:00"))
  s <- publication_rate_summary(assign_final_published(r, dedup_decisions_for_analysis(d)))
  expect_equal(s$n_cohort, 4L)
  expect_equal(s$n_pending, 2L)
  expect_equal(s$n_evaluated, 2L)
  expect_equal(s$n_published + s$n_not_published, s$n_evaluated)
  expect_equal(s$publication_rate, 1 / 2)
})

test_that("rate divides by the denominator, never by the cohort", {
  # Boundary: exactly one pending abstract is enough to separate the two.
  r <- bind_rows(res("A1","definite"), res("A2","no_match"), res("A3","probable"))
  d <- dec("A3","R01","skip","2026-04-20 10:00:00")
  s <- publication_rate_summary(assign_final_published(r, dedup_decisions_for_analysis(d)))
  expect_equal(s$publication_rate, 1 / 2)      # denominator 2
  expect_false(isTRUE(all.equal(s$publication_rate, 1 / 3)))  # cohort 3
})

test_that("zero pending makes denominator equal the cohort", {
  r <- bind_rows(res("A1","definite"), res("A2","no_match"))
  s <- publication_rate_summary(
         assign_final_published(r, dedup_decisions_for_analysis(
           dec(character(0), character(0), character(0), character(0)))))
  expect_equal(s$n_pending, 0L)
  expect_equal(s$n_evaluated, s$n_cohort)
})

test_that("all pending gives NA rate, not NaN or an error", {
  r <- bind_rows(res("A1","probable"), res("A2","possible"))
  s <- publication_rate_summary(
         assign_final_published(r, dedup_decisions_for_analysis(
           dec(character(0), character(0), character(0), character(0)))))
  expect_equal(s$n_evaluated, 0L)
  expect_true(is.na(s$publication_rate))
})

# ============================================================
# BVA 4: the live pipeline output must satisfy the same invariants
# ============================================================

test_that("shipped outputs satisfy the denominator invariants", {
  path <- here::here("output", "final_analytical_dataset.csv")
  if (!file.exists(path)) skip("pipeline output not present")
  f <- readr::read_csv(path, show_col_types = FALSE)
  s <- publication_rate_summary(f)

  expect_equal(s$n_evaluated, s$n_cohort - s$n_pending)
  expect_equal(s$n_published + s$n_not_published, s$n_evaluated)
  expect_true(s$publication_rate > 0 && s$publication_rate < 1)

  na_rows <- f[is.na(f$final_published), ]
  expect_true(all(na_rows$classification %in% c("probable", "possible")),
              label = "every unresolved abstract is probable or possible")
})

test_that("exported rate is reconstructible from exported counts", {
  p1 <- here::here("output", "aim1_publication_rate.csv")
  p2 <- here::here("output", "final_analytical_dataset.csv")
  if (!file.exists(p1) || !file.exists(p2)) skip("pipeline output not present")
  a <- readr::read_csv(p1, show_col_types = FALSE)
  s <- publication_rate_summary(readr::read_csv(p2, show_col_types = FALSE))
  val <- function(m) a$value[a$metric == m]

  expect_true("n_evaluated" %in% a$metric,
              label = "aim1 must export the denominator it divided by")
  expect_equal(val("n_evaluated"), s$n_evaluated)
  expect_equal(round(val("published") / val("n_evaluated") * 100, 1),
               val("publication_rate"))
})

# ============================================================
# BVA 5: the pre-congress boundary
#
# PI decision, 2026-09-05: a publication that appeared before the congress
# cannot be a conference-to-publication conversion, and no other evidence
# overrides that. The boundary is months_to_pub == 0, the congress date itself.
#
# The interval is measured to the PRINT ISSUE date, so a paper whose issue is
# dated the month of the congress lands at or near zero. AAGL2023_048 is the
# real case: six days before, which under the electronic date would have read
# as five months before.
# ============================================================

test_that("a publication on the congress date itself counts as published", {
  d <- dec("A1", "AUTO", "match", "2026-01-01 00:00:00")
  out <- assign_final_published(res("A1", "definite", months_to_pub = 0),
                                dedup_decisions_for_analysis(d))
  expect_true(out$final_published)
})

test_that("a publication just before the congress does not count", {
  d <- dec("A1", "AUTO", "match", "2026-01-01 00:00:00")
  out <- assign_final_published(res("A1", "definite", months_to_pub = -0.01),
                                dedup_decisions_for_analysis(d))
  expect_false(out$final_published)
})

test_that("a definite classification does not override the exclusion", {
  # This is the branch order the decision changed. `definite` used to win
  # outright, which is how AAGL2015_010 was counted despite its credited paper
  # predating the congress by two weeks.
  d <- dec("A1", "AUTO", "match", "2026-01-01 00:00:00")
  out <- assign_final_published(res("A1", "definite", months_to_pub = -0.5),
                                dedup_decisions_for_analysis(d))
  expect_false(out$final_published)
})

test_that("a human match does not override the exclusion", {
  d <- dec("A1", "R01", "match", "2026-01-01 00:00:00")
  out <- assign_final_published(res("A1", "excluded", months_to_pub = -5.1),
                                dedup_decisions_for_analysis(d))
  expect_false(out$final_published)
})

test_that("two independent human matches do not override the exclusion", {
  # AAGL2023_048 had exactly this: two reviewers, three days apart, both match.
  d <- rbind(dec("A1", "R03", "match", "2026-01-01 00:00:00"),
             dec("A1", "R01", "match", "2026-01-04 00:00:00"))
  out <- assign_final_published(res("A1", "excluded", months_to_pub = -0.2),
                                dedup_decisions_for_analysis(d))
  expect_false(out$final_published)
})

test_that("a missing interval is undated, not early", {
  # An abstract with no resolvable publication date must not be swept into the
  # exclusion. It is decided on the evidence that exists.
  d <- dec("A1", "R01", "match", "2026-01-01 00:00:00")
  out <- assign_final_published(res("A1", "possible", months_to_pub = NA_real_),
                                dedup_decisions_for_analysis(d))
  expect_true(out$final_published)
})

test_that("the exclusion does not resurrect an abstract a reviewer rejected", {
  # Pre-congress and a human no_match both point the same way; the result must
  # be FALSE, not NA.
  d <- dec("A1", "R01", "no_match", "2026-01-01 00:00:00")
  out <- assign_final_published(res("A1", "excluded", months_to_pub = -3),
                                dedup_decisions_for_analysis(d))
  expect_false(out$final_published)
})

test_that("assign_final_published refuses input without the interval", {
  # The rule cannot be applied silently-not-at-all. A caller passing a table
  # without months_to_pub gets an error, not a quietly weaker cascade.
  d <- dec("A1", "AUTO", "match", "2026-01-01 00:00:00")
  bare <- tibble::tibble(abstract_id = "A1", classification = "definite",
                         best_pmid = NA_character_)
  expect_error(
    assign_final_published(bare, dedup_decisions_for_analysis(d)),
    "months_to_pub")
})
