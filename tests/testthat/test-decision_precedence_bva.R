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

res <- function(id, classification, best_pmid = NA_character_) {
  tibble::tibble(abstract_id = id, classification = classification,
                 best_pmid = best_pmid)
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
                 dec("A1", "GW",   "match",    "2026-04-17 12:00:01"))
  out <- dedup_decisions_for_analysis(d)
  expect_equal(nrow(out), 1L)
  expect_equal(out$reviewer, "GW")
})

test_that("human beats AUTO at the exact same timestamp (the tie boundary)", {
  d <- bind_rows(dec("A1", "AUTO", "no_match", "2026-04-17 12:00:00"),
                 dec("A1", "GW",   "match",    "2026-04-17 12:00:00"))
  expect_equal(dedup_decisions_for_analysis(d)$reviewer, "GW")
})

test_that("human beats AUTO when AUTO is one second NEWER", {
  # This is the regression. Under timestamp-only precedence AUTO wins here.
  d <- bind_rows(dec("A1", "GW",   "match",    "2026-04-17 12:00:00"),
                 dec("A1", "AUTO", "no_match", "2026-04-17 12:00:01"))
  out <- dedup_decisions_for_analysis(d)
  expect_equal(out$reviewer, "GW")
  expect_equal(out$manual_decision, "match")
})

test_that("human survives an AUTO pass re-run far in the future", {
  # The re-run scenario: adjudication re-executes and stamps AUTO with today.
  d <- bind_rows(dec("A1", "GW",   "match",    "2026-04-27 02:15:55"),
                 dec("A1", "AUTO", "no_match", "2099-01-01 00:00:00"))
  expect_equal(dedup_decisions_for_analysis(d)$reviewer, "GW")
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
                 dec("A1", "GW",   "match",    "2026-04-14 09:00:00"),
                 dec("A2", "AUTO", "no_match", "2026-04-17 12:00:00"))
  out <- dedup_decisions_for_analysis(d)
  expect_equal(nrow(out), 2L)
  expect_equal(out$reviewer[out$abstract_id == "A1"], "GW")
  expect_equal(out$reviewer[out$abstract_id == "A2"], "AUTO")
})

test_that("latest human wins among humans; one row per abstract always", {
  d <- bind_rows(dec("A1", "GW",  "no_match", "2026-04-20 10:00:00"),
                 dec("A1", "JM",  "match",    "2026-04-21 10:00:00"),
                 dec("A1", "TMM", "skip",     "2026-04-19 10:00:00"))
  out <- dedup_decisions_for_analysis(d)
  expect_equal(nrow(out), 1L)
  expect_equal(out$reviewer, "JM")
})

test_that("rows with NA reviewer are excluded entirely", {
  d <- bind_rows(dec("A1", NA_character_, "match", "2099-01-01 00:00:00"),
                 dec("A1", "GW",          "no_match", "2026-04-14 09:00:00"))
  expect_equal(dedup_decisions_for_analysis(d)$reviewer, "GW")
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
       else dec("A1", "GW", decision, "2026-04-20 10:00:00")
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
  d <- bind_rows(dec("A3","GW","skip","2026-04-20 10:00:00"),
                 dec("A4","GW","skip","2026-04-20 10:00:00"))
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
  d <- dec("A3","GW","skip","2026-04-20 10:00:00")
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
