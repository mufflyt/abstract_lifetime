# Cycle 22 of 24. Mix: 4 BVA, 3 semantic, 3 adversarial.
#
# Target: output/manual_review_decisions.csv, the artefact itself.
#
# test-decision_precedence_bva.R and test-decision_mutation.R cover the
# FUNCTIONS in R/utils_decisions.R against synthetic input. Nothing tested the
# 2,372-row log they are applied to. That log is the study's only record of
# human judgement, it is committed, it is served to reviewers through the Shiny
# app, and the one defect that most changed the results came from how rows in it
# were selected: AUTO rows outranking human ones by timestamp would have
# discarded 489 of 533 human decisions and moved the rate from 16.9% to 14.1%.
#
# Contracts read from the artefact and from R/utils_decisions.R, not assumed:
#   decisions are match / no_match / skip
#   reviewer is AUTO or a pseudonymous R## (see R/utils_reviewer_ids.R)
#   dedup keeps the latest row per abstract, preferring any human row over AUTO

library(testthat)
library(dplyr)

P_DEC   <- here::here("output", "manual_review_decisions.csv")
P_CLEAN <- here::here("data", "processed", "abstracts_cleaned.csv")
P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_QUEUE <- here::here("output", "manual_review_queue.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")
dec <- function() readr::read_csv(P_DEC, show_col_types = FALSE)

DECISIONS <- c("match", "no_match", "skip")

# ============================================================
# BVA 22.1 - the decision vocabulary is closed and none is empty
# ============================================================
test_that("manual_decision uses only the three documented values", {
  need(P_DEC)
  d <- dec()
  expect_gt(nrow(d), 0)
  v <- d$manual_decision
  expect_true(!any(is.na(v)), label = sprintf("%d rows have no decision at all", sum(is.na(v))))
  expect_setequal(unique(v), DECISIONS)
  # None of the three may be empty: a vocabulary where a level never occurs
  # means the reviewers were never offered it, or it was renamed.
  for (k in DECISIONS) {
    expect_gt(sum(v == k), 0, label = paste("no row carries the decision", k))
  }
})

# ============================================================
# BVA 22.2 - a match decision carries a PMID and the others do not
# ============================================================
test_that("manual_pmid is present exactly for match decisions", {
  need(P_DEC)
  d <- dec()
  m <- d |> filter(manual_decision == "match")
  o <- d |> filter(manual_decision != "match")
  expect_true(all(!is.na(m$manual_pmid)),
              label = sprintf("%d match decisions carry no PMID, so the match names nothing",
                              sum(is.na(m$manual_pmid))))
  # A PMID on a no_match is contradictory: the reviewer said this is not the
  # paper while naming a paper.
  expect_true(all(is.na(o$manual_pmid)),
              label = sprintf("%d non-match decisions carry a PMID: %s",
                              sum(!is.na(o$manual_pmid)),
                              paste(utils::head(o$manual_decision[!is.na(o$manual_pmid)], 3),
                                    collapse = ", ")))
})

# ============================================================
# BVA 22.3 - timestamps are real, ordered and inside the study window
# ============================================================
test_that("review timestamps are parseable and lie in a plausible window", {
  need(P_DEC)
  d <- dec()
  ts <- d$review_timestamp
  expect_true(!any(is.na(ts)),
              label = sprintf("%d rows have no timestamp, and dedup orders on it",
                              sum(is.na(ts))))
  skip_if(all(is.na(ts)), "no timestamps")
  # Dedup at utils_decisions.R keeps the LATEST row per abstract. A timestamp
  # before the first congress or in the future would silently win or lose every
  # tie it takes part in.
  expect_true(all(ts >= as.POSIXct("2012-01-01", tz = "UTC"), na.rm = TRUE),
              label = "a review predates the first congress")
  expect_true(all(ts <= Sys.time() + 86400, na.rm = TRUE),
              label = sprintf("%d reviews are timestamped in the future",
                              sum(ts > Sys.time() + 86400, na.rm = TRUE)))
})

# ============================================================
# BVA 22.4 - every abstract has at least one and the counts add up
# ============================================================
test_that("the log covers the cohort with at least one row per abstract", {
  need(P_DEC, P_CLEAN)
  d <- dec()
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  per <- d |> count(abstract_id, name = "k")
  expect_true(all(per$k >= 1))
  missing <- setdiff(cl$abstract_id, d$abstract_id)
  # An abstract with no row at all was never adjudicated, by a human or by
  # AUTO, so its outcome comes from nowhere.
  expect_true(length(missing) == 0,
              label = sprintf("%d cohort abstracts have no decision row: %s",
                              length(missing),
                              paste(utils::head(missing, 3), collapse = ", ")))
  expect_equal(sum(per$k), nrow(d))
})

# ============================================================
# SEMANTIC 22.5 - reviewer identities are pseudonyms, never names
# ============================================================
test_that("the reviewer column holds only AUTO or a pseudonymous id", {
  need(P_DEC)
  d <- dec()
  r <- unique(as.character(d$reviewer[!is.na(d$reviewer)]))
  bad <- r[!grepl("^(AUTO|R[0-9]{2})$", r)]
  # The deidentification in #9 replaced three sets of real initials with
  # R01/R02/R03. This is the assertion that a future append cannot reintroduce
  # a name, which the guard test checks from the other direction.
  expect_true(length(bad) == 0,
              label = paste("reviewer values that are neither AUTO nor R##:",
                            paste(bad, collapse = ", ")))
})

# ============================================================
# SEMANTIC 22.6 - dedup precedence prefers a human over AUTO
# ============================================================
test_that("no abstract's outcome rests on AUTO where a human also ruled", {
  need(P_DEC)
  d <- dec()
  skip_if_not(file.exists(here::here("R", "utils_decisions.R")), "utils absent")
  source(here::here("R", "utils_decisions.R"))
  kept <- dedup_decisions_for_analysis(d)
  human_ids <- d |> filter(!is.na(reviewer), reviewer != "AUTO") |>
    pull(abstract_id) |> unique()
  # The defect that motivated the whole precedence battery: selecting purely by
  # timestamp let a later AUTO row override an earlier human one. Asserted here
  # against the real log rather than a fixture.
  overridden <- kept |> filter(abstract_id %in% human_ids, reviewer == "AUTO")
  expect_equal(nrow(overridden), 0L,
               label = sprintf("%d abstracts kept an AUTO decision despite a human decision existing",
                               nrow(overridden)))
  expect_equal(anyDuplicated(kept$abstract_id), 0L,
               label = "dedup returned more than one row for an abstract")
})

# ============================================================
# SEMANTIC 22.7 - the queue and the log describe the same work
# ============================================================
test_that("every queued abstract received a human decision", {
  need(P_DEC, P_QUEUE)
  d <- dec(); q <- readr::read_csv(P_QUEUE, show_col_types = FALSE)
  skip_if(nrow(q) == 0, "empty queue")
  human <- d |> filter(!is.na(reviewer), reviewer != "AUTO")
  unreviewed <- setdiff(q$abstract_id, human$abstract_id)
  # The queue is what 05_adjudicate.R decided needed human eyes. An abstract
  # that was queued and never humanly ruled on is unfinished adjudication
  # resolved by algorithm, which is the thing the queue exists to prevent.
  expect_true(length(unreviewed) == 0,
              label = sprintf("%d of %d queued abstracts have no human decision: %s",
                              length(unreviewed), nrow(q),
                              paste(utils::head(unreviewed, 3), collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 22.8 - no reviewer contradicts themself on one abstract
# ============================================================
test_that("a reviewer changing their mind resolves deterministically", {
  need(P_DEC)
  d <- dec()
  pairs <- d |>
    filter(!is.na(reviewer), reviewer != "AUTO") |>
    group_by(abstract_id, reviewer) |>
    filter(n_distinct(manual_decision) > 1) |>
    summarise(n = n(), distinct_ts = n_distinct(review_timestamp), .groups = "drop")
  skip_if(nrow(pairs) == 0, "no reviewer recorded conflicting decisions")

  # 22 abstract/reviewer pairs carry more than one decision. My first version
  # asserted that number was zero, which was the wrong contract: a reviewer
  # revisiting an abstract and changing their answer is legitimate, and dedup
  # exists precisely to keep the later row.
  #
  # What must hold is that the resolution is DETERMINISTIC. Two contradictory
  # decisions by the same reviewer at the same timestamp would be ordered
  # arbitrarily by slice(1), so the recorded outcome would depend on row order
  # in the file rather than on anything the reviewer did.
  tied <- pairs |> filter(distinct_ts < n)
  expect_equal(nrow(tied), 0L,
               label = sprintf(paste("%d abstract/reviewer pairs hold contradictory",
                                     "decisions with duplicate timestamps, so which",
                                     "one survives dedup depends on file order: %s"),
                               nrow(tied), paste(utils::head(tied$abstract_id, 3),
                                                 collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 22.9 - the outcome column follows the decisions kept
# ============================================================
test_that("final_published follows from the deduplicated decisions", {
  need(P_DEC, P_FINAL)
  skip_if_not(file.exists(here::here("R", "utils_decisions.R")), "utils absent")
  source(here::here("R", "utils_decisions.R"))
  d <- dec(); f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  kept <- dedup_decisions_for_analysis(d)
  j <- f |> select(abstract_id, final_published) |>
    inner_join(kept |> select(abstract_id, manual_decision), by = "abstract_id")
  skip_if(nrow(j) == 0, "no overlap")
  # The outcome column may diverge from the decision log, but only for reasons
  # that were decided rather than accumulated. Two exist, both PI decisions of
  # 2026-09-05 and both recorded in docs/OUTCOME_DEFINITION.md:
  #
  #   1. A publication predating the congress is never counted, whatever the
  #      reviewer said. That is an eligibility rule, not a judgment about the
  #      match, so a `match` on such an abstract is correctly unpublished.
  #   2. A HUMAN no_match overrides the algorithm; an AUTO no_match does not.
  #      An AUTO row is a prefill of the algorithm's own verdict, so an AUTO
  #      no_match sitting against a `definite` classification is the algorithm
  #      contradicting itself.
  #
  # Anything outside those two is a genuine disagreement.
  j <- j |>
    left_join(f |> select(abstract_id, months_to_pub), by = "abstract_id") |>
    left_join(kept |> select(abstract_id, reviewer), by = "abstract_id")

  pre_congress <- !is.na(j$months_to_pub) & j$months_to_pub < 0
  is_auto      <- !is.na(j$reviewer) & j$reviewer == "AUTO"
  published    <- j$final_published %in% c(TRUE, "TRUE")

  bad_match <- sum(j$manual_decision == "match" & !published & !pre_congress)
  bad_no    <- sum(j$manual_decision == "no_match" & published & !is_auto)

  expect_equal(bad_match + bad_no, 0L,
               label = sprintf(paste("%d 'match' decisions are unpublished for a reason",
                                     "other than the pre-congress rule, and %d human",
                                     "'no_match' decisions are counted published"),
                               bad_match, bad_no))
})

test_that("an AUTO no_match against a definite classification is a stale prefill", {
  # Not a defect in the cascade, but worth counting: these are AUTO rows whose
  # own note records a superseded scoring vocabulary and which contradict the
  # classification the current scorer assigns. Three abstracts, none seen by a
  # human. If the prefill is ever regenerated they should disappear; if the
  # count grows, something is writing stale decisions again.
  need(P_DEC, P_FINAL)
  source(here::here("R", "utils_decisions.R"))
  d <- dec(); f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  kept <- dedup_decisions_for_analysis(d)
  j <- f |> select(abstract_id, classification, final_published) |>
    inner_join(kept |> select(abstract_id, reviewer, manual_decision),
               by = "abstract_id")
  stale <- j |> filter(reviewer == "AUTO", manual_decision == "no_match",
                       classification == "definite")
  expect_lte(
    nrow(stale), 3L,
    label = sprintf(paste("%d AUTO no_match rows contradict a definite",
                          "classification, up from the 3 known stale prefills: %s"),
                    nrow(stale), paste(utils::head(stale$abstract_id, 5), collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 22.10 - the log is not from an older cohort
# ============================================================
test_that("the decision log refers only to abstracts that still exist", {
  need(P_DEC, P_CLEAN)
  d <- dec()
  parsed <- here::here("data", "processed", "abstracts_parsed.csv")
  known <- if (file.exists(parsed)) {
    unique(readr::read_csv(parsed, show_col_types = FALSE)$abstract_id)
  } else {
    unique(readr::read_csv(P_CLEAN, show_col_types = FALSE)$abstract_id)
  }
  orphans <- setdiff(unique(d$abstract_id), known)
  # Human decisions are expensive and are never regenerated, so a cohort change
  # strands them silently. An orphaned decision is work that no longer applies
  # to anything.
  expect_true(length(orphans) == 0,
              label = sprintf("%d decisions refer to abstracts not in the parse: %s",
                              length(orphans),
                              paste(utils::head(orphans, 3), collapse = ", ")))
})
