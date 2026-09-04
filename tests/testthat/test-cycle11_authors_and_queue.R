# Cycle 11 of 24. Mix: 3 BVA, 4 semantic, 3 adversarial.
#
# Targets: author-list truncation and the manual review queue. Neither has been
# tested by cycles 1-10 or by the concurrent remediation suite. The truncation
# work matters because team size is a REPORTED SIGNIFICANT PREDICTOR in the
# draft abstract, and the variable behind it is censored.

library(testthat)
library(dplyr)

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_CLEAN <- here::here("data", "processed", "abstracts_cleaned.csv")
P_QUEUE <- here::here("output", "manual_review_queue.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

# ============================================================
# BVA 11.1 - a zero-author abstract must be explainable
# ============================================================
test_that("author_count is zero only for withdrawn abstracts", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  zero <- f |> filter(author_count == 0)
  skip_if(nrow(zero) == 0, "no zero-author abstracts")
  # An abstract with no parseable author is either withdrawn or a parse failure.
  # The two need different responses, so they must be distinguishable.
  expect_true(all(grepl("withdraw", zero$title, ignore.case = TRUE)),
              label = paste(sum(!grepl("withdraw", zero$title, ignore.case = TRUE)),
                            "abstract(s) have no authors and are not marked withdrawn"))
  expect_true(all(is.na(zero$first_author_normalized)))
})

# ============================================================
# BVA 11.2 - a hard ceiling with mass piled on it is truncation
# ============================================================
# REGISTERED FAILING TEST - see tests/expected_failures.yaml and the ledger.
test_that("no author variable piles up at a hard ceiling", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  worst <- NULL
  for (v in intersect(c("author_count", "n_authors"), names(f))) {
    x <- f[[v]][!is.na(f[[v]])]
    if (!length(x)) next
    top <- max(x)
    share <- mean(x == top)
    # A real team-size distribution has a thin right tail. Half the mass sitting
    # exactly on the maximum is the signature of a display or parse cap, and it
    # censors any coefficient estimated on the variable.
    if (share > 0.25) {
      worst <- c(worst, sprintf("%s: max %d, %.1f%% of rows at it", v, top, 100 * share))
    }
  }
  expect_null(worst,
              label = paste("author variables are censored at a ceiling:",
                            paste(worst, collapse = "; ")))
})

# ============================================================
# BVA 11.3 - the queue is the tiers that need a human, and nothing else
# ============================================================
test_that("every queued abstract is probable, possible, or a tie", {
  need(P_FINAL, P_QUEUE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  q <- readr::read_csv(P_QUEUE, show_col_types = FALSE)
  j <- f |> filter(abstract_id %in% q$abstract_id)
  bad <- j |> filter(!(classification %in% c("probable", "possible")),
                     !(has_tie %in% c(TRUE, "TRUE")))
  expect_equal(nrow(bad), 0L,
               label = paste(nrow(bad), "queued abstracts are neither probable,",
                             "possible, nor tied"))
})

# ============================================================
# SEMANTIC 11.4 - team size is a reported predictor and must not be censored
# ============================================================
# REGISTERED FAILING TEST - see tests/expected_failures.yaml and the ledger.
test_that("the team-size predictor spans a usable range", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  skip_if(!"n_authors" %in% names(f), "n_authors absent")
  x <- f$n_authors[!is.na(f$n_authors)]
  # aim3 reports an odds ratio "per additional author". That phrase presumes the
  # variable can take more than a handful of values and is not censored. AAGL
  # abstracts routinely carry more than five authors.
  expect_gt(max(x), 5,
            label = sprintf(paste("n_authors is capped at %d with %.1f%% of rows at the cap;",
                                  "an odds ratio 'per additional author' is estimated over a",
                                  "censored range"), max(x), 100 * mean(x == max(x))))
})

# ============================================================
# SEMANTIC 11.5 - the truncation flag must reach the consumers
# ============================================================
test_that("the authors_truncated flag survives adjudication", {
  need(P_CLEAN)
  awm <- here::here("output", "abstracts_with_matches.csv")
  skip_if(!file.exists(awm), "matches file absent")
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  m  <- readr::read_csv(awm, n_max = 1, show_col_types = FALSE)
  skip_if(!"authors_truncated" %in% names(cl),
          "authors_truncated not computed at cleaning")
  # 02_clean_abstracts.R:54 computes this flag and uses it to suppress
  # last-author credit, then R/05_adjudicate.R's explicit select dropped it.
  # With it gone nothing downstream could tell a censored author list from a
  # genuinely short one. Asserted at the stage that owns the carry-forward;
  # the analytical dataset inherits it from here.
  expect_true("authors_truncated" %in% names(m),
              label = paste("authors_truncated is computed at cleaning and used there,",
                            "but does not survive adjudication; downstream consumers",
                            "cannot tell a censored author list from a short one"))
})

# ============================================================
# SEMANTIC 11.6 - the last-author guard behaves as documented
# ============================================================
test_that("last author is withheld exactly when the list was truncated", {
  need(P_CLEAN)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  skip_if(!all(c("authors_truncated", "author_name_last") %in% names(cl)),
          "columns absent")
  trunc <- cl |> filter(authors_truncated %in% c(TRUE, "TRUE"))
  skip_if(nrow(trunc) == 0, "no truncated lists")
  # This guard is correct and is asserted so it cannot regress: a truncated list
  # must not award last-author credit to whoever happens to be visible last.
  expect_true(all(is.na(trunc$author_name_last)),
              label = paste(sum(!is.na(trunc$author_name_last)),
                            "truncated lists still carry a last author"))
})

# ============================================================
# SEMANTIC 11.7 - the queue's stated rule matches its contents
# ============================================================
test_that("the queue contains every abstract its rule selects, and only those", {
  need(P_FINAL, P_QUEUE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  q <- readr::read_csv(P_QUEUE, show_col_types = FALSE)
  expected <- f |>
    filter(classification %in% c("probable", "possible") | has_tie %in% c(TRUE, "TRUE")) |>
    pull(abstract_id)
  expect_true(setequal(q$abstract_id, expected),
              label = paste("queue and rule disagree on",
                            length(setdiff(union(q$abstract_id, expected),
                                           intersect(q$abstract_id, expected))),
                            "abstracts"))
})

# ============================================================
# ADVERSARIAL 11.8 - no-match abstracts reach the queue only through a tie
# ============================================================
test_that("a no_match abstract is queued only because its candidates tied", {
  need(P_FINAL, P_QUEUE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  q <- readr::read_csv(P_QUEUE, show_col_types = FALSE)
  nm <- f |> filter(abstract_id %in% q$abstract_id, classification == "no_match")
  skip_if(nrow(nm) == 0, "no no_match abstracts queued")
  expect_true(all(nm$has_tie %in% c(TRUE, "TRUE")),
              label = paste(sum(!(nm$has_tie %in% c(TRUE, "TRUE"))),
                            "no_match abstracts are queued without a tie"))
  # Recorded, not asserted as wrong: a tie among candidates that all score below
  # the match threshold is still a no-match. Whether those are worth a
  # reviewer's time is a workload decision, not a correctness one.
  succeed()
})

# ============================================================
# ADVERSARIAL 11.9 - the queue is a clean subset of the cohort
# ============================================================
test_that("the queue has no duplicates and no abstracts outside the cohort", {
  need(P_FINAL, P_QUEUE)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  q <- readr::read_csv(P_QUEUE, show_col_types = FALSE)
  expect_equal(anyDuplicated(q$abstract_id), 0L,
               label = "a duplicated queue row would be adjudicated twice")
  expect_length(setdiff(q$abstract_id, f$abstract_id), 0L)
  expect_lte(nrow(q), nrow(f))
})

# ============================================================
# ADVERSARIAL 11.10 - a parsed author list implies a parsed first author
# ============================================================
test_that("first_author_normalized exists wherever authors were parsed", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  have <- f |> filter(author_count > 0)
  missing <- sum(is.na(have$first_author_normalized))
  expect_equal(missing, 0L,
               label = paste(missing, "abstracts parsed at least one author but",
                             "normalised to NA; every author-based score and the",
                             "NPI match key depend on this field"))
})
