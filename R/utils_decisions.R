# utils_decisions.R — adjudication precedence and publication-status assignment.
#
# Extracted from 06_analyze_results.R so the rules can be exercised directly by
# tests. Both functions are pure: they take data frames and return data frames,
# read nothing from disk, and depend on no global state.
#
# Every rule encoded here was the site of a defect. See
# tests/testthat/test-decision_precedence_bva.R for the boundary cases and
# tests/testthat/test-decision_mutation.R for the planted defects that must be
# caught if any of this is edited.

suppressPackageStartupMessages({
  library(dplyr)
})

#' Reduce the decision log to one row per abstract
#'
#' Two reviewer populations share this table: human reviewers, and the
#' algorithmic pass recorded as reviewer "AUTO". Precedence between them must
#' not depend on timestamps.
#'
#' A human decision always outranks AUTO for the same abstract, regardless of
#' which was written first. AUTO is retained where no human ever ruled, because
#' discarding it would strand probable/possible abstracts at NA and silently
#' remove them from the publication-rate denominator.
#'
#' Among rows from the same population, the most recent timestamp wins.
#'
#' @param decisions Decision log with abstract_id, reviewer, manual_decision,
#'   manual_pmid, review_timestamp.
#' @return One row per abstract_id.
dedup_decisions_for_analysis <- function(decisions) {
  stopifnot(is.data.frame(decisions))
  required <- c("abstract_id", "reviewer", "manual_decision", "review_timestamp")
  missing_cols <- setdiff(required, names(decisions))
  if (length(missing_cols) > 0) {
    stop("dedup_decisions_for_analysis: missing columns: ",
         paste(missing_cols, collapse = ", "))
  }

  human_reviewed_ids <- decisions |>
    filter(!is.na(reviewer), reviewer != "AUTO") |>
    pull(abstract_id) |>
    unique()

  decisions |>
    filter(!is.na(reviewer)) |>
    filter(!(abstract_id %in% human_reviewed_ids & reviewer == "AUTO")) |>
    group_by(abstract_id) |>
    arrange(desc(review_timestamp), .by_group = TRUE) |>
    slice(1) |>
    ungroup()
}

#' Assign final publication status
#'
#' The branch order is deliberate and is documented rather than defended.
#' `classification == "definite"` is evaluated before any reviewer branch, so a
#' definite algorithmic match records TRUE even where a reviewer answered
#' no_match or skip. Reordering is a methodological decision, not a bug fix, and
#' the tests assert the current order so that any change is visible.
#'
#' NA means unresolved adjudication: the algorithm said probable or possible and
#' no reviewer resolved it. These leave the denominator.
#'
#' @param results Abstract-level table carrying `classification`.
#' @param decisions_deduped Output of `dedup_decisions_for_analysis()`.
#' @return `results` plus `manual_decision`, `manual_pmid`, `final_published`,
#'   `final_pmid`.
assign_final_published <- function(results, decisions_deduped) {
  stopifnot(is.data.frame(results), "classification" %in% names(results))

  # The join below is one-to-one by contract. A decisions table carrying more
  # than one row per abstract silently multiplies cohort rows and inflates every
  # downstream count, which is the failure dedup_decisions_for_analysis() exists
  # to prevent. Fail loudly rather than returning a larger cohort than was
  # passed in. Caught by tests/testthat/test-cycle02_survival_estimand.R.
  if ("abstract_id" %in% names(decisions_deduped)) {
    n_dup <- sum(duplicated(decisions_deduped$abstract_id))
    if (n_dup > 0) {
      stop("assign_final_published: decisions table has ", n_dup,
           " duplicate abstract_id row(s). Pass it through ",
           "dedup_decisions_for_analysis() first.")
    }
  }

  join_cols <- intersect(c("abstract_id", "manual_decision", "manual_pmid"),
                         names(decisions_deduped))

  results |>
    left_join(select(decisions_deduped, all_of(join_cols)), by = "abstract_id") |>
    mutate(
      final_published = case_when(
        classification == "definite" ~ TRUE,
        manual_decision == "match" ~ TRUE,
        manual_decision == "no_match" ~ FALSE,
        classification %in% c("no_match", "no_candidates", "excluded") ~ FALSE,
        TRUE ~ NA
      ),
      final_pmid = if ("manual_pmid" %in% names(results) ||
                       "manual_pmid" %in% join_cols) {
        coalesce(.data$manual_pmid, .data$best_pmid)
      } else {
        .data$best_pmid
      }
    )
}

#' Summarise the publication rate and its denominator
#'
#' The denominator is the cohort minus unresolved abstracts, not the cohort.
#' Reporting only the cohort alongside the rate makes the figure impossible to
#' reconstruct, which is what `aim1_publication_rate.csv` did.
#'
#' @param results_with_fp Table carrying `final_published`.
#' @return One-row tibble: n_cohort, n_pending, n_evaluated, n_published,
#'   n_not_published, publication_rate.
publication_rate_summary <- function(results_with_fp) {
  stopifnot("final_published" %in% names(results_with_fp))

  n_cohort    <- nrow(results_with_fp)
  n_pending   <- sum(is.na(results_with_fp$final_published))
  n_evaluated <- n_cohort - n_pending
  n_published <- sum(results_with_fp$final_published, na.rm = TRUE)

  tibble::tibble(
    n_cohort         = n_cohort,
    n_pending        = n_pending,
    n_evaluated      = n_evaluated,
    n_published      = n_published,
    n_not_published  = n_evaluated - n_published,
    publication_rate = if (n_evaluated == 0) NA_real_ else n_published / n_evaluated
  )
}
