# utils_flow_counts.R — the numbers in Figure 1, derived once.
#
# Extracted from R/strobe_flowchart.R so CI can exercise the real derivation
# rather than a copy of it. The assertions previously lived only inside that
# script, which runs only from 00_run_all.R, which CI does not run: a wrong
# count in the figure could reach the manuscript with the whole suite green.
# tests/testthat/test-cycle05_flow_fidelity_tables.R tested a re-implementation
# of these conditions against synthetic tuples, which proves the copy is
# self-consistent and nothing about the figure.

#' Derive every box in the STROBE participant-flow diagram.
#'
#' @param analytic_tbl output/final_analytical_dataset.csv
#' @param n_parsed     rows in data/processed/abstracts_parsed.csv
#' @param n_cohort     rows in data/processed/abstracts_cleaned.csv
#' @param assert       stop() if the arithmetic does not close
#' @return named integer list, one entry per box
derive_flow_counts <- function(analytic_tbl, n_parsed, n_cohort, assert = TRUE) {
  n_pending   <- sum(is.na(analytic_tbl$final_published))
  n_evaluated <- n_cohort - n_pending
  n_published <- sum(analytic_tbl$final_published, na.rm = TRUE)
  n_not_pub   <- n_evaluated - n_published

  # Abstracts whose credited publication predates their congress. PI decision,
  # 2026-05-09: such a paper cannot be a conference-to-publication conversion,
  # so they are counted UNPUBLISHED. They stay in the denominator, which is why
  # they are a breakdown of "Not published" and not an exclusion arrow off the
  # spine -- an exclusion arrow would say they left the study, and they did not.
  # See docs/OUTCOME_DEFINITION.md.
  n_pre_congress <- sum(!is.na(analytic_tbl$months_to_pub) &
                          analytic_tbl$months_to_pub < 0, na.rm = TRUE)
  n_no_pub_found <- n_not_pub - n_pre_congress

  out <- list(
    parsed = n_parsed, video = n_parsed - n_cohort, cohort = n_cohort,
    pending = n_pending, evaluated = n_evaluated, published = n_published,
    not_published = n_not_pub, pre_congress = n_pre_congress,
    no_pub_found = n_no_pub_found)
  out <- lapply(out, as.integer)

  if (assert) {
    stopifnot(
      nrow(analytic_tbl) == n_cohort,
      n_published + n_not_pub == n_evaluated,
      n_evaluated + n_pending == n_cohort,
      # No abstract may be counted published on a paper that predates its
      # congress. This is the outcome rule, not arithmetic: if it ever fails
      # the cascade in R/utils_decisions.R has regressed.
      sum(analytic_tbl$final_published %in% TRUE &
            analytic_tbl$months_to_pub < 0, na.rm = TRUE) == 0,
      n_no_pub_found + n_pre_congress == n_not_pub,
      # Every box is a count, so none may be negative -- a negative box means a
      # subtraction ran on mismatched inputs and still "closed".
      all(vapply(out, function(x) x >= 0L, logical(1)))
    )
  }
  out
}
