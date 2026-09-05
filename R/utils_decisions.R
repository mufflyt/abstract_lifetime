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

  # The pre-congress guard needs the interval. Every production caller passes a
  # table that carries it (R/05_adjudicate.R computes it); a caller that does
  # not would silently lose the rule, so fail rather than skip it.
  if (!"months_to_pub" %in% names(results)) {
    stop("assign_final_published: `results` has no months_to_pub column, so the ",
         "pre-congress exclusion cannot be applied. See docs/OUTCOME_DEFINITION.md.")
  }

  results |>
    left_join(select(decisions_deduped, all_of(join_cols)), by = "abstract_id") |>
    mutate(
      # PI decision, 2026-09-05: a publication that appeared before the
      # congress cannot be a conference-to-publication conversion, and a
      # reviewer's `match` does not override that. The test is therefore the
      # FIRST branch, ahead of both `definite` and the reviewer verdict.
      #
      # It is applied to `months_to_pub`, which is the interval to the paper
      # actually credited to the abstract, measured to the print issue date
      # (PI decision, same day; see docs/OUTCOME_DEFINITION.md). Testing
      # `classification == "excluded"` instead would miss three abstracts:
      # two where a reviewer supplied a PMID other than the scored best
      # candidate, so the pre-conference penalty was computed against a paper
      # that is not the one counted, and one scored `definite` despite its
      # credited paper predating the congress by two weeks.
      #
      # A missing interval is NOT treated as pre-congress: an abstract with no
      # resolvable date is undated, not early, and the branches below decide it
      # on the evidence that does exist.
      final_published = case_when(
        !is.na(.data$months_to_pub) & .data$months_to_pub < 0 ~ FALSE,
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

#' Apply the pre-congress exclusion to an already-assigned outcome
#'
#' PI decision, 2026-09-05: a publication that appeared before the congress
#' cannot be a conference-to-publication conversion, and neither a `definite`
#' classification nor a reviewer's `match` overrides that.
#'
#' `assign_final_published()` applies the same rule, but it can only see the
#' interval its input carries. R/05_adjudicate.R joins publication metadata on
#' `best_pmid` and runs before the reviewer decisions are read, so where a
#' reviewer supplied a different PMID the interval at that point describes the
#' algorithm's candidate rather than the paper being credited, and is sometimes
#' missing altogether. R/06_analyze_results.R re-joins on `final_pmid`, and only
#' after that is the interval the one the rule is about. This function is how
#' the rule gets applied to it.
#'
#' Two abstracts depend on this. AAGL2018_002 and AAGL2018_019 carry a reviewer
#' PMID whose `best_pmid` had no date, so the interval reaching
#' `assign_final_published()` is NA and the rule cannot fire; after the refresh
#' both resolve to 10.3 months before their congress.
#'
#' @param results Table carrying `final_published` and a refreshed `months_to_pub`.
#' @return The same table with `final_published` set FALSE wherever the credited
#'   publication predates the congress. A missing interval is left alone: an
#'   abstract with no resolvable date is undated, not early.
apply_pre_congress_exclusion <- function(results) {
  stopifnot(is.data.frame(results))
  if (!all(c("final_published", "months_to_pub") %in% names(results))) {
    stop("apply_pre_congress_exclusion: needs final_published and months_to_pub")
  }
  pre <- !is.na(results$months_to_pub) & results$months_to_pub < 0
  results$final_published[pre] <- FALSE
  results
}

#' Adopt the outcome R/06_analyze_results.R settled on
#'
#' R/07_make_tables.R and R/08_make_figures.R recompute the outcome from
#' `abstracts_with_matches.csv` through the same cascade, which was intended to
#' stop them drifting from the analysis. It does not quite: 06 refreshes
#' `months_to_pub` against the credited PMID before applying the pre-congress
#' exclusion, and 07 and 08 do not, so an abstract can be unpublished in the
#' analysis and published in the tables.
#'
#' They run after 06 in `00_run_all.R`, so the settled outcome is available on
#' disk. Adopting it is what "cannot drift" actually requires.
#'
#' @return `results` with `final_published` and `final_pmid` taken from the
#'   analytical dataset where it exists, unchanged otherwise.
adopt_analysis_outcome <- function(results,
                                   fad_path = here::here("output", "final_analytical_dataset.csv")) {
  if (!file.exists(fad_path)) return(results)
  auth <- readr::read_csv(fad_path, show_col_types = FALSE)
  if (!all(c("abstract_id", "final_published") %in% names(auth))) return(results)

  keep <- intersect(c("abstract_id", "final_published", "final_pmid", "months_to_pub"),
                    names(auth))
  auth <- dplyr::distinct(auth[, keep, drop = FALSE], .data$abstract_id, .keep_all = TRUE)
  names(auth)[names(auth) != "abstract_id"] <-
    paste0(".auth_", names(auth)[names(auth) != "abstract_id"])

  n_before <- nrow(results)
  out <- dplyr::left_join(results, auth, by = "abstract_id")
  if (nrow(out) != n_before) {
    stop("adopt_analysis_outcome: join changed the row count")
  }
  for (col in setdiff(names(auth), "abstract_id")) {
    target <- sub("^\\.auth_", "", col)
    if (target %in% names(out)) {
      out[[target]] <- dplyr::coalesce(out[[col]], out[[target]])
    }
    out[[col]] <- NULL
  }
  out
}
