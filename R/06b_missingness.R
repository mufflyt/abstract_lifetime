# 06b_missingness.R — Missing-data reporting for the analytical dataset.
#
# Why this exists
# ---------------
# docs/STATISTICAL_ANALYSIS.md records the missing-data handling as
# "complete-case throughout. No imputation anywhere in the pipeline." That is
# accurate but it is not a missing-data analysis: the manuscript reports no
# missingness table, no mechanism test, and no check on the one assumption the
# headline rate depends on — that the 55 abstracts with unresolved adjudication
# are missing at random with respect to publication status
# (docs/COHORT_ASSEMBLY.md section 8).
#
# This script produces both.
#
# Section 1 uses mysterycall::build_missingness_mcar_table(), which reports
# item-level missingness per variable and runs Little's MCAR test over the
# numeric block. Package pinned in docs/REPRODUCIBILITY.md; the script degrades
# to the descriptive table alone if it is absent.
#
# Section 2 is specific to this study and is not from that package: it compares
# the unresolved abstracts against the evaluated ones on every covariate the
# models use, which is the assumption test the denominator rests on.
#
# Outputs:
#   output/missingness_by_variable.csv
#   output/missingness_mcar.csv
#   output/missingness_interpretation.txt
#   output/unresolved_vs_evaluated.csv

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(cli); library(tibble)
})

cli_h2("Missing-data analysis")

fad_path <- here("output", "final_analytical_dataset.csv")
if (!file.exists(fad_path)) {
  cli_alert_danger("No final_analytical_dataset.csv - run 06_analyze_results.R first")
} else {

results <- read_csv(fad_path, show_col_types = FALSE)

# Variables that carry real item-level missingness and are used in analysis or
# reporting. Flags derived by regex are never NA and are excluded.
item_vars <- intersect(
  c("sample_size", "months_to_pub", "cited_by_count", "journal_impact_proxy",
    "gender_unified", "state_unified", "subspecialty_unified",
    "practice_type", "subspecialty", "first_author_first"),
  names(results)
)

# ---- 1. Missingness per variable, and Little's MCAR test --------------------

if (requireNamespace("mysterycall", quietly = TRUE)) {
  mc <- mysterycall::build_missingness_mcar_table(
    results, item_vars = item_vars, run_mcar = TRUE
  )

  write_csv(mc$missingness, here("output", "missingness_by_variable.csv"))
  cli_alert_success("Missingness by variable: {nrow(mc$missingness)} variables")
  print(as.data.frame(
    mc$missingness[, intersect(c("variable", "n_total", "n_missing", "pct_missing"),
                               names(mc$missingness))]
  ))

  if (!is.null(mc$mcar)) {
    mcar_row <- tibble(
      test = "little_mcar",
      statistic = mc$mcar$statistic,
      df = mc$mcar$df,
      p_value = mc$mcar$p.value,
      n_patterns = mc$mcar$n_missing_patterns,
      n_rows = mc$mcar$n_rows_tested,
      vars_tested = paste(mc$mcar$vars_tested, collapse = "; "),
      note = mc$mcar$note %||% NA_character_
    )
    write_csv(mcar_row, here("output", "missingness_mcar.csv"))
    cli_alert_info(
      "Little's MCAR: chi-square {round(mc$mcar$statistic, 1)}, \\
       df {mc$mcar$df}, p {format.pval(mc$mcar$p.value, digits = 3)}"
    )
    # Little's test is only defined on the numeric block; the categorical
    # variables above are described but not tested. Recorded, not hidden.
    if (!is.null(mc$mcar$note)) cli_alert_info(mc$mcar$note)
  }

  if (!is.null(mc$interpretation)) {
    writeLines(mc$interpretation, here("output", "missingness_interpretation.txt"))
    cli_alert_success("Interpretation written for the manuscript")
  }
} else {
  cli_alert_warning("mysterycall not installed - descriptive missingness only")
  cli_alert_info("Install with: remotes::install_github('mufflyt/mysterycall@42d66d92')")
  desc <- tibble(
    variable = item_vars,
    n_total = nrow(results),
    n_missing = vapply(item_vars, function(v) sum(is.na(results[[v]])), integer(1)),
    pct_missing = round(100 * vapply(item_vars,
                                     function(v) mean(is.na(results[[v]])),
                                     numeric(1)), 1)
  )
  write_csv(desc, here("output", "missingness_by_variable.csv"))
  print(as.data.frame(desc))
}

# ---- 2. Are the unresolved abstracts missing at random? --------------------
#
# The publication rate divides by the 1,051 evaluated abstracts, dropping the
# 55 whose adjudication never resolved. That is an available-case analysis and
# it assumes the 55 do not differ systematically from the rest. Nothing tested
# it until now.

cli_h3("Unresolved vs evaluated")

results <- results |> mutate(.unresolved = is.na(final_published))
n_unres <- sum(results$.unresolved)
cli_alert_info("{n_unres} unresolved against {nrow(results) - n_unres} evaluated")

compare_vars <- intersect(
  c("congress_year", "is_rct", "is_multicenter", "is_academic", "is_us_based",
    "has_funding", "sample_size", "n_authors", "abstract_word_count",
    "best_score", "n_candidates", "study_design", "gender_unified"),
  names(results)
)

rows <- lapply(compare_vars, function(v) {
  x <- results[[v]]
  if (is.numeric(x) || is.logical(x)) {
    a <- as.numeric(x[results$.unresolved])
    b <- as.numeric(x[!results$.unresolved])
    tt <- suppressWarnings(tryCatch(stats::wilcox.test(a, b), error = function(e) NULL))
    tibble(variable = v, type = "numeric",
           unresolved = round(mean(a, na.rm = TRUE), 3),
           evaluated  = round(mean(b, na.rm = TRUE), 3),
           test = "wilcoxon",
           p_value = if (is.null(tt)) NA_real_ else round(tt$p.value, 4))
  } else {
    tab <- table(x, results$.unresolved)
    ct <- suppressWarnings(tryCatch(stats::chisq.test(tab), error = function(e) NULL))
    tibble(variable = v, type = "categorical",
           unresolved = NA_real_, evaluated = NA_real_,
           test = "chi-square",
           p_value = if (is.null(ct)) NA_real_ else round(ct$p.value, 4))
  }
})

cmp <- bind_rows(rows) |> arrange(p_value)

# Two of these comparisons are definitional rather than informative, and must
# be labelled as such or they will be over-read. An abstract is unresolved only
# if the algorithm scored it `probable` or `possible`, which is the mid-score
# band by construction, so `best_score` and `n_candidates` MUST differ from a
# comparison group containing 709 `no_match` abstracts. A difference in any
# other covariate is a real signal about who fails to get adjudicated.
DEFINITIONAL <- c("best_score", "n_candidates")
cmp <- cmp |>
  mutate(interpretation = if_else(
    variable %in% DEFINITIONAL,
    "definitional - unresolved are the mid-score band by construction",
    "substantive"
  ))
write_csv(cmp, here("output", "unresolved_vs_evaluated.csv"))

print(as.data.frame(cmp))

substantive <- cmp |> filter(interpretation == "substantive", p_value < 0.05)
if (nrow(substantive) > 0) {
  cli_alert_warning(
    "{nrow(substantive)} substantive covariate{?s} differ between the unresolved \\
     and the evaluated at p < 0.05: {paste(substantive$variable, collapse = ', ')}"
  )
  cli_alert_info(
    "The 55 are therefore NOT missing completely at random. Dropping them from \\
     the denominator assumes missing-at-random given the observed data, which is \\
     weaker and untestable. docs/COHORT_ASSEMBLY.md bounds the rate at 16.1% if \\
     all are unpublished and 21.1% if all are published."
  )
} else {
  cli_alert_success(
    "No substantive covariate distinguishes the unresolved from the evaluated"
  )
}

}
