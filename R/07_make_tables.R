# 07_make_tables.R — Generate publication-quality tables

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(cli)
library(config)

source(here("R", "utils_decisions.R"))

cfg <- config::get(file = here("config.yml"))

dir.create(here("output", "tables"), showWarnings = FALSE, recursive = TRUE)

cli_h2("Generating Tables")

results <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)

# Determine final published status.
# Precedence and the outcome cascade live in R/utils_decisions.R so that this
# script cannot drift from R/06_analyze_results.R. The inline copy that used to
# live here omitted the human-outranks-AUTO rule and agreed with the analysis
# only by accident of timestamp ordering. See docs/FAILURE_MODES.md F9.
if (file.exists(here("output", "manual_review_decisions.csv"))) {
  decisions <- read_csv(here("output", "manual_review_decisions.csv"), show_col_types = FALSE)
  results <- assign_final_published(results, dedup_decisions_for_analysis(decisions))
  # 06 refreshes months_to_pub against the credited PMID before applying the
  # pre-congress exclusion; this script does not, so recomputing the cascade
  # alone is not enough to match it. Adopt the settled outcome instead. 06 runs
  # first in 00_run_all.R. See R/utils_decisions.R.
  results <- adopt_analysis_outcome(results)
} else {
  results <- results |>
    mutate(final_published = classification == "definite")
}

# ============================================================
# Table 1: Abstract Characteristics
# ============================================================
cli_alert_info("Table 1: Abstract Characteristics")

table1_data <- results |>
  filter(!is.na(final_published)) |>
  mutate(Published = if_else(final_published, "Published", "Not published"))

# Covariates in reporting order. Only those actually present are used, so a run
# without the demographics merge produces a shorter table rather than an error.
table1_vars <- intersect(
  c("is_rct", "is_multicenter", "is_academic", "is_us_based",
    "study_design", "research_category", "gender_unified",
    "n_authors", "sample_size", "abstract_word_count"),
  names(table1_data)
)

table1_path <- here("output", "tables", "table1_characteristics.csv")

if (requireNamespace("mysterycall", quietly = TRUE)) {
  # mysterycall::mysterycall_table1() gives a stratified table with median [IQR]
  # and mean (SD) for continuous variables, level-wise counts and percentages
  # for categorical ones, and a p-value per variable. The hand-rolled version
  # this replaces was two rows wide with five variables and no test, and could
  # not show the study_design or gender distributions at all.
  #
  # min_cell = 5 suppresses small cells. Package pinned in
  # docs/REPRODUCIBILITY.md.
  t1_obj <- mysterycall::mysterycall_table1(
    table1_data,
    covariates    = table1_vars,
    stratify_by   = "Published",
    include_overall = TRUE,
    cont_stats    = c("median_iqr", "mean_sd"),
    p_value       = TRUE,
    min_cell      = 5L
  )
  # It returns a classed list; `table` is the tibble and `column_ns` carries the
  # stratum sizes, which belong in the caption rather than being recomputed.
  table1 <- t1_obj$table
  write_csv(table1, table1_path)

  ns <- t1_obj$column_ns
  write_csv(
    tibble::tibble(stratum = names(ns), n = as.integer(ns)),
    here("output", "tables", "table1_column_ns.csv")
  )
  cli_alert_success(
    "Table 1 saved ({nrow(table1)} rows, {length(table1_vars)} variables, \
     with p-values; strata {paste(names(ns), ns, sep = '=', collapse = ', ')})"
  )
} else {
  cli_alert_warning("mysterycall not installed - writing the reduced Table 1")
  cli_alert_info("Install with: remotes::install_github('mufflyt/mysterycall@42d66d92')")
  table1 <- table1_data |>
    group_by(final_published) |>
    summarise(
      n = n(),
      n_rct = sum(is_rct, na.rm = TRUE),
      pct_rct = round(mean(is_rct, na.rm = TRUE) * 100, 1),
      median_sample_size = median(sample_size, na.rm = TRUE),
      iqr_sample_size = paste0(
        round(quantile(sample_size, 0.25, na.rm = TRUE)), "-",
        round(quantile(sample_size, 0.75, na.rm = TRUE))
      ),
      n_academic = sum(is_academic, na.rm = TRUE),
      pct_academic = round(mean(is_academic, na.rm = TRUE) * 100, 1),
      n_us = sum(is_us_based, na.rm = TRUE),
      pct_us = round(mean(is_us_based, na.rm = TRUE) * 100, 1),
      mean_authors = round(mean(author_count, na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    mutate(published = if_else(final_published, "Published", "Not Published"))
  write_csv(table1, table1_path)
  cli_alert_success("Table 1 saved (reduced form)")
}

# ============================================================
# Table 2: Publication Rate by Category
# ============================================================
cli_alert_info("Table 2: Publication Rate by Category/Subgroup")

# By RCT status
by_rct <- results |>
  filter(!is.na(final_published)) |>
  group_by(is_rct) |>
  summarise(
    subgroup = if_else(first(is_rct), "RCT", "Non-RCT"),
    n = n(), published = sum(final_published),
    rate = round(mean(final_published) * 100, 1),
    .groups = "drop"
  )

# By academic status
by_academic <- results |>
  filter(!is.na(final_published)) |>
  group_by(is_academic) |>
  summarise(
    subgroup = if_else(first(is_academic), "Academic", "Non-Academic"),
    n = n(), published = sum(final_published),
    rate = round(mean(final_published) * 100, 1),
    .groups = "drop"
  )

# By US status
by_us <- results |>
  filter(!is.na(final_published)) |>
  group_by(is_us_based) |>
  summarise(
    subgroup = if_else(first(is_us_based), "US-based", "International"),
    n = n(), published = sum(final_published),
    rate = round(mean(final_published) * 100, 1),
    .groups = "drop"
  )

table2 <- bind_rows(
  by_rct |> select(subgroup, n, published, rate),
  by_academic |> select(subgroup, n, published, rate),
  by_us |> select(subgroup, n, published, rate)
)

write_csv(table2, here("output", "tables", "table2_pub_rate_subgroups.csv"))
cli_alert_success("Table 2 saved")

# ============================================================
# Table 3: Logistic Regression Results
# ============================================================
cli_alert_info("Table 3: Predictors of Publication")

model_path <- here("output", "aim3_logistic_regression.csv")
if (file.exists(model_path)) {
  aim3_raw <- read_csv(model_path, show_col_types = FALSE)
}
if (file.exists(model_path) && all(c("conf.low", "conf.high", "estimate", "p.value") %in% names(aim3_raw))) {
  table3 <- aim3_raw |>
    mutate(
      or_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""
      )
    ) |>
    select(term, or_ci, p.value, significance)

  write_csv(table3, here("output", "tables", "table3_logistic_regression.csv"))
  cli_alert_success("Table 3 saved")
} else if (file.exists(model_path)) {
  cli_alert_warning("Table 3 skipped: aim3 regression had insufficient data")
}

# ============================================================
# Table 4: Search Strategy Performance
# ============================================================
cli_alert_info("Table 4: Search Strategy Performance")

strat_path <- here("output", "aim4_strategy_performance.csv")
if (file.exists(strat_path)) {
  table4 <- read_csv(strat_path, show_col_types = FALSE) |>
    arrange(desc(n_found_correct))

  write_csv(table4, here("output", "tables", "table4_search_strategies.csv"))
  cli_alert_success("Table 4 saved")
}

cli_alert_success("All tables generated in output/tables/")
