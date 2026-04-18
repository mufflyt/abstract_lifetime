# 07_make_tables.R — Generate publication-quality tables

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(cli)
library(config)

cfg <- config::get(file = here("config.yml"))

dir.create(here("output", "tables"), showWarnings = FALSE, recursive = TRUE)

cli_h2("Generating Tables")

results <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)

# Determine final published status
if (file.exists(here("output", "manual_review_decisions.csv"))) {
  decisions <- read_csv(here("output", "manual_review_decisions.csv"), show_col_types = FALSE)
  decisions <- decisions |>
    filter(!is.na(reviewer)) |>
    group_by(abstract_id) |>
    arrange(desc(review_timestamp)) |>
    slice(1) |>
    ungroup()
  results <- results |>
    left_join(decisions |> select(abstract_id, manual_decision), by = "abstract_id") |>
    mutate(final_published = case_when(
      classification == "definite" ~ TRUE,
      manual_decision == "match" ~ TRUE,
      manual_decision == "no_match" ~ FALSE,
      classification %in% c("no_match", "no_candidates", "excluded") ~ FALSE,
      TRUE ~ NA
    ))
} else {
  results <- results |>
    mutate(final_published = classification == "definite")
}

# ============================================================
# Table 1: Abstract Characteristics
# ============================================================
cli_alert_info("Table 1: Abstract Characteristics")

table1 <- results |>
  filter(!is.na(final_published)) |>
  group_by(final_published) |>
  summarise(
    n = n(),
    n_rct = sum(is_rct, na.rm = TRUE),
    pct_rct = round(mean(is_rct, na.rm = TRUE) * 100, 1),
    median_sample_size = median(sample_size, na.rm = TRUE),
    iqr_sample_size = paste0(
      round(quantile(sample_size, 0.25, na.rm = TRUE)),
      "-",
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

write_csv(table1, here("output", "tables", "table1_characteristics.csv"))
cli_alert_success("Table 1 saved")

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
