# 08_make_figures.R — Generate publication-quality figures

library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(cli)
library(config)

cfg <- config::get(file = here("config.yml"))

cli_h2("Generating Figures")

results <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)

# Determine final published status
if (file.exists(here("output", "manual_review_decisions.csv"))) {
  decisions <- read_csv(here("output", "manual_review_decisions.csv"), show_col_types = FALSE)
  results <- results |>
    left_join(decisions |> select(abstract_id, manual_decision), by = "abstract_id") |>
    mutate(final_published = case_when(
      classification == "accept" ~ TRUE,
      manual_decision == "match" ~ TRUE,
      manual_decision == "no_match" ~ FALSE,
      classification %in% c("reject", "no_candidates") ~ FALSE,
      TRUE ~ NA
    ))
} else {
  results <- results |>
    mutate(final_published = classification == "accept")
}

theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

# ============================================================
# Figure 1: CONSORT-style flow diagram data (text-based)
# ============================================================
cli_alert_info("Figure 1: Flow diagram data")

flow <- tibble::tibble(
  step = c("Total abstracts parsed",
           "After cleaning",
           "PubMed candidates found",
           "Auto-accepted",
           "Manual review",
           "Rejected / No match"),
  n = c(
    nrow(results),
    sum(!is.na(results$title)),
    sum(results$n_candidates > 0, na.rm = TRUE),
    sum(results$classification == "accept", na.rm = TRUE),
    sum(results$classification == "review", na.rm = TRUE),
    sum(results$classification %in% c("reject", "no_candidates"), na.rm = TRUE)
  )
)
write_csv(flow, here("output", "figures", "figure1_flow_data.csv"))

# ============================================================
# Figure 2: Time to publication histogram
# ============================================================
cli_alert_info("Figure 2: Time to publication")

published <- results |>
  filter(final_published & !is.na(months_to_pub) & months_to_pub > 0)

if (nrow(published) > 0) {
  fig2 <- ggplot(published, aes(x = months_to_pub)) +
    geom_histogram(binwidth = 2, fill = "#2166AC", color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = median(months_to_pub)), linetype = "dashed",
               color = "#B2182B", linewidth = 1) +
    labs(
      title = "Time from Conference Presentation to Full Publication",
      subtitle = sprintf("AAGL 2023 Global Congress (n = %d published)", nrow(published)),
      x = "Months to Publication",
      y = "Number of Abstracts"
    ) +
    theme_pub +
    annotate("text",
             x = median(published$months_to_pub) + 1,
             y = Inf, vjust = 2, hjust = 0,
             label = sprintf("Median: %.1f mo", median(published$months_to_pub)),
             color = "#B2182B", fontface = "italic")

  ggsave(here("output", "figures", "figure2_time_to_pub.png"), fig2,
         width = 8, height = 5, dpi = 300)
  ggsave(here("output", "figures", "figure2_time_to_pub.pdf"), fig2,
         width = 8, height = 5)
  cli_alert_success("Figure 2 saved")
}

# ============================================================
# Figure 3: Kaplan-Meier curve
# ============================================================
cli_alert_info("Figure 3: Kaplan-Meier curve")

km_path <- here("data", "processed", "km_fit.rds")
if (file.exists(km_path)) {
  km_fit <- readRDS(km_path)
  km_df <- tibble::tibble(
    time = km_fit$time,
    surv = 1 - km_fit$surv,  # Convert to cumulative publication
    lower = 1 - km_fit$upper,
    upper = 1 - km_fit$lower
  )

  fig3 <- ggplot(km_df, aes(x = time, y = surv)) +
    geom_step(linewidth = 1, color = "#2166AC") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#2166AC") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
      title = "Cumulative Publication Rate Over Time",
      subtitle = "AAGL 2023 Global Congress Oral Presentations",
      x = "Months Since Conference",
      y = "Cumulative Publication Rate"
    ) +
    theme_pub

  ggsave(here("output", "figures", "figure3_km_curve.png"), fig3,
         width = 8, height = 5, dpi = 300)
  ggsave(here("output", "figures", "figure3_km_curve.pdf"), fig3,
         width = 8, height = 5)
  cli_alert_success("Figure 3 saved")
}

# ============================================================
# Figure 4: Search strategy comparison (forest-plot style)
# ============================================================
cli_alert_info("Figure 4: Search strategy performance")

strat_path <- here("output", "aim4_strategy_performance.csv")
if (file.exists(strat_path)) {
  strat <- read_csv(strat_path, show_col_types = FALSE) |>
    filter(!is.na(n_found_correct)) |>
    mutate(strategy = factor(strategy, levels = strategy[order(n_found_correct)]))

  fig4 <- ggplot(strat, aes(x = strategy, y = n_found_correct)) +
    geom_col(fill = "#2166AC", alpha = 0.8) +
    geom_text(aes(label = sprintf("%d (%.0f%%)", n_found_correct, pct_found)),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(
      title = "Search Strategy Contribution to Correct Matches",
      subtitle = "Number of confirmed publications found by each strategy",
      x = "Search Strategy",
      y = "Number of Correct Matches Found"
    ) +
    theme_pub +
    theme(panel.grid.major.y = element_blank())

  ggsave(here("output", "figures", "figure4_strategy_perf.png"), fig4,
         width = 8, height = 5, dpi = 300)
  ggsave(here("output", "figures", "figure4_strategy_perf.pdf"), fig4,
         width = 8, height = 5)
  cli_alert_success("Figure 4 saved")
}

# ============================================================
# Figure 5: Score distribution
# ============================================================
cli_alert_info("Figure 5: Score distribution")

scores <- results |> filter(!is.na(best_score) & best_score > 0)
if (nrow(scores) > 0) {
  fig5 <- ggplot(scores, aes(x = best_score, fill = classification)) +
    geom_histogram(binwidth = 0.5, color = "white", alpha = 0.8) +
    scale_fill_manual(values = c(
      "accept" = "#1B9E77", "review" = "#D95F02", "reject" = "#7570B3"
    )) +
    geom_vline(xintercept = cfg$scoring$auto_accept, linetype = "dashed", color = "#1B9E77") +
    geom_vline(xintercept = cfg$scoring$manual_review, linetype = "dashed", color = "#D95F02") +
    labs(
      title = "Distribution of Best Match Scores",
      subtitle = "Dashed lines show auto-accept and manual review thresholds",
      x = "Match Score",
      y = "Count",
      fill = "Classification"
    ) +
    theme_pub

  ggsave(here("output", "figures", "figure5_score_dist.png"), fig5,
         width = 8, height = 5, dpi = 300)
  ggsave(here("output", "figures", "figure5_score_dist.pdf"), fig5,
         width = 8, height = 5)
  cli_alert_success("Figure 5 saved")
}

cli_alert_success("All figures generated in output/figures/")
