# 08_make_figures.R - Generate publication-quality figures
# Main manuscript: Figures 1-6
# Supplementary: Figures S1-S4 (matching strategy details)

library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(cli)
library(config)
library(survival)
library(tidyr)
source(here("R", "utils_congresses.R"))

cfg <- config::get(file = here("config.yml"))

dir.create(here("output", "figures"), showWarnings = FALSE, recursive = TRUE)

cli_h2("Generating Figures")

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

n_congresses <- length(unique(results$congress_year))
year_range <- paste0(min(results$congress_year, na.rm = TRUE), "-",
                     max(results$congress_year, na.rm = TRUE))
n_total <- nrow(results)
n_probable <- sum(results$classification == "probable", na.rm = TRUE)

theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

congress_colors <- c(
  "2012" = "#E41A1C", "2013" = "#377EB8", "2014" = "#4DAF4A",
  "2015" = "#984EA3", "2016" = "#FF7F00", "2017" = "#A6761D",
  "2018" = "#A65628", "2019" = "#F781BF", "2020" = "#999999",
  "2021" = "#66C2A5", "2022" = "#FC8D62", "2023" = "#8DA0CB"
)

# ============================================================
# MAIN FIGURE 1: STROBE flow diagram
# ============================================================
cli_alert_info("Figure 1: Flow diagram")

n_video_excluded <- 48
n_scraped <- n_total + n_video_excluded
n_with_candidates <- sum(results$n_candidates > 0, na.rm = TRUE)
n_definite_fig <- sum(results$classification == "definite", na.rm = TRUE)
n_probable_fig <- sum(results$classification == "probable", na.rm = TRUE)
n_possible_fig <- sum(results$classification == "possible", na.rm = TRUE)
n_no_match_fig <- sum(results$classification %in% c("no_match", "no_candidates"), na.rm = TRUE)
n_excluded_fig <- sum(results$classification == "excluded", na.rm = TRUE)

flow <- tibble::tibble(
  step = c("Total abstracts parsed", "Video excluded", "Oral included",
           "Searched", "With candidates", "No candidates",
           "Definite", "Probable", "Possible", "Excluded", "No match"),
  n = c(n_scraped, n_video_excluded, n_total, n_total,
        n_with_candidates, n_total - n_with_candidates,
        n_definite_fig, n_probable_fig, n_possible_fig,
        n_excluded_fig, n_no_match_fig)
)
write_csv(flow, here("output", "figures", "figure1_flow_data.csv"))

tryCatch({
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    grViz_code <- sprintf('
    digraph flow {
      graph [rankdir=TB, fontname="Arial", fontsize=12, nodesep=0.5, ranksep=0.6,
             bgcolor=white, splines=ortho]
      node [shape=box, style="filled", fontname="Arial", fontsize=11,
            width=3.2, height=0.7, penwidth=1.5, color="#333333"]
      edge [arrowsize=0.8, color="#333333", penwidth=1.2]

      id [label="Abstracts identified from JMIG\\nsupplement issues, 2012-2023\\n(12 congresses)\\nn = %d", fillcolor="white"]
      ex [label="Excluded: video presentations\\nn = %d", fillcolor="white"]
      inc [label="Oral presentations included\\nn = %d", fillcolor="white"]
      search [label="Publication search\\n6 databases: PubMed, CrossRef,\\nEurope PMC, OpenAlex,\\nSemantic Scholar, DOI-chain\\nreverse citations", fillcolor="white"]
      cand [label="Abstracts with candidate\\npublications identified\\nn = %d", fillcolor="white"]
      nocand [label="No candidates\\nidentified\\nn = %d", fillcolor="white"]
      score [label="Composite scoring\\n(10 components: title, abstract,\\nauthor, journal, date matching)", fillcolor="white"]
      def [label="Definite match\\n(score >= 7, text evidence)\\nn = %d", fillcolor="#D5E8D4", color="#82B366"]
      prob [label="Probable match\\n(score 3-7, text evidence)\\nn = %d", fillcolor="#FFF2CC", color="#D6B656"]
      poss [label="Possible match\\n(weak evidence or ties)\\nn = %d", fillcolor="#FFE6CC", color="#D79B00"]
      nomatch [label="No match\\n(score < 3)\\nn = %d", fillcolor="#F8CECC", color="#B85450"]
      excl [label="Excluded\\n(pre-conference\\npublication)\\nn = %d", fillcolor="#E1D5E7", color="#9673A6"]

      id -> inc
      id -> ex [style=dashed, constraint=false]
      inc -> search
      search -> cand
      search -> nocand [style=dashed, constraint=false]
      cand -> score
      score -> def
      score -> prob
      score -> poss
      score -> nomatch
      score -> excl [style=dashed]

      {rank=same; id; ex}
      {rank=same; cand; nocand}
      {rank=same; def; prob; poss; nomatch; excl}
    }',
    n_scraped, n_video_excluded, n_total,
    n_with_candidates, n_total - n_with_candidates,
    n_definite_fig, n_probable_fig, n_possible_fig,
    n_no_match_fig, n_excluded_fig)

    g <- DiagrammeR::grViz(grViz_code)
    html_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(g, html_file, selfcontained = TRUE)
    if (requireNamespace("webshot2", quietly = TRUE)) {
      webshot2::webshot(html_file, here("output", "figures", "figure1_flow_diagram.png"),
                        vwidth = 1000, vheight = 900, zoom = 3)
      cli_alert_success("Figure 1 saved")
    }
  }
}, error = function(e) cli_alert_warning("Flow diagram failed: {e$message}"))

# ============================================================
# MAIN FIGURE 2: Kaplan-Meier cumulative publication curve (pooled)
# ============================================================
cli_alert_info("Figure 2: KM curve (pooled)")

km_data <- results |>
  filter(!is.na(final_published)) |>
  mutate(
    time = if_else(final_published & !is.na(months_to_pub),
                   months_to_pub,
                   as.numeric(difftime(as.Date(cfg$pubmed$date_end, "%Y/%m/%d"),
                                       conference_date_for(congress_year, cfg),
                                       units = "days")) / 30.44),
    event = as.integer(final_published)
  ) |>
  filter(time > 0)

if (nrow(km_data) > 0) {
  km_fit <- survfit(Surv(time, event) ~ 1, data = km_data)
  km_df <- tibble(time = km_fit$time, surv = 1 - km_fit$surv,
                  lower = 1 - km_fit$upper, upper = 1 - km_fit$lower)

  last_event_time <- max(km_df$time[c(TRUE, diff(km_df$surv) > 0)], na.rm = TRUE)
  x_max <- min(last_event_time + 12, max(km_df$time))
  km_df_trunc <- km_df |> filter(time <= x_max)

  fig2 <- ggplot(km_df_trunc, aes(x = time, y = surv)) +
    geom_step(linewidth = 1, color = "#2166AC") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#2166AC") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, 120, by = 12)) +
    labs(
      title = "Cumulative Publication Rate Over Time",
      subtitle = sprintf("AAGL %s - %d definite matches; %d probable pending review",
                         year_range, sum(km_data$event == 1), n_probable),
      x = "Months Since Conference",
      y = "Cumulative Publication Rate"
    ) +
    theme_pub

  ggsave(here("output", "figures", "figure2_km_curve.png"), fig2,
         width = 8, height = 5, dpi = 300)
  ggsave(here("output", "figures", "figure2_km_curve.pdf"), fig2,
         width = 8, height = 5)
  cli_alert_success("Figure 2 saved")
}

# ============================================================
# MAIN FIGURE 3: KM curves stratified by congress year
# ============================================================
cli_alert_info("Figure 3: KM curves by congress year")

if (nrow(km_data) > 0 && "congress_year" %in% names(km_data)) {
  year_counts <- km_data |> count(congress_year)
  viable_years <- year_counts$congress_year[year_counts$n >= 10]
  km_strat <- km_data |> filter(congress_year %in% viable_years)

  if (length(viable_years) >= 2) {
    km_fit_strat <- survfit(Surv(time, event) ~ congress_year, data = km_strat)
    strata_names <- names(km_fit_strat$strata)
    km_strat_df <- list()
    idx <- 0
    for (s_i in seq_along(strata_names)) {
      n_s <- km_fit_strat$strata[s_i]
      rows <- (idx + 1):(idx + n_s)
      yr <- gsub("congress_year=", "", strata_names[s_i])
      km_strat_df[[s_i]] <- tibble(time = km_fit_strat$time[rows],
                                    surv = 1 - km_fit_strat$surv[rows],
                                    congress_year = yr)
      idx <- idx + n_s
    }
    km_strat_df <- bind_rows(km_strat_df)

    # Truncate
    km_strat_df <- km_strat_df |> filter(time <= x_max)

    fig3 <- ggplot(km_strat_df, aes(x = time, y = surv, color = congress_year)) +
      geom_step(linewidth = 0.8, alpha = 0.85) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
      scale_x_continuous(breaks = seq(0, 120, by = 12)) +
      scale_color_manual(values = congress_colors) +
      labs(
        title = "Cumulative Publication Rate by Congress Year",
        subtitle = sprintf("AAGL %s - %d abstracts across %d congresses",
                           year_range, nrow(km_strat), length(viable_years)),
        x = "Months Since Conference",
        y = "Cumulative Publication Rate",
        color = "Congress Year"
      ) +
      theme_pub +
      theme(legend.position = "right")

    ggsave(here("output", "figures", "figure3_km_by_year.png"), fig3,
           width = 10, height = 6, dpi = 300)
    ggsave(here("output", "figures", "figure3_km_by_year.pdf"), fig3,
           width = 10, height = 6)
    cli_alert_success("Figure 3 saved")
  }
}

# ============================================================
# MAIN FIGURE 4: Publication rate by subgroup (combined panel)
# ============================================================
cli_alert_info("Figure 4: Publication rate by subgroup")

subgroup_data <- list()

# By study design (top 6)
if ("study_design" %in% names(results)) {
  sd <- results |>
    filter(!is.na(final_published)) |>
    group_by(study_design) |>
    summarise(n = n(), rate = mean(final_published) * 100, .groups = "drop") |>
    filter(n >= 10) |>
    arrange(desc(rate)) |>
    head(6) |>
    mutate(category = "Study Design", subgroup = study_design)
  subgroup_data <- c(subgroup_data, list(sd))
}

# By practice type
if ("practice_type" %in% names(results)) {
  pt <- results |>
    filter(!is.na(final_published), !is.na(practice_type)) |>
    group_by(practice_type) |>
    summarise(n = n(), rate = mean(final_published) * 100, .groups = "drop") |>
    filter(n >= 5) |>
    arrange(desc(rate)) |>
    mutate(category = "Practice Type", subgroup = practice_type)
  subgroup_data <- c(subgroup_data, list(pt))
}

# By subspecialty (top 6)
if ("subspecialty" %in% names(results)) {
  sp <- results |>
    filter(!is.na(final_published), !is.na(subspecialty)) |>
    group_by(subspecialty) |>
    summarise(n = n(), rate = mean(final_published) * 100, .groups = "drop") |>
    filter(n >= 5) |>
    arrange(desc(rate)) |>
    head(6) |>
    mutate(category = "Subspecialty", subgroup = subspecialty)
  subgroup_data <- c(subgroup_data, list(sp))
}

# By US vs international
by_us <- results |>
  filter(!is.na(final_published)) |>
  group_by(is_us_based) |>
  summarise(n = n(), rate = mean(final_published) * 100, .groups = "drop") |>
  mutate(category = "Geography",
         subgroup = if_else(is_us_based, "US-based", "International"))
subgroup_data <- c(subgroup_data, list(by_us))

# By gender
if ("first_author_gender" %in% names(results)) {
  by_gen <- results |>
    filter(!is.na(final_published), !is.na(first_author_gender)) |>
    group_by(first_author_gender) |>
    summarise(n = n(), rate = mean(final_published) * 100, .groups = "drop") |>
    mutate(category = "First Author Gender", subgroup = first_author_gender)
  subgroup_data <- c(subgroup_data, list(by_gen))
}

if (length(subgroup_data) > 0) {
  all_sub <- bind_rows(subgroup_data) |>
    mutate(
      label = paste0(subgroup, " (n=", n, ")"),
      category = factor(category, levels = unique(category))
    )

  fig4 <- ggplot(all_sub, aes(x = reorder(label, rate), y = rate)) +
    geom_col(fill = "#2166AC", alpha = 0.8, width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", rate)), hjust = -0.1, size = 3) +
    coord_flip() +
    facet_wrap(~ category, scales = "free_y", ncol = 1) +
    labs(
      title = "Publication Rate by Subgroup",
      subtitle = sprintf("AAGL %s - definite + reviewer-confirmed matches", year_range),
      x = "",
      y = "Publication Rate (%)"
    ) +
    theme_pub +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      panel.grid.major.y = element_blank()
    )

  ggsave(here("output", "figures", "figure4_subgroup_rates.png"), fig4,
         width = 10, height = 12, dpi = 300)
  ggsave(here("output", "figures", "figure4_subgroup_rates.pdf"), fig4,
         width = 10, height = 12)
  cli_alert_success("Figure 4 saved")
}

# ============================================================
# MAIN FIGURE 5: Cox PH forest plot
# ============================================================
cli_alert_info("Figure 5: Cox PH forest plot")

cox_path <- here("output", "aim2b_cox_regression.csv")
if (file.exists(cox_path)) {
  cox <- read_csv(cox_path, show_col_types = FALSE) |>
    filter(term != "(Intercept)") |>
    mutate(
      label = case_when(
        term == "is_rctTRUE" ~ "RCT design",
        term == "log_sample_size" ~ "Sample size (log)",
        term == "is_academicTRUE" ~ "Academic affiliation",
        term == "is_us_basedTRUE" ~ "US-based",
        term == "n_authors" ~ "Number of authors",
        term == "first_author_gendermale" ~ "Male first author",
        term == "practice_typecommunity" ~ "Community (vs Academic)",
        term == "practice_typemilitary_va" ~ "Military/VA (vs Academic)",
        term == "practice_typeprivate_practice" ~ "Private practice (vs Academic)",
        term == "practice_typeresearch_institute" ~ "Research institute (vs Academic)",
        term == "is_multicenterTRUE" ~ "Multicenter study",
        term == "has_fundingTRUE" ~ "Funding reported",
        TRUE ~ gsub("TRUE$", "", gsub("_", " ", term))
      ),
      label = factor(label, levels = rev(label)),
      significant = p.value < 0.05
    ) |>
    filter(!is.na(estimate), estimate > 0, !is.na(conf.low))

  if (nrow(cox) > 0) {
    fig5 <- ggplot(cox, aes(x = estimate, y = label)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      geom_errorbar(aes(xmin = conf.low, xmax = pmin(conf.high, 15)),
                    width = 0.25, linewidth = 0.6, orientation = "y") +
      geom_point(aes(color = significant), size = 3) +
      scale_color_manual(values = c("TRUE" = "#D32F2F", "FALSE" = "#333333"),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "NS"),
                         name = "") +
      scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8)) +
      labs(
        title = "Predictors of Time to Publication",
        subtitle = "Cox Proportional Hazards Model - Hazard Ratios with 95% CI",
        x = "Hazard Ratio (log scale)",
        y = ""
      ) +
      theme_pub +
      theme(panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.justification = "center")

    ggsave(here("output", "figures", "figure5_cox_forest.png"), fig5,
           width = 9, height = 6, dpi = 300)
    ggsave(here("output", "figures", "figure5_cox_forest.pdf"), fig5,
           width = 9, height = 6)
    cli_alert_success("Figure 5 saved")
  }
}

# ============================================================
# MAIN FIGURE 6: Time to publication histogram
# ============================================================
cli_alert_info("Figure 6: Time to publication histogram")

published <- results |>
  filter(final_published & !is.na(months_to_pub) & months_to_pub > 0)

if (nrow(published) > 0) {
  med_ttp <- median(published$months_to_pub)

  fig6 <- ggplot(published, aes(x = months_to_pub)) +
    geom_histogram(binwidth = 3, fill = "#2166AC", color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = med_ttp), linetype = "dashed",
               color = "#B2182B", linewidth = 1) +
    labs(
      title = "Time from Conference Presentation to Full Publication",
      subtitle = sprintf("AAGL %s (n = %d definite matches; %d probable pending review)",
                         year_range, nrow(published), n_probable),
      x = "Months to Publication",
      y = "Number of Abstracts"
    ) +
    theme_pub +
    annotate("text", x = med_ttp + 1, y = Inf, vjust = 2, hjust = 0,
             label = sprintf("Median: %.1f mo", med_ttp),
             color = "#B2182B", fontface = "italic")

  ggsave(here("output", "figures", "figure6_time_to_pub.png"), fig6,
         width = 8, height = 5, dpi = 300)
  ggsave(here("output", "figures", "figure6_time_to_pub.pdf"), fig6,
         width = 8, height = 5)
  cli_alert_success("Figure 6 saved")
}

# ============================================================
# SUPPLEMENTARY FIGURES
# ============================================================
cli_h3("Supplementary Figures")

# S1: Publication rate by congress year
cli_alert_info("Figure S1: Publication rate by year")
if ("congress_year" %in% names(results)) {
  year_rates <- results |>
    filter(!is.na(final_published)) |>
    group_by(congress_year) |>
    summarise(n = n(), n_published = sum(final_published),
              rate = mean(final_published) * 100, .groups = "drop") |>
    mutate(congress_year = factor(congress_year))

  figS1 <- ggplot(year_rates, aes(x = congress_year, y = rate, fill = congress_year)) +
    geom_col(alpha = 0.85, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.0f%%\n(%d/%d)", rate, n_published, n)),
              vjust = -0.3, size = 3) +
    scale_fill_manual(values = congress_colors) +
    scale_y_continuous(limits = c(0, max(year_rates$rate, na.rm = TRUE) * 1.3),
                       labels = scales::percent_format(scale = 1)) +
    labs(title = "Publication Rate by AAGL Congress Year",
         subtitle = sprintf("Definite + reviewer-confirmed matches (n = %d)", n_total),
         x = "Congress Year", y = "Publication Rate") +
    theme_pub

  ggsave(here("output", "figures", "figureS1_pub_rate_by_year.png"), figS1,
         width = 10, height = 6, dpi = 300)
  ggsave(here("output", "figures", "figureS1_pub_rate_by_year.pdf"), figS1,
         width = 10, height = 6)
  cli_alert_success("Figure S1 saved")
}

# S2: Search strategy comparison
cli_alert_info("Figure S2: Search strategy performance")
strat_path <- here("output", "aim4_strategy_performance.csv")
if (file.exists(strat_path)) {
  strat <- read_csv(strat_path, show_col_types = FALSE) |>
    filter(!is.na(n_found_correct)) |>
    mutate(strategy = factor(strategy, levels = strategy[order(n_found_correct)]))

  figS2 <- ggplot(strat, aes(x = strategy, y = n_found_correct)) +
    geom_col(fill = "#2166AC", alpha = 0.8) +
    geom_text(aes(label = sprintf("%d (%.0f%%)", n_found_correct, pct_found)),
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = "Search Strategy Contribution to Correct Matches",
         x = "Search Strategy", y = "Number of Correct Matches Found") +
    theme_pub + theme(panel.grid.major.y = element_blank())

  ggsave(here("output", "figures", "figureS2_strategy_perf.png"), figS2,
         width = 8, height = 5, dpi = 300)
  ggsave(here("output", "figures", "figureS2_strategy_perf.pdf"), figS2,
         width = 8, height = 5)
  cli_alert_success("Figure S2 saved")
}

# S3: Score distribution
cli_alert_info("Figure S3: Score distribution")
scores <- results |> filter(!is.na(best_score) & best_score > 0)
if (nrow(scores) > 0) {
  figS3 <- ggplot(scores, aes(x = best_score, fill = classification)) +
    geom_histogram(binwidth = 0.5, color = "white", alpha = 0.8) +
    scale_fill_manual(values = c(
      "definite" = "#1B9E77", "probable" = "#D95F02", "possible" = "#E7298A",
      "no_match" = "#7570B3", "excluded" = "#666666", "no_candidates" = "#999999"
    )) +
    geom_vline(xintercept = cfg$scoring$auto_accept, linetype = "dashed", color = "#1B9E77") +
    geom_vline(xintercept = cfg$scoring$manual_review, linetype = "dashed", color = "#D95F02") +
    labs(title = "Distribution of Best Match Scores",
         subtitle = "Dashed lines show definite and probable thresholds",
         x = "Match Score", y = "Count", fill = "Classification") +
    theme_pub

  ggsave(here("output", "figures", "figureS3_score_dist.png"), figS3,
         width = 8, height = 5, dpi = 300)
  ggsave(here("output", "figures", "figureS3_score_dist.pdf"), figS3,
         width = 8, height = 5)
  cli_alert_success("Figure S3 saved")
}

# S4: Classification by year
cli_alert_info("Figure S4: Classification by year")
if ("congress_year" %in% names(results)) {
  class_by_year <- results |>
    filter(!is.na(congress_year), !is.na(classification)) |>
    count(congress_year, classification) |>
    mutate(congress_year = factor(congress_year),
           classification = factor(classification,
             levels = c("definite", "probable", "possible", "no_match", "excluded", "no_candidates")))

  figS4 <- ggplot(class_by_year, aes(x = congress_year, y = n, fill = classification)) +
    geom_col(position = "fill", alpha = 0.85) +
    scale_fill_manual(values = c(
      "definite" = "#1B9E77", "probable" = "#D95F02", "possible" = "#E7298A",
      "no_match" = "#7570B3", "excluded" = "#666666", "no_candidates" = "#999999"
    )) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Match Classification Breakdown by Congress Year",
         x = "Congress Year", y = "Proportion", fill = "Classification") +
    theme_pub

  ggsave(here("output", "figures", "figureS4_class_by_year.png"), figS4,
         width = 10, height = 6, dpi = 300)
  ggsave(here("output", "figures", "figureS4_class_by_year.pdf"), figS4,
         width = 10, height = 6)
  cli_alert_success("Figure S4 saved")
}

cli_alert_success("All figures generated in output/figures/")
