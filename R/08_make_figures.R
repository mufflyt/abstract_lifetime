# 08_make_figures.R -Generate publication-quality figures

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

theme_pub <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

congress_colors <- c(
  "2012" = "#E41A1C", "2013" = "#377EB8", "2014" = "#4DAF4A",
  "2015" = "#984EA3", "2016" = "#FF7F00", "2018" = "#A65628",
  "2019" = "#F781BF", "2020" = "#999999", "2021" = "#66C2A5",
  "2022" = "#FC8D62", "2023" = "#8DA0CB"
)

# ============================================================
# Figure 1: STROBE flow diagram (PRISMA-style for search process)
# ============================================================
cli_alert_info("Figure 1: Flow diagram")

n_video_excluded <- 48  # fixed from original scrape
n_scraped <- n_total + n_video_excluded
n_with_candidates <- sum(results$n_candidates > 0, na.rm = TRUE)
n_definite_fig <- sum(results$classification == "definite", na.rm = TRUE)
n_probable_fig <- sum(results$classification == "probable", na.rm = TRUE)
n_possible_fig <- sum(results$classification == "possible", na.rm = TRUE)
n_no_match_fig <- sum(results$classification %in% c("no_match", "no_candidates"), na.rm = TRUE)
n_excluded_fig <- sum(results$classification == "excluded", na.rm = TRUE)

flow <- tibble::tibble(
  step = c("Total abstracts parsed",
           "Video presentations excluded",
           "Oral abstracts included",
           "Searched across 6 databases",
           "With candidate publications",
           "Without any candidates",
           "Definite matches",
           "Probable (review needed)",
           "Possible (weak evidence)",
           "Excluded (pre-conference pub)",
           "No match"),
  n = c(n_scraped, n_video_excluded, n_total,
        n_total, n_with_candidates, n_total - n_with_candidates,
        n_definite_fig, n_probable_fig, n_possible_fig,
        n_excluded_fig, n_no_match_fig)
)
write_csv(flow, here("output", "figures", "figure1_flow_data.csv"))

# Render visual flow diagram
tryCatch({
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    grViz_code <- sprintf('
    digraph flow {
      graph [rankdir=TB, fontname="Arial", fontsize=12, nodesep=0.5, ranksep=0.6,
             bgcolor=white, splines=ortho]
      node [shape=box, style="filled", fontname="Arial", fontsize=11,
            width=3.2, height=0.7, penwidth=1.5, color="#333333"]
      edge [arrowsize=0.8, color="#333333", penwidth=1.2]

      // Top: Identification
      id [label="Abstracts identified from JMIG\\nsupplement issues, 2012-2023\\n(12 congresses)\\nn = %d",
          fillcolor="white"]

      // Exclusion (right side)
      ex [label="Excluded: video presentations\\nn = %d",
          fillcolor="white"]

      // Included
      inc [label="Oral presentations included\\nn = %d",
           fillcolor="white"]

      // Search
      search [label="Publication search\\n6 databases: PubMed, CrossRef,\\nEurope PMC, OpenAlex,\\nSemantic Scholar, DOI-chain\\nreverse citations",
              fillcolor="white"]

      // Candidates
      cand [label="Abstracts with candidate\\npublications identified\\nn = %d",
            fillcolor="white"]
      nocand [label="No candidates\\nidentified\\nn = %d",
              fillcolor="white"]

      // Scoring
      score [label="Composite scoring\\n(10 components: title, abstract,\\nauthor, journal, date matching)",
             fillcolor="white"]

      // Classification outcomes
      def [label="Definite match\\n(score >= 7, text evidence)\\nn = %d",
           fillcolor="#D5E8D4", color="#82B366"]
      prob [label="Probable match\\n(score 3-7, text evidence)\\nn = %d",
            fillcolor="#FFF2CC", color="#D6B656"]
      poss [label="Possible match\\n(weak evidence or ties)\\nn = %d",
            fillcolor="#FFE6CC", color="#D79B00"]
      nomatch [label="No match\\n(score < 3)\\nn = %d",
               fillcolor="#F8CECC", color="#B85450"]
      excl [label="Excluded\\n(pre-conference\\npublication)\\nn = %d",
            fillcolor="#E1D5E7", color="#9673A6"]

      // Layout
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

      // Force left-right arrangement
      {rank=same; id; ex}
      {rank=same; cand; nocand}
      {rank=same; def; prob; poss; nomatch; excl}
    }',
    n_scraped, n_video_excluded, n_total,
    n_with_candidates, n_total - n_with_candidates,
    n_definite_fig, n_probable_fig, n_possible_fig,
    n_no_match_fig, n_excluded_fig)

    g <- DiagrammeR::grViz(grViz_code)

    # Save as PNG via htmlwidgets + webshot
    html_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(g, html_file, selfcontained = TRUE)
    if (requireNamespace("webshot2", quietly = TRUE)) {
      webshot2::webshot(html_file,
                        here("output", "figures", "figure1_flow_diagram.png"),
                        vwidth = 1000, vheight = 900, zoom = 3)
      cli_alert_success("Figure 1 flow diagram saved (PNG)")
    } else if (requireNamespace("webshot", quietly = TRUE)) {
      webshot::webshot(html_file,
                        here("output", "figures", "figure1_flow_diagram.png"),
                        vwidth = 1000, vheight = 900, zoom = 3)
      cli_alert_success("Figure 1 flow diagram saved (PNG)")
    } else {
      cli_alert_warning("Install webshot2 for PNG rendering: install.packages('webshot2')")
    }
  } else {
    cli_alert_warning("Install DiagrammeR for flow diagram: install.packages('DiagrammeR')")
  }
}, error = function(e) {
  cli_alert_warning("Flow diagram rendering failed: {e$message}")
})

# ============================================================
# Figure 2: Time to publication histogram (all years, faceted)
# ============================================================
cli_alert_info("Figure 2: Time to publication")

published <- results |>
  filter(final_published & !is.na(months_to_pub) & months_to_pub > 0)

if (nrow(published) > 0) {
  med_ttp <- median(published$months_to_pub)

  fig2 <- ggplot(published, aes(x = months_to_pub)) +
    geom_histogram(binwidth = 3, fill = "#2166AC", color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = med_ttp), linetype = "dashed",
               color = "#B2182B", linewidth = 1) +
    labs(
      title = "Time from Conference Presentation to Full Publication",
      subtitle = sprintf("AAGL Global Congress %s (n = %d published across %d congresses)",
                         year_range, nrow(published), n_congresses),
      x = "Months to Publication",
      y = "Number of Abstracts"
    ) +
    theme_pub +
    annotate("text",
             x = med_ttp + 1,
             y = Inf, vjust = 2, hjust = 0,
             label = sprintf("Median: %.1f mo", med_ttp),
             color = "#B2182B", fontface = "italic")

  ggsave(here("output", "figures", "figure2_time_to_pub.png"), fig2,
         width = 8, height = 5, dpi = 300)
  ggsave(here("output", "figures", "figure2_time_to_pub.pdf"), fig2,
         width = 8, height = 5)
  cli_alert_success("Figure 2 saved")
}

# ============================================================
# Figure 3: Kaplan-Meier curve (pooled)
# ============================================================
cli_alert_info("Figure 3: Kaplan-Meier curve (pooled)")

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
  km_df <- tibble::tibble(
    time = km_fit$time,
    surv = 1 - km_fit$surv,
    lower = 1 - km_fit$upper,
    upper = 1 - km_fit$lower
  )

  fig3 <- ggplot(km_df, aes(x = time, y = surv)) +
    geom_step(linewidth = 1, color = "#2166AC") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#2166AC") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
      title = "Cumulative Publication Rate Over Time",
      subtitle = sprintf("AAGL %s -%d abstracts across %d congresses",
                         year_range, nrow(km_data), n_congresses),
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
# Figure 4: Search strategy comparison
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
      subtitle = sprintf("AAGL %s -%d abstracts", year_range, n_total),
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
# Figure 5: Score distribution by classification
# ============================================================
cli_alert_info("Figure 5: Score distribution")

scores <- results |> filter(!is.na(best_score) & best_score > 0)
if (nrow(scores) > 0) {
  fig5 <- ggplot(scores, aes(x = best_score, fill = classification)) +
    geom_histogram(binwidth = 0.5, color = "white", alpha = 0.8) +
    scale_fill_manual(values = c(
      "definite" = "#1B9E77", "probable" = "#D95F02", "possible" = "#E7298A",
      "no_match" = "#7570B3", "excluded" = "#666666", "no_candidates" = "#999999"
    )) +
    geom_vline(xintercept = cfg$scoring$auto_accept, linetype = "dashed", color = "#1B9E77") +
    geom_vline(xintercept = cfg$scoring$manual_review, linetype = "dashed", color = "#D95F02") +
    labs(
      title = "Distribution of Best Match Scores",
      subtitle = sprintf("AAGL %s -dashed lines show definite and probable thresholds",
                         year_range),
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

# ============================================================
# Figure 6: Publication rate by congress year (bar chart)
# ============================================================
cli_alert_info("Figure 6: Publication rate by congress year")

if ("congress_year" %in% names(results)) {
  year_rates <- results |>
    filter(!is.na(final_published)) |>
    group_by(congress_year) |>
    summarise(
      n = n(),
      n_published = sum(final_published),
      rate = mean(final_published) * 100,
      .groups = "drop"
    ) |>
    mutate(congress_year = factor(congress_year))

  fig6 <- ggplot(year_rates, aes(x = congress_year, y = rate,
                                  fill = congress_year)) +
    geom_col(alpha = 0.85, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.0f%%\n(%d/%d)", rate, n_published, n)),
              vjust = -0.3, size = 3) +
    scale_fill_manual(values = congress_colors) +
    scale_y_continuous(limits = c(0, max(year_rates$rate, na.rm = TRUE) * 1.3),
                       labels = scales::percent_format(scale = 1)) +
    labs(
      title = "Publication Rate by AAGL Congress Year",
      subtitle = sprintf("Definite + reviewer-confirmed matches (n = %d abstracts)", n_total),
      x = "Congress Year",
      y = "Publication Rate"
    ) +
    theme_pub

  ggsave(here("output", "figures", "figure6_pub_rate_by_year.png"), fig6,
         width = 10, height = 6, dpi = 300)
  ggsave(here("output", "figures", "figure6_pub_rate_by_year.pdf"), fig6,
         width = 10, height = 6)
  cli_alert_success("Figure 6 saved")
}

# ============================================================
# Figure 7: Kaplan-Meier curves stratified by congress year
# ============================================================
cli_alert_info("Figure 7: KM curves by congress year")

if (nrow(km_data) > 0 && "congress_year" %in% names(km_data)) {
  # Only include years with enough data
  year_counts <- km_data |> count(congress_year)
  viable_years <- year_counts$congress_year[year_counts$n >= 10]
  km_strat <- km_data |> filter(congress_year %in% viable_years)

  if (length(viable_years) >= 2) {
    km_fit_strat <- survfit(Surv(time, event) ~ congress_year, data = km_strat)

    # Extract per-stratum data
    strata_names <- names(km_fit_strat$strata)
    km_strat_df <- list()
    idx <- 0
    for (s in seq_along(strata_names)) {
      n_s <- km_fit_strat$strata[s]
      rows <- (idx + 1):(idx + n_s)
      yr <- gsub("congress_year=", "", strata_names[s])
      km_strat_df[[s]] <- tibble(
        time = km_fit_strat$time[rows],
        surv = 1 - km_fit_strat$surv[rows],
        congress_year = yr
      )
      idx <- idx + n_s
    }
    km_strat_df <- bind_rows(km_strat_df)

    fig7 <- ggplot(km_strat_df, aes(x = time, y = surv,
                                     color = congress_year)) +
      geom_step(linewidth = 0.8, alpha = 0.85) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      scale_color_manual(values = congress_colors) +
      labs(
        title = "Cumulative Publication Rate by Congress Year",
        subtitle = sprintf("AAGL %s -%d abstracts across %d congresses",
                           year_range, nrow(km_strat), length(viable_years)),
        x = "Months Since Conference",
        y = "Cumulative Publication Rate",
        color = "Congress Year"
      ) +
      theme_pub +
      theme(legend.position = "right")

    ggsave(here("output", "figures", "figure7_km_by_year.png"), fig7,
           width = 10, height = 6, dpi = 300)
    ggsave(here("output", "figures", "figure7_km_by_year.pdf"), fig7,
           width = 10, height = 6)
    cli_alert_success("Figure 7 saved")
  }
}

# ============================================================
# Figure 8: Time to publication by congress year (box plot)
# ============================================================
cli_alert_info("Figure 8: Time to publication by year")

if (nrow(published) > 0 && "congress_year" %in% names(published)) {
  pub_by_year <- published |>
    filter(!is.na(congress_year)) |>
    mutate(congress_year = factor(congress_year))

  if (nrow(pub_by_year) >= 5) {
    fig8 <- ggplot(pub_by_year, aes(x = congress_year, y = months_to_pub,
                                     fill = congress_year)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 21, show.legend = FALSE) +
      geom_jitter(width = 0.15, alpha = 0.3, size = 1.2) +
      scale_fill_manual(values = congress_colors) +
      labs(
        title = "Time to Publication by Congress Year",
        subtitle = sprintf("AAGL %s -%d published abstracts", year_range, nrow(pub_by_year)),
        x = "Congress Year",
        y = "Months to Publication"
      ) +
      theme_pub

    ggsave(here("output", "figures", "figure8_ttp_by_year.png"), fig8,
           width = 10, height = 6, dpi = 300)
    ggsave(here("output", "figures", "figure8_ttp_by_year.pdf"), fig8,
           width = 10, height = 6)
    cli_alert_success("Figure 8 saved")
  }
}

# ============================================================
# Figure 9: Classification breakdown by congress year (stacked bar)
# ============================================================
cli_alert_info("Figure 9: Classification by year")

if ("congress_year" %in% names(results)) {
  class_by_year <- results |>
    filter(!is.na(congress_year), !is.na(classification)) |>
    count(congress_year, classification) |>
    mutate(
      congress_year = factor(congress_year),
      classification = factor(classification,
        levels = c("definite", "probable", "possible", "no_match", "excluded", "no_candidates"))
    )

  fig9 <- ggplot(class_by_year, aes(x = congress_year, y = n,
                                     fill = classification)) +
    geom_col(position = "fill", alpha = 0.85) +
    scale_fill_manual(values = c(
      "definite" = "#1B9E77", "probable" = "#D95F02", "possible" = "#E7298A",
      "no_match" = "#7570B3", "excluded" = "#666666", "no_candidates" = "#999999"
    )) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Match Classification Breakdown by Congress Year",
      subtitle = sprintf("AAGL %s -%d abstracts", year_range, n_total),
      x = "Congress Year",
      y = "Proportion",
      fill = "Classification"
    ) +
    theme_pub

  ggsave(here("output", "figures", "figure9_class_by_year.png"), fig9,
         width = 10, height = 6, dpi = 300)
  ggsave(here("output", "figures", "figure9_class_by_year.pdf"), fig9,
         width = 10, height = 6)
  cli_alert_success("Figure 9 saved")
}

# ============================================================
# Figure 10: Forest plot of Cox proportional hazards ratios
# ============================================================
cli_alert_info("Figure 10: Cox PH forest plot")

cox_path <- here("output", "aim2b_cox_regression.csv")
if (file.exists(cox_path)) {
  cox <- read_csv(cox_path, show_col_types = FALSE) |>
    filter(term != "(Intercept)") |>
    mutate(
      # Clean up term names for display
      label = case_when(
        term == "is_rctTRUE" ~ "RCT design",
        term == "log_sample_size" ~ "Sample size (log)",
        term == "is_academicTRUE" ~ "Academic affiliation",
        term == "is_us_basedTRUE" ~ "US-based",
        term == "session_typeVideo" ~ "Video session",
        term == "n_authors" ~ "Number of authors",
        term == "first_author_gendermale" ~ "Male first author",
        term == "practice_typecommunity" ~ "Community (vs Academic)",
        term == "practice_typemilitary_va" ~ "Military/VA",
        term == "practice_typeprivate_practice" ~ "Private practice",
        term == "practice_typeresearch_institute" ~ "Research institute",
        term == "is_multicenterTRUE" ~ "Multicenter study",
        term == "has_fundingTRUE" ~ "Funding reported",
        TRUE ~ gsub("TRUE$", "", gsub("_", " ", term))
      ),
      label = factor(label, levels = rev(label)),
      significant = p.value < 0.05
    ) |>
    filter(!is.na(estimate), estimate > 0, !is.na(conf.low))

  if (nrow(cox) > 0) {
    fig10 <- ggplot(cox, aes(x = estimate, y = label)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      geom_errorbarh(aes(xmin = conf.low, xmax = pmin(conf.high, 15)),
                     height = 0.25, linewidth = 0.6) +
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
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = c(0.85, 0.15)
      )

    ggsave(here("output", "figures", "figure10_cox_forest.png"), fig10,
           width = 9, height = 6, dpi = 300)
    ggsave(here("output", "figures", "figure10_cox_forest.pdf"), fig10,
           width = 9, height = 6)
    cli_alert_success("Figure 10 saved")
  }
}

cli_alert_success("All figures generated in output/figures/")
