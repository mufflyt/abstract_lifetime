# 06_analyze_results.R — Statistical analysis for all four research aims

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(survival)
library(broom)
library(cli)
library(config)
source(here("R", "utils_congresses.R"))

cfg <- config::get(file = here("config.yml"))

cli_h2("Statistical Analysis")

# Load adjudicated results (incorporate manual review if available)
results <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)

# Check for manual review decisions
manual_review_path <- here("output", "manual_review_decisions.csv")
if (file.exists(manual_review_path)) {
  decisions <- read_csv(manual_review_path, show_col_types = FALSE)
  cli_alert_info("Incorporating {nrow(decisions)} manual review decisions")

  results <- results |>
    left_join(decisions |> select(abstract_id, manual_decision, manual_pmid),
              by = "abstract_id") |>
    mutate(
      final_published = case_when(
        classification == "accept" ~ TRUE,
        manual_decision == "match" ~ TRUE,
        manual_decision == "no_match" ~ FALSE,
        classification == "reject" ~ FALSE,
        classification == "no_candidates" ~ FALSE,
        TRUE ~ NA  # Still pending review
      ),
      final_pmid = coalesce(manual_pmid, best_pmid)
    )
} else {
  cli_alert_warning("No manual review decisions found — using auto-classification only")
  results <- results |>
    mutate(
      final_published = classification == "accept",
      final_pmid = best_pmid
    )
}

# ============================================================
# AIM 1: Publication rate
# ============================================================
cli_h3("Aim 1: Publication Rate")

n_total <- nrow(results)
n_published <- sum(results$final_published, na.rm = TRUE)
n_pending <- sum(is.na(results$final_published))

# Wilson score interval for proportion
prop_test <- prop.test(n_published, n_total - n_pending, correct = FALSE)
pub_rate <- n_published / (n_total - n_pending)

aim1 <- tibble::tibble(
  metric = c("total_abstracts", "published", "not_published", "pending_review",
             "publication_rate", "ci_lower", "ci_upper"),
  value = c(n_total, n_published, n_total - n_pending - n_published, n_pending,
            round(pub_rate * 100, 1),
            round(prop_test$conf.int[1] * 100, 1),
            round(prop_test$conf.int[2] * 100, 1))
)

cli_alert_info("Publication rate: {round(pub_rate*100,1)}% ({n_published}/{n_total - n_pending})")
cli_alert_info("95% CI: [{round(prop_test$conf.int[1]*100,1)}%, {round(prop_test$conf.int[2]*100,1)}%]")

# By category (if available)
if ("category" %in% names(results)) {
  aim1_by_cat <- results |>
    filter(!is.na(final_published)) |>
    group_by(category) |>
    summarise(
      n = n(),
      n_published = sum(final_published),
      rate = round(mean(final_published) * 100, 1),
      .groups = "drop"
    )
  write_csv(aim1_by_cat, here("output", "aim1_by_category.csv"))
}

# ============================================================
# AIM 2: Time to publication
# ============================================================
cli_h3("Aim 2: Time to Publication")

published <- results |> filter(final_published)

if (nrow(published) > 0 && "months_to_pub" %in% names(published)) {
  ttp <- published$months_to_pub[!is.na(published$months_to_pub)]

  aim2 <- tibble::tibble(
    metric = c("n_with_dates", "median_months", "q1_months", "q3_months",
               "mean_months", "min_months", "max_months"),
    value = c(length(ttp),
              round(median(ttp), 1), round(quantile(ttp, 0.25), 1),
              round(quantile(ttp, 0.75), 1), round(mean(ttp), 1),
              round(min(ttp), 1), round(max(ttp), 1))
  )

  cli_alert_info("Median time to publication: {round(median(ttp), 1)} months (IQR: {round(quantile(ttp, 0.25), 1)}-{round(quantile(ttp, 0.75), 1)})")

  # Kaplan-Meier analysis
  # Create survival object: event = publication, time = months since conference
  # Censored = not published by end of search window
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
    km_summary <- summary(km_fit)
    saveRDS(km_fit, here("data", "processed", "km_fit.rds"))
    cli_alert_success("Kaplan-Meier model saved")
  }
} else {
  cli_alert_warning("No published articles with dates to analyze")
  aim2 <- tibble::tibble(metric = "n_with_dates", value = 0)
}

# ============================================================
# AIM 3: Predictors of publication
# ============================================================
cli_h3("Aim 3: Predictors of Publication")

model_data <- results |>
  filter(!is.na(final_published)) |>
  mutate(
    published_int = as.integer(final_published),
    log_sample_size = log1p(coalesce(sample_size, 0))
  )

if (nrow(model_data) >= 20 && length(unique(model_data$published_int)) >= 2) {
  # Logistic regression
  model <- glm(
    published_int ~ is_rct + log_sample_size + is_academic + is_us_based,
    data = model_data,
    family = binomial(link = "logit")
  )

  model_tidy <- tryCatch(
    tidy(model, exponentiate = TRUE, conf.int = TRUE),
    error = function(e) {
      cli_alert_warning("Confidence intervals failed ({conditionMessage(e)}); returning point estimates only")
      tidy(model, exponentiate = TRUE, conf.int = FALSE)
    }
  )
  aim3 <- model_tidy |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  write_csv(aim3, here("output", "aim3_logistic_regression.csv"))
  saveRDS(model, here("data", "processed", "logistic_model.rds"))

  cli_alert_success("Logistic regression complete")
  print(aim3)
} else {
  cli_alert_warning("Insufficient data or no outcome variation for logistic regression (n = {nrow(model_data)}, unique outcomes = {length(unique(model_data$published_int))})")
  aim3 <- tibble::tibble(term = "insufficient_data", estimate = NA)
}

# ============================================================
# AIM 4: Search strategy performance
# ============================================================
cli_h3("Aim 4: Search Strategy Performance")

# For accepted matches, which strategies/sources found the winning PMID?
accepted_pmids <- results |>
  filter(final_published) |>
  mutate(final_pmid = as.character(final_pmid)) |>
  select(abstract_id, final_pmid)

# --- PubMed strategy performance ---
strategy_path <- here("data", "processed", "pubmed_strategy_results.csv")
candidates_path <- here("data", "processed", "pubmed_candidates.csv")

aim4_pubmed <- NULL
if (file.exists(strategy_path) && file.exists(candidates_path)) {
  strategy_results <- read_csv(strategy_path, show_col_types = FALSE)
  candidates <- read_csv(candidates_path, show_col_types = FALSE)

  strategy_contribution <- candidates |>
    mutate(pmid = as.character(pmid)) |>
    inner_join(accepted_pmids, by = c("abstract_id", "pmid" = "final_pmid")) |>
    mutate(strategy_list = strsplit(strategies, ";\\s*")) |>
    unnest(strategy_list) |>
    count(strategy_list, name = "n_found_correct") |>
    rename(strategy = strategy_list)

  aim4_pubmed <- strategy_results |>
    group_by(strategy) |>
    summarise(
      n_searched = n(),
      n_with_hits = sum(n_results > 0),
      yield_pct = round(mean(n_results > 0) * 100, 1),
      .groups = "drop"
    ) |>
    left_join(
      strategy_contribution |>
        mutate(pct_found = round(n_found_correct / nrow(accepted_pmids) * 100, 1)) |>
        select(strategy, n_found_correct, pct_found),
      by = "strategy"
    ) |>
    mutate(source = "PubMed")
}

# --- Supplementary API performance ---
supp_apis <- list(
  list(file = "crossref_candidates.csv", name = "CrossRef", pmid_col = NULL, doi_col = "doi"),
  list(file = "europmc_candidates.csv", name = "Europe PMC", pmid_col = "pmid", doi_col = "doi"),
  list(file = "openalex_candidates.csv", name = "OpenAlex", pmid_col = "pmid", doi_col = "doi"),
  list(file = "semantic_scholar_candidates.csv", name = "Semantic Scholar", pmid_col = "pmid", doi_col = "doi")
)

aim4_supp <- purrr::map_dfr(supp_apis, function(api) {
  fpath <- here("data", "processed", api$file)
  if (!file.exists(fpath)) return(NULL)
  cands <- read_csv(fpath, show_col_types = FALSE)

  n_total <- 98L  # Total abstracts searched
  n_abstracts_with_hits <- length(unique(cands$abstract_id))
  n_candidates <- nrow(cands)

  # Check if this source found the winning PMID for any accepted match
  n_found <- 0L
  if (!is.null(api$pmid_col) && api$pmid_col %in% names(cands)) {
    cands_pmid <- cands[[api$pmid_col]]
    if (!is.null(cands_pmid)) {
      matched <- cands |>
        mutate(.pmid = as.character(.data[[api$pmid_col]])) |>
        inner_join(accepted_pmids, by = c("abstract_id", ".pmid" = "final_pmid"))
      n_found <- nrow(matched)
    }
  }

  tibble::tibble(
    strategy = api$name,
    n_searched = n_total,
    n_with_hits = n_abstracts_with_hits,
    yield_pct = round(n_abstracts_with_hits / n_total * 100, 1),
    n_found_correct = if (n_found > 0) n_found else NA_integer_,
    pct_found = if (n_found > 0) round(n_found / nrow(accepted_pmids) * 100, 1) else NA_real_,
    source = "Supplementary"
  )
})

aim4 <- bind_rows(aim4_pubmed, aim4_supp)
write_csv(aim4, here("output", "aim4_strategy_performance.csv"))
cli_alert_success("Strategy ablation analysis complete")

# ============================================================
# Save all aim summaries
# ============================================================
write_csv(aim1, here("output", "aim1_publication_rate.csv"))
write_csv(aim2, here("output", "aim2_time_to_pub.csv"))
if (exists("aim3")) write_csv(aim3, here("output", "aim3_logistic_regression.csv"))

cli_alert_success("Analysis complete — results in output/")
