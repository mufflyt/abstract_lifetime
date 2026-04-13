# 04_score_matches.R — Score all candidate matches

library(here)
library(readr)
library(dplyr)
library(purrr)
library(cli)
library(config)

source(here("R", "utils_scoring.R"))
source(here("R", "utils_text.R"))

cfg <- config::get(file = here("config.yml"))

cli_h2("Scoring Candidate Matches")

# Load data
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
candidates <- read_csv(here("data", "processed", "pubmed_candidates.csv"), show_col_types = FALSE)

# Reconstruct list columns
abstracts <- abstracts |>
  mutate(
    keywords = strsplit(keywords_str, ";\\s*"),
    all_authors_normalized = strsplit(all_authors_str, ";\\s*")
  )

cli_alert_info("Scoring matches for {nrow(abstracts)} abstracts against {nrow(candidates)} candidates")

# Score each abstract
all_results <- list()

for (i in seq_len(nrow(abstracts))) {
  row <- abstracts[i, ]

  # Get candidates for this abstract
  abs_candidates <- candidates |>
    filter(abstract_id == row$abstract_id)

  if (nrow(abs_candidates) == 0) {
    all_results <- c(all_results, list(tibble::tibble(
      abstract_id = row$abstract_id,
      best_pmid = NA_character_,
      best_score = 0,
      classification = "no_candidates",
      has_tie = FALSE,
      n_candidates = 0L,
      score_details = list(NULL)
    )))
    next
  }

  result <- score_abstract_candidates(row, abs_candidates, cfg)
  all_results <- c(all_results, list(result))

  if (i %% 10 == 0) {
    cli_alert_info("[{i}/{nrow(abstracts)}] scored")
  }
}

results_df <- bind_rows(all_results)

# Summary
cli_h3("Scoring Summary")
cli_alert_info("Auto-accept (score >= {cfg$scoring$auto_accept}): {sum(results_df$classification == 'accept')}")
cli_alert_info("Manual review (score {cfg$scoring$manual_review}-{cfg$scoring$auto_accept - 0.1}): {sum(results_df$classification == 'review')}")
cli_alert_info("Auto-reject (score < {cfg$scoring$manual_review}): {sum(results_df$classification == 'reject')}")
cli_alert_info("No candidates: {sum(results_df$classification == 'no_candidates')}")
cli_alert_info("Ties requiring review: {sum(results_df$has_tie)}")

# Save results (without list column for CSV)
results_save <- results_df |>
  select(-score_details)
write_csv(results_save, here("data", "processed", "match_scores.csv"))

# Save detailed scores as RDS (preserves list column)
saveRDS(results_df, here("data", "processed", "match_scores_detailed.rds"))

cli_alert_success("Scores saved to data/processed/match_scores.csv")
