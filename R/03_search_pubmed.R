# 03_search_pubmed.R — Multi-strategy PubMed search for all abstracts

library(here)
library(readr)
library(dplyr)
library(purrr)
library(cli)
library(config)

source(here("R", "utils_pubmed.R"))
source(here("R", "utils_text.R"))

cfg <- config::get(file = here("config.yml"))

cli_h2("PubMed Multi-Strategy Search")

# Load cleaned abstracts
abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")
if (!file.exists(abstracts_path)) {
  stop("No cleaned abstracts found. Run 02_clean_abstracts.R first.")
}
abstracts <- read_csv(abstracts_path, show_col_types = FALSE)

# Reconstruct keywords as list
abstracts <- abstracts |>
  mutate(keywords = strsplit(keywords_str, ";\\s*"))

cli_alert_info("Searching PubMed for {nrow(abstracts)} abstracts")

# Check for checkpoint
checkpoint_path <- here(cfg$pipeline$checkpoint_dir, "pubmed_search_checkpoint.rds")
if (file.exists(checkpoint_path)) {
  cli_alert_info("Found checkpoint, resuming...")
  checkpoint <- readRDS(checkpoint_path)
  completed_ids <- checkpoint$completed_ids
  all_candidates <- checkpoint$all_candidates
  all_strategy_results <- checkpoint$all_strategy_results
} else {
  completed_ids <- character(0)
  all_candidates <- list()
  all_strategy_results <- list()
}

# Search each abstract
remaining <- abstracts |> filter(!abstract_id %in% completed_ids)
cli_alert_info("{nrow(remaining)} abstracts remaining to search")

for (i in seq_len(nrow(remaining))) {
  row <- remaining[i, ]
  cli_alert_info("[{i}/{nrow(remaining)}] Searching: {str_trunc(row$title, 60)}")

  # Run all 6 strategies
  candidates <- search_abstract(row, cfg)

  if (nrow(candidates) > 0) {
    cli_alert_success("  Found {nrow(candidates)} unique candidates via {max(candidates$n_strategies)} strategies")

    # Fetch details for all candidate PMIDs
    details <- fetch_pubmed_details(candidates$pmid, cfg)

    # Filter out the JMIG supplement itself
    if (nrow(details) > 0) {
      is_suppl <- vapply(seq_len(nrow(details)), function(j) {
        is_supplement_article(details[j, ], cfg)
      }, logical(1))
      details <- details[!is_suppl, ]
      if (sum(is_suppl) > 0) {
        cli_alert_info("  Filtered out {sum(is_suppl)} supplement articles")
      }
    }

    # Merge candidate strategy info with details
    if (nrow(details) > 0) {
      details <- details |>
        left_join(candidates, by = "pmid") |>
        mutate(abstract_id = row$abstract_id)

      all_candidates[[row$abstract_id]] <- details
    }
  } else {
    cli_alert_warning("  No candidates found")
  }

  # Track strategy-level results from the search we already ran (no duplicate
  # API calls — reuse the candidates tibble which records which strategies
  # found each PMID).
  strategies <- build_search_strategies(row, cfg)
  for (sname in names(strategies)) {
    n_hits <- if (nrow(candidates) > 0 && "strategies" %in% names(candidates)) {
      sum(grepl(sname, candidates$strategies))
    } else 0L
    all_strategy_results <- c(all_strategy_results, list(tibble::tibble(
      abstract_id = row$abstract_id,
      strategy = sname,
      query = strategies[[sname]],
      n_results = n_hits
    )))
  }

  completed_ids <- c(completed_ids, row$abstract_id)

  # Checkpoint every N abstracts
  if (i %% cfg$pubmed$cache_every_n == 0) {
    cli_alert_info("  Saving checkpoint ({length(completed_ids)} complete)...")
    saveRDS(list(
      completed_ids = completed_ids,
      all_candidates = all_candidates,
      all_strategy_results = all_strategy_results
    ), checkpoint_path)
  }
}

# Final save
saveRDS(list(
  completed_ids = completed_ids,
  all_candidates = all_candidates,
  all_strategy_results = all_strategy_results
), checkpoint_path)

# Combine all candidates into one dataframe
candidates_df <- bind_rows(all_candidates)
strategy_df <- bind_rows(all_strategy_results)

write_csv(candidates_df, here("data", "processed", "pubmed_candidates.csv"))
write_csv(strategy_df, here("data", "processed", "pubmed_strategy_results.csv"))

cli_alert_success("PubMed search complete")
cli_alert_info("Total candidates: {nrow(candidates_df)}")
cli_alert_info("Abstracts with >=1 candidate: {n_distinct(candidates_df$abstract_id)}")
cli_alert_info("Abstracts with 0 candidates: {nrow(abstracts) - n_distinct(candidates_df$abstract_id)}")

# Save search efficacy summary
efficacy <- strategy_df |>
  group_by(strategy) |>
  summarise(
    n_abstracts_searched = n(),
    n_with_results = sum(n_results > 0),
    pct_with_results = round(mean(n_results > 0) * 100, 1),
    mean_results = round(mean(n_results), 1),
    median_results = median(n_results),
    .groups = "drop"
  )

write_csv(efficacy, here("output", "search_strategy_efficacy.csv"))
cli_alert_success("Strategy efficacy saved to output/search_strategy_efficacy.csv")
