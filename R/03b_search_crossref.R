# 03b_search_crossref.R — Supplementary search via CrossRef + Europe PMC

library(here)
library(readr)
library(dplyr)
library(purrr)
library(cli)
library(config)

source(here("R", "utils_crossref.R"))
source(here("R", "utils_text.R"))

cfg <- config::get(file = here("config.yml"))

cli_h2("Supplementary Search: CrossRef + Europe PMC")

# Load abstracts and existing PubMed candidates
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
pubmed_path <- here("data", "processed", "pubmed_candidates.csv")

if (file.exists(pubmed_path)) {
  pubmed_candidates <- read_csv(pubmed_path, show_col_types = FALSE)
  abstracts_with_hits <- unique(pubmed_candidates$abstract_id)
} else {
  pubmed_candidates <- tibble()
  abstracts_with_hits <- character(0)
}

# Focus on abstracts with 0 PubMed hits
no_hits <- abstracts |> filter(!abstract_id %in% abstracts_with_hits)
cli_alert_info("{nrow(no_hits)} abstracts with 0 PubMed candidates — searching CrossRef + Europe PMC")

# Also search abstracts with low-confidence PubMed matches (< 4 candidates)
low_hits <- pubmed_candidates |>
  count(abstract_id) |>
  filter(n <= 2) |>
  pull(abstract_id)
low_hit_abstracts <- abstracts |> filter(abstract_id %in% low_hits)

to_search <- bind_rows(no_hits, low_hit_abstracts) |> distinct(abstract_id, .keep_all = TRUE)
cli_alert_info("Total abstracts for supplementary search: {nrow(to_search)}")

all_cr_results <- list()
all_epmc_results <- list()

for (i in seq_len(nrow(to_search))) {
  row <- to_search[i, ]
  cli_alert_info("[{i}/{nrow(to_search)}] {str_trunc(row$title, 50)}")

  # CrossRef search
  cr <- search_crossref(row$title, max_results = cfg$crossref$max_results)
  if (nrow(cr) > 0) {
    cr$abstract_id <- row$abstract_id
    all_cr_results <- c(all_cr_results, list(cr))
    cli_alert_success("  CrossRef: {nrow(cr)} results")
  }

  Sys.sleep(0.5)  # Polite

  # Europe PMC search
  epmc <- search_europmc(row$title, row$first_author_normalized,
                         max_results = cfg$europmc$max_results)
  if (nrow(epmc) > 0) {
    epmc$abstract_id <- row$abstract_id
    all_epmc_results <- c(all_epmc_results, list(epmc))
    cli_alert_success("  Europe PMC: {nrow(epmc)} results")
  }

  Sys.sleep(0.3)
}

# Save CrossRef results
cr_df <- bind_rows(all_cr_results)
if (nrow(cr_df) > 0) {
  write_csv(cr_df, here("data", "processed", "crossref_candidates.csv"))
  cli_alert_success("CrossRef candidates: {nrow(cr_df)}")
}

# Save Europe PMC results
epmc_df <- bind_rows(all_epmc_results)
if (nrow(epmc_df) > 0) {
  write_csv(epmc_df, here("data", "processed", "europmc_candidates.csv"))
  cli_alert_success("Europe PMC candidates: {nrow(epmc_df)}")
}

# For Europe PMC results that have PMIDs, fetch full details and merge with PubMed candidates
if (nrow(epmc_df) > 0) {
  new_pmids <- epmc_df |>
    filter(!is.na(pmid) & !pmid %in% pubmed_candidates$pmid) |>
    pull(pmid) |>
    unique()

  if (length(new_pmids) > 0) {
    cli_alert_info("Fetching PubMed details for {length(new_pmids)} new PMIDs from Europe PMC")
    source(here("R", "utils_pubmed.R"))
    new_details <- fetch_pubmed_details(new_pmids, cfg)

    if (nrow(new_details) > 0) {
      # Add abstract_id mapping
      pmid_to_abstract <- epmc_df |>
        filter(pmid %in% new_pmids) |>
        select(pmid, abstract_id) |>
        distinct()
      new_details <- new_details |>
        left_join(pmid_to_abstract, by = "pmid") |>
        mutate(strategies = "europmc", n_strategies = 1L)

      # Append to PubMed candidates
      combined <- bind_rows(pubmed_candidates, new_details) |>
        distinct(abstract_id, pmid, .keep_all = TRUE)
      write_csv(combined, here("data", "processed", "pubmed_candidates.csv"))
      cli_alert_success("Updated candidates file with {nrow(new_details)} new entries")
    }
  }
}

cli_alert_success("Supplementary search complete")
