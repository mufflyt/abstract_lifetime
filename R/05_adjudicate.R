# 05_adjudicate.R — Apply scoring thresholds and generate output files

library(here)
library(readr)
library(dplyr)
library(cli)
library(config)

cfg <- config::get(file = here("config.yml"))

cli_h2("Adjudicating Matches")

# Load data
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
scores <- read_csv(here("data", "processed", "match_scores.csv"), show_col_types = FALSE)
candidates <- read_csv(here("data", "processed", "pubmed_candidates.csv"), show_col_types = FALSE)

# Join abstracts with their best matches
results <- abstracts |>
  left_join(scores, by = "abstract_id") |>
  left_join(
    candidates |>
      select(pmid, pub_title, pub_journal, pub_year, pub_month, pub_doi,
             pub_first_author, pub_last_author) |>
      distinct(pmid, .keep_all = TRUE),
    by = c("best_pmid" = "pmid")
  )

# Compute time to publication for accepted matches
results <- results |>
  mutate(
    pub_month_num = case_when(
      !is.na(suppressWarnings(as.integer(pub_month))) ~ as.integer(pub_month),
      pub_month %in% month.abb ~ match(pub_month, month.abb),
      TRUE ~ 1L
    ),
    pub_date = if_else(
      !is.na(pub_year),
      as.Date(sprintf("%s-%02d-01", pub_year, pub_month_num)),
      as.Date(NA)
    ),
    conference_date = as.Date(cfg$conference$date),
    months_to_pub = as.numeric(difftime(pub_date, conference_date, units = "days")) / 30.44
  )

# --- Output files ---

# 1. Full results with matches (including score breakdown)
score_component_cols <- c("title_sim", "title_pts", "abstract_pts",
                          "first_au_pts", "last_au_pts", "coauthor_pts",
                          "team_bonus", "journal_pts", "keyword_pts",
                          "date_pts", "no_text_penalty")
# Only include score columns that exist
available_score_cols <- intersect(score_component_cols, names(results))

results_out <- results |>
  select(
    abstract_id, title, first_author_normalized, last_author_normalized,
    author_count, is_rct, sample_size, is_academic, is_us_based,
    best_pmid, best_score, classification, has_tie, n_candidates,
    all_of(available_score_cols),
    pub_title, pub_journal, pub_year, pub_doi, pub_first_author,
    months_to_pub
  )

write_csv(results_out, here("output", "abstracts_with_matches.csv"))
cli_alert_success("Full results: output/abstracts_with_matches.csv")

# 2. Manual review queue
review_queue <- results |>
  filter(classification == "review" | has_tie) |>
  select(
    abstract_id, title, first_author_normalized,
    abstract_objective, abstract_conclusion,
    best_pmid, best_score, has_tie, n_candidates,
    all_of(available_score_cols),
    pub_title, pub_journal, pub_doi
  )

write_csv(review_queue, here("output", "manual_review_queue.csv"))
cli_alert_success("Manual review queue ({nrow(review_queue)} abstracts): output/manual_review_queue.csv")

# 3. Summary
cli_h3("Adjudication Summary")
cli_alert_info("Total abstracts: {nrow(results)}")
cli_alert_info("Auto-accepted matches: {sum(results$classification == 'accept', na.rm = TRUE)}")
cli_alert_info("Pending manual review: {sum(results$classification == 'review', na.rm = TRUE)}")
cli_alert_info("Rejected / no match: {sum(results$classification %in% c('reject', 'no_candidates'), na.rm = TRUE)}")

accepted <- results |> filter(classification == "accept")
if (nrow(accepted) > 0) {
  cli_alert_info("Median months to publication (accepted): {round(median(accepted$months_to_pub, na.rm = TRUE), 1)}")
}
