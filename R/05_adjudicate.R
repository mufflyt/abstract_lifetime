# 05_adjudicate.R — Apply scoring thresholds and generate output files

library(here)
library(readr)
library(dplyr)
library(cli)
library(config)
source(here("R", "utils_congresses.R"))

cfg <- config::get(file = here("config.yml"))

cli_h2("Adjudicating Matches")

# Load data
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
scores <- read_csv(here("data", "processed", "match_scores.csv"), show_col_types = FALSE)
candidates <- read_csv(here("data", "processed", "pubmed_candidates.csv"), show_col_types = FALSE)

# Join abstracts with their best matches
results <- abstracts |>
  left_join(scores |> mutate(best_pmid = as.character(best_pmid)), by = "abstract_id") |>
  left_join(
    candidates |>
      mutate(pmid = as.character(pmid)) |>
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
    conference_date = conference_date_for(congress_year, cfg),
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

# Exclude pre-conference publications entirely per study protocol.
# These are papers published BEFORE the conference date — they represent
# pre-existing work, not conference-to-publication conversions.
n_excluded <- sum(results$classification == "excluded", na.rm = TRUE)
if (n_excluded > 0) {
  cli_alert_info("Excluding {n_excluded} pre-conference publications")
  results <- results |> filter(classification != "excluded")
}

results_out <- results |>
  select(
    abstract_id, any_of("congress_year"), title,
    first_author_normalized, last_author_normalized,
    author_count, is_rct, sample_size, is_academic, is_us_based,
    any_of(c("session_type", "study_design", "is_multicenter",
             "has_funding", "stat_sig_reported",
             "has_numeric_results", "is_database_study", "has_industry",
             "has_trial_registration", "has_irb_statement",
             "abstract_word_count",
             "research_category", "primary_procedure")),
    best_pmid, best_score, classification, has_tie, n_candidates,
    all_of(available_score_cols),
    pub_title, pub_journal, pub_year, pub_doi, pub_first_author,
    months_to_pub
  )

# Blank publication-level fields for no_match/possible/no_candidates —
# these come from a wrong candidate, not the abstract's actual publication.
pub_cols <- c("pub_title", "pub_journal", "pub_year", "pub_doi",
              "pub_first_author", "months_to_pub")
wrong <- results_out$classification %in% c("no_match", "no_candidates", "possible")
for (col in intersect(pub_cols, names(results_out))) {
  results_out[[col]][wrong] <- NA
}
cli_alert_info("Blanked pub fields for {sum(wrong)} no_match/possible abstracts")

write_csv(results_out, here("output", "abstracts_with_matches.csv"))
cli_alert_success("Full results: output/abstracts_with_matches.csv")

# 2. Manual review queue (probable + possible + ties need human adjudication)
review_queue <- results |>
  filter(classification %in% c("probable", "possible") | has_tie) |>
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
cli_alert_info("Definite matches: {sum(results$classification == 'definite', na.rm = TRUE)}")
cli_alert_info("Probable (review needed): {sum(results$classification == 'probable', na.rm = TRUE)}")
cli_alert_info("Possible (weak evidence): {sum(results$classification == 'possible', na.rm = TRUE)}")
cli_alert_info("No match: {sum(results$classification %in% c('no_match', 'no_candidates'), na.rm = TRUE)}")
cli_alert_info("Excluded (pre-conference): {sum(results$classification == 'excluded', na.rm = TRUE)}")

accepted <- results |> filter(classification == "definite")
if (nrow(accepted) > 0) {
  cli_alert_info("Median months to publication (definite): {round(median(accepted$months_to_pub, na.rm = TRUE), 1)}")
}
