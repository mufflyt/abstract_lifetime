#!/usr/bin/env Rscript
# Build a gold standard validation set by sampling abstracts and manually
# verifying publication status via intensive PubMed search.
#
# Strategy: for each sampled abstract, perform a focused PubMed search using
# the full title (quoted phrase search), then check first-author + last-author
# searches. If any search returns a PMID whose title Jaccard >= 0.6 with the
# abstract title, classify as published. This provides a more thorough search
# than the pipeline's multi-strategy approach and serves as ground truth.

suppressPackageStartupMessages({
  library(here); library(config); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(rentrez)
})

source(here("R", "utils_scoring.R"))
source(here("R", "utils_text.R"))

cfg <- config::get(file = here("config.yml"))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

cli_h1("Building Gold Standard Validation Set")

matches <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)

set.seed(cfg$pipeline$seed)

# Sample 25 definite + 25 no_match, stratified
n_sample <- cfg$validation$gold_standard_n %||% 25L
definites <- matches |> filter(classification == "definite") |> slice_sample(n = min(n_sample, n()))
no_matches <- matches |> filter(classification == "no_match") |> slice_sample(n = min(n_sample, n()))
sample_abs <- bind_rows(definites, no_matches)

cli_alert_info("Sampled {nrow(sample_abs)} abstracts: {sum(sample_abs$classification == 'definite')} definite + {sum(sample_abs$classification == 'no_match')} no_match")

has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
delay <- if (has_key) 1 / cfg$pubmed$rate_limit_with_key else 1 / cfg$pubmed$rate_limit_per_sec

# Intensive search: try multiple exact and relaxed queries
verify_one <- function(row) {
  title <- row$title
  first_au <- row$first_author_normalized
  last_au <- row$last_author_normalized

  title_words <- str_split(normalize_title(title), "\\s+")[[1]]
  title_words <- title_words[nchar(title_words) >= 3]

  queries <- character()

  # Q1: Full title phrase (first 10 significant words)
  if (length(title_words) >= 5) {
    queries <- c(queries, sprintf('"%s"[TI]', paste(head(title_words, 10), collapse = " ")))
  }

  # Q2: First 6 words of title
  if (length(title_words) >= 4) {
    queries <- c(queries, sprintf('"%s"[TI]', paste(head(title_words, 6), collapse = " ")))
  }

  # Q3: First author + title keywords
  if (!is.na(first_au) && nchar(first_au) > 1 && length(title_words) >= 3) {
    queries <- c(queries, sprintf('"%s"[AU] AND (%s)[TI]',
                                   first_au, paste(head(title_words, 4), collapse = " AND ")))
  }

  # Q4: Last author + title keywords
  if (!is.na(last_au) && nchar(last_au) > 1 && length(title_words) >= 3) {
    queries <- c(queries, sprintf('"%s"[AU] AND (%s)[TI]',
                                   last_au, paste(head(title_words, 4), collapse = " AND ")))
  }

  # Q5: First author broad
  if (!is.na(first_au) && nchar(first_au) > 1) {
    queries <- c(queries, sprintf('"%s"[1AU]', first_au))
  }

  best_pmid <- NA_character_
  best_score <- 0
  best_title <- NA_character_

  for (q in queries) {
    Sys.sleep(delay)
    result <- tryCatch(
      rentrez::entrez_search(db = "pubmed", term = q, retmax = 20),
      error = function(e) NULL
    )
    if (is.null(result) || length(result$ids) == 0) next

    Sys.sleep(delay)
    details <- tryCatch(
      rentrez::entrez_fetch(db = "pubmed", id = result$ids, rettype = "xml"),
      error = function(e) NULL
    )
    if (is.null(details)) next

    doc <- tryCatch(xml2::read_xml(details), error = function(e) NULL)
    if (is.null(doc)) next

    articles <- xml2::xml_find_all(doc, "//PubmedArticle")
    for (art in articles) {
      pmid <- xml2::xml_text(xml2::xml_find_first(art, ".//PMID"))
      pub_title <- xml2::xml_text(xml2::xml_find_first(art, ".//ArticleTitle"))
      if (is.na(pub_title)) next

      sim <- jaccard_similarity(normalize_title(title), normalize_title(pub_title))
      if (sim > best_score) {
        best_score <- sim
        best_pmid <- pmid
        best_title <- pub_title
      }
    }

    if (best_score >= 0.7) break  # High-confidence match, stop searching
  }

  tibble(
    abstract_id = row$abstract_id,
    title = title,
    first_author = first_au,
    verified_published = best_score >= 0.5,
    verified_pmid = if (best_score >= 0.5) best_pmid else NA_character_,
    verified_title = if (best_score >= 0.5) best_title else NA_character_,
    verified_jaccard = round(best_score, 3),
    verified_doi = NA_character_,
    verified_journal = NA_character_,
    verified_pub_date = NA_character_,
    notes = case_when(
      best_score >= 0.7 ~ "high-confidence match",
      best_score >= 0.5 ~ "moderate-confidence match",
      best_score >= 0.3 ~ "possible match (needs manual check)",
      TRUE ~ "no match found in intensive search"
    )
  )
}

cli_alert_info("Running intensive verification searches...")
results <- list()
for (i in seq_len(nrow(sample_abs))) {
  if (i %% 10 == 0) cli_alert_info("  {i}/{nrow(sample_abs)}")
  results[[i]] <- verify_one(sample_abs[i, ])
}

gold <- bind_rows(results)

gs_path <- here("data", "validation", "gold_standard.csv")
write_csv(gold, gs_path)
cli_alert_success("Gold standard saved: {gs_path}")

n_pub <- sum(gold$verified_published)
cli_alert_info("Verified published: {n_pub} / {nrow(gold)}")
cli_alert_info("  From definite: {sum(gold$verified_published[gold$abstract_id %in% definites$abstract_id])} / {nrow(definites)}")
cli_alert_info("  From no_match: {sum(gold$verified_published[gold$abstract_id %in% no_matches$abstract_id])} / {nrow(no_matches)}")

# Now run the validation script
cli_h2("Running validation against gold standard")

# Update the validation script to use new classification labels
validation_script <- here("R", "validation_gold_standard.R")
