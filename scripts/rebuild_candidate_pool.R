#!/usr/bin/env Rscript
# rebuild_candidate_pool.R — Restore data/processed/pubmed_candidates.csv to the
# pool that R/04_score_matches.R actually scored.
#
# Why this exists
# ---------------
# R/03b_search_crossref.R rewrites pubmed_candidates.csv in place at the end of
# every supplementary-search run. On 2026-04-19 it ran AFTER 04_score_matches.R,
# leaving a file that is a strict subset of the pool the scores were computed
# against: 283 of the 1,102 winning PMIDs were absent, so R/05_adjudicate.R
# could not join publication metadata for them and 74 of the 178 published
# abstracts carried no publication date. Documented as F2 in
# docs/FAILURE_MODES.md.
#
# data/processed/match_scores_detailed.rds is the surviving record of every
# (abstract_id, pmid) pair that was scored. This script takes the pairs the pool
# is missing, refetches their publication metadata from PubMed, and rebuilds the
# file.
#
# What it cannot restore
# ----------------------
# The `strategies` / `n_strategies` provenance for a recovered pair is gone —
# it lived only in the overwritten file. Recovered rows are tagged
# strategies = "unrecovered" so Aim 4's strategy attribution can exclude them
# honestly rather than silently miscount them.
#
# Rows keyed to abstract_ids outside the 1,154 parsed presentations are dropped.
# Those are residue from the superseded 686-row 2023 scrape.
#
# Usage: Rscript scripts/rebuild_candidate_pool.R
# Resumable: refetched metadata is checkpointed after every batch.

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(purrr)
  library(tidyr); library(cli); library(rentrez); library(xml2); library(stringr)
})

source(here("R", "utils_pubmed.R"))
cfg <- config::get(file = here("config.yml"))

pool_path <- here("data", "processed", "pubmed_candidates.csv")
ckpt_path <- here("data", "cache", "checkpoints", "candidate_pool_rebuild.rds")
dir.create(dirname(ckpt_path), showWarnings = FALSE, recursive = TRUE)

cli_h1("Rebuilding the PubMed candidate pool")

# ---- 1. What was scored, and what the pool currently holds -------------------

detailed <- readRDS(here("data", "processed", "match_scores_detailed.rds"))
parsed   <- read_csv(here("data", "processed", "abstracts_parsed_web.csv"),
                     show_col_types = FALSE)

scored_pairs <- detailed |>
  filter(!map_lgl(score_details, is.null)) |>
  transmute(abstract_id, .sd = score_details) |>
  unnest(.sd) |>
  transmute(abstract_id, pmid = as.character(pmid)) |>
  distinct()

pool_old <- read_csv(pool_path, show_col_types = FALSE,
                     col_types = cols(.default = col_character()))

# Reviewer-supplied PMIDs are a second source of pairs the pool must carry. A
# reviewer who found the publication independently entered a manual_pmid the
# search never returned, so it is in no candidate set. R/05_adjudicate.R and the
# refresh block in R/06_analyze_results.R both resolve publication metadata out
# of this pool, so without these rows a reviewer-confirmed publication has no
# date. Six of the 178 published abstracts were in exactly that position.
decisions_path <- here("output", "manual_review_decisions.csv")
reviewer_pairs <- if (file.exists(decisions_path)) {
  read_csv(decisions_path, show_col_types = FALSE) |>
    filter(manual_decision == "match", !is.na(manual_pmid)) |>
    transmute(abstract_id, pmid = as.character(manual_pmid)) |>
    filter(str_detect(pmid, "^[0-9]+$")) |>
    distinct()
} else {
  tibble::tibble(abstract_id = character(), pmid = character())
}
cli_alert_info("Reviewer-supplied PMIDs: {nrow(reviewer_pairs)}")

wanted <- bind_rows(scored_pairs, reviewer_pairs) |> distinct()

missing_pairs <- wanted |>
  anti_join(transmute(pool_old, abstract_id, pmid = as.character(pmid)) |> distinct(),
            by = c("abstract_id", "pmid"))

cli_alert_info("Scored pairs: {nrow(scored_pairs)}")
cli_alert_info("Pairs already in the pool: {nrow(pool_old)}")
cli_alert_info("Pairs to recover: {nrow(missing_pairs)} across \\
                {n_distinct(missing_pairs$pmid)} PMIDs")

if (nrow(missing_pairs) == 0) {
  cli_alert_success("Nothing to recover — the pool already covers every scored pair")
  quit(save = "no")
}

# ---- 2. Fetch the missing PMIDs ---------------------------------------------

need <- unique(missing_pairs$pmid)
fetched <- if (file.exists(ckpt_path)) readRDS(ckpt_path) else list()
done <- if (length(fetched) > 0) unique(bind_rows(fetched)$pmid) else character(0)
need <- setdiff(need, done)
cli_alert_info("{length(done)} PMIDs already checkpointed; {length(need)} to fetch")

if (length(need) > 0) {
  batches <- split(need, ceiling(seq_along(need) / 100))
  for (i in seq_along(batches)) {
    raw <- tryCatch(
      rentrez::entrez_fetch(db = "pubmed", id = batches[[i]], rettype = "xml"),
      error = function(e) { cli_alert_warning("batch {i}: {e$message}"); NA_character_ }
    )
    if (!is.na(raw)) {
      parsed_batch <- tryCatch(parse_pubmed_xml(raw), error = function(e) NULL)
      if (!is.null(parsed_batch) && nrow(parsed_batch) > 0) {
        fetched[[length(fetched) + 1]] <- parsed_batch
      }
    }
    saveRDS(fetched, ckpt_path)
    if (i %% 10 == 0 || i == length(batches)) {
      cli_alert_info("  batch {i}/{length(batches)} \\
                      ({sum(vapply(fetched, nrow, integer(1)))} records)")
    }
    Sys.sleep(if (nchar(Sys.getenv("ENTREZ_KEY", "")) > 0) 0.11 else 0.34)
  }
}

recovered <- bind_rows(fetched) |>
  mutate(across(everything(), as.character)) |>
  distinct(pmid, .keep_all = TRUE)
cli_alert_success("Recovered metadata for {nrow(recovered)} PMIDs")

unresolved <- setdiff(unique(missing_pairs$pmid), recovered$pmid)
if (length(unresolved) > 0) {
  cli_alert_warning("{length(unresolved)} PMIDs returned no record (withdrawn or \\
                     merged); their pairs are dropped")
}

# ---- 3. Rebuild ---------------------------------------------------------------

recovered_rows <- missing_pairs |>
  inner_join(recovered, by = "pmid", relationship = "many-to-many") |>
  mutate(strategies = "unrecovered", n_strategies = NA_character_,
         first_query = NA_character_)

pool_new <- bind_rows(pool_old, recovered_rows) |>
  filter(abstract_id %in% parsed$abstract_id) |>
  distinct(abstract_id, pmid, .keep_all = TRUE) |>
  select(any_of(names(pool_old)))

cli_h2("Verification")
scores <- read_csv(here("data", "processed", "match_scores.csv"),
                   show_col_types = FALSE) |>
  mutate(best_pmid = as.character(best_pmid))

unresolvable <- scores |>
  filter(!is.na(best_pmid)) |>
  anti_join(distinct(select(pool_new, abstract_id, pmid)),
            by = c("abstract_id", "best_pmid" = "pmid"))

cli_alert_info("Pool rows: {nrow(pool_old)} -> {nrow(pool_new)}")
cli_alert_info("Winning PMIDs still unresolvable: {nrow(unresolvable)} (was 283)")

cohort_pairs <- pool_new |> filter(abstract_id %in% scores$abstract_id) |> nrow()
cli_alert_info("Cohort pairs in pool: {cohort_pairs} vs sum(n_candidates) = \\
                {sum(scores$n_candidates)}")

write_csv(pool_new, pool_path)
cli_alert_success("Wrote {pool_path}")
