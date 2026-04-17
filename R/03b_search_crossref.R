# 03b_search_crossref.R — Supplementary search via CrossRef + Europe PMC

library(here)
library(readr)
library(dplyr)
library(purrr)
library(cli)
library(config)

source(here("R", "utils_crossref.R"))
source(here("R", "utils_text.R"))
source(here("R", "utils_pubmed.R"))

cfg <- config::get(file = here("config.yml"))

cli_h2("Supplementary Search: CrossRef + Europe PMC + OpenAlex + Semantic Scholar")

# Load abstracts and existing PubMed candidates
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
pubmed_path <- here("data", "processed", "pubmed_candidates.csv")

if (file.exists(pubmed_path)) {
  pubmed_candidates <- read_csv(pubmed_path, show_col_types = FALSE) |>
    mutate(across(everything(), as.character))
  abstracts_with_hits <- unique(pubmed_candidates$abstract_id)
} else {
  pubmed_candidates <- tibble()
  abstracts_with_hits <- character(0)
}

# ============================================================
# CrossRef: targeted search for low-hit abstracts only
# ============================================================
no_hits <- abstracts |> filter(!abstract_id %in% abstracts_with_hits)
low_hits <- pubmed_candidates |>
  count(abstract_id) |>
  filter(n <= 2) |>
  pull(abstract_id)
low_hit_abstracts <- abstracts |> filter(abstract_id %in% low_hits)

cr_to_search <- bind_rows(no_hits, low_hit_abstracts) |> distinct(abstract_id, .keep_all = TRUE)
cli_alert_info("CrossRef: searching {nrow(cr_to_search)} abstracts with 0 or few PubMed hits")

date_start_cr <- gsub("/", "-", cfg$pubmed$date_start)
date_end_cr <- gsub("/", "-", cfg$pubmed$date_end)

all_cr_results <- list()
for (i in seq_len(nrow(cr_to_search))) {
  row <- cr_to_search[i, ]
  cr <- search_crossref(row$title, max_results = cfg$crossref$max_results,
                        date_start = date_start_cr, date_end = date_end_cr)
  if (nrow(cr) > 0) {
    cr$abstract_id <- row$abstract_id
    all_cr_results <- c(all_cr_results, list(cr))
  }
  Sys.sleep(0.5)
}
cr_df <- bind_rows(all_cr_results)
if (nrow(cr_df) > 0) {
  write_csv(cr_df, here("data", "processed", "crossref_candidates.csv"))
  cli_alert_success("CrossRef candidates: {nrow(cr_df)} across {n_distinct(cr_df$abstract_id)} abstracts")
}

# ============================================================
# Europe PMC: search ALL abstracts with multi-strategy approach
# ============================================================
cli_h3("Europe PMC: multi-strategy search for all {nrow(abstracts)} abstracts")

# Parse year range from config
year_start <- as.integer(substr(cfg$pubmed$date_start, 1, 4))
year_end <- as.integer(substr(cfg$pubmed$date_end, 1, 4))

# Check for Europe PMC checkpoint
epmc_checkpoint_path <- here(cfg$pipeline$checkpoint_dir, "europmc_search_checkpoint.rds")
if (file.exists(epmc_checkpoint_path)) {
  epmc_checkpoint <- readRDS(epmc_checkpoint_path)
  epmc_completed <- epmc_checkpoint$completed_ids
  all_epmc_results <- epmc_checkpoint$all_epmc_results
  cli_alert_info("Resuming from checkpoint ({length(epmc_completed)} already done)")
} else {
  epmc_completed <- character(0)
  all_epmc_results <- list()
}

remaining <- abstracts |> filter(!abstract_id %in% epmc_completed)
cli_alert_info("{nrow(remaining)} abstracts remaining for Europe PMC search")

for (i in seq_len(nrow(remaining))) {
  row <- remaining[i, ]
  if (i %% 10 == 0 || i == 1) {
    cli_alert_info("[{i}/{nrow(remaining)}] {stringr::str_trunc(row$title, 60)}")
  }

  epmc <- search_europmc(
    title = row$title,
    first_author = row$first_author_normalized,
    max_results = cfg$europmc$max_results,
    year_start = year_start,
    year_end = year_end
  )

  if (nrow(epmc) > 0) {
    epmc$abstract_id <- row$abstract_id
    all_epmc_results <- c(all_epmc_results, list(epmc))
    cli_alert_success("  {nrow(epmc)} Europe PMC results ({paste(unique(epmc$epmc_strategy), collapse=', ')})")
  }

  epmc_completed <- c(epmc_completed, row$abstract_id)

  # Checkpoint every 20 abstracts
  if (i %% 20 == 0) {
    saveRDS(list(completed_ids = epmc_completed, all_epmc_results = all_epmc_results),
            epmc_checkpoint_path)
  }

  Sys.sleep(0.3)  # Polite rate limiting
}

# Final checkpoint
saveRDS(list(completed_ids = epmc_completed, all_epmc_results = all_epmc_results),
        epmc_checkpoint_path)

# Combine Europe PMC results
epmc_df <- bind_rows(all_epmc_results)
if (nrow(epmc_df) > 0) {
  write_csv(epmc_df, here("data", "processed", "europmc_candidates.csv"))
  n_with_pmid <- sum(!is.na(epmc_df$pmid) & epmc_df$pmid != "")
  cli_alert_success("Europe PMC total: {nrow(epmc_df)} candidates ({n_with_pmid} with PMIDs) across {n_distinct(epmc_df$abstract_id)} abstracts")
} else {
  cli_alert_warning("Europe PMC returned 0 results")
}

# ============================================================
# OpenAlex: search ALL abstracts with keyword-based search
# ============================================================
cli_h3("OpenAlex: keyword search for all {nrow(abstracts)} abstracts")

date_start_oa <- gsub("/", "-", cfg$pubmed$date_start)
date_end_oa <- gsub("/", "-", cfg$pubmed$date_end)

oa_checkpoint_path <- here(cfg$pipeline$checkpoint_dir, "openalex_search_checkpoint.rds")
if (file.exists(oa_checkpoint_path)) {
  oa_checkpoint <- readRDS(oa_checkpoint_path)
  oa_completed <- oa_checkpoint$completed_ids
  all_oa_results <- oa_checkpoint$all_oa_results
  cli_alert_info("Resuming from checkpoint ({length(oa_completed)} already done)")
} else {
  oa_completed <- character(0)
  all_oa_results <- list()
}

oa_remaining <- abstracts |> filter(!abstract_id %in% oa_completed)
cli_alert_info("{nrow(oa_remaining)} abstracts remaining for OpenAlex search")

for (i in seq_len(nrow(oa_remaining))) {
  row <- oa_remaining[i, ]
  if (i %% 10 == 0 || i == 1) {
    cli_alert_info("[{i}/{nrow(oa_remaining)}] {stringr::str_trunc(row$title, 60)}")
  }

  oa <- search_openalex(
    title = row$title,
    first_author = row$first_author_normalized,
    max_results = cfg$crossref$max_results,
    date_start = date_start_oa,
    date_end = date_end_oa
  )

  if (nrow(oa) > 0) {
    oa$abstract_id <- row$abstract_id
    all_oa_results <- c(all_oa_results, list(oa))
    n_with_pmid <- sum(!is.na(oa$pmid) & oa$pmid != "")
    cli_alert_success("  {nrow(oa)} OpenAlex results ({n_with_pmid} with PMIDs)")
  }

  oa_completed <- c(oa_completed, row$abstract_id)

  if (i %% 20 == 0) {
    saveRDS(list(completed_ids = oa_completed, all_oa_results = all_oa_results),
            oa_checkpoint_path)
  }

  Sys.sleep(0.1)  # OpenAlex is generous with rate limits
}

saveRDS(list(completed_ids = oa_completed, all_oa_results = all_oa_results),
        oa_checkpoint_path)

oa_df <- bind_rows(all_oa_results)
if (nrow(oa_df) > 0) {
  write_csv(oa_df, here("data", "processed", "openalex_candidates.csv"))
  n_with_pmid <- sum(!is.na(oa_df$pmid) & oa_df$pmid != "")
  cli_alert_success("OpenAlex total: {nrow(oa_df)} candidates ({n_with_pmid} with PMIDs) across {n_distinct(oa_df$abstract_id)} abstracts")
} else {
  cli_alert_warning("OpenAlex returned 0 results")
}

# ============================================================
# Semantic Scholar: search ALL abstracts
# ============================================================
cli_h3("Semantic Scholar: keyword search for all {nrow(abstracts)} abstracts")

s2_checkpoint_path <- here(cfg$pipeline$checkpoint_dir, "semantic_scholar_checkpoint.rds")
if (file.exists(s2_checkpoint_path)) {
  s2_checkpoint <- readRDS(s2_checkpoint_path)
  s2_completed <- s2_checkpoint$completed_ids
  all_s2_results <- s2_checkpoint$all_s2_results
  cli_alert_info("Resuming from checkpoint ({length(s2_completed)} already done)")
} else {
  s2_completed <- character(0)
  all_s2_results <- list()
}

s2_remaining <- abstracts |> filter(!abstract_id %in% s2_completed)
cli_alert_info("{nrow(s2_remaining)} abstracts remaining for Semantic Scholar search")

for (i in seq_len(nrow(s2_remaining))) {
  row <- s2_remaining[i, ]
  if (i %% 10 == 0 || i == 1) {
    cli_alert_info("[{i}/{nrow(s2_remaining)}] {stringr::str_trunc(row$title, 60)}")
  }

  s2 <- search_semantic_scholar(
    title = row$title,
    first_author = row$first_author_normalized,
    max_results = cfg$semantic_scholar$max_results,
    year_start = year_start,
    year_end = year_end
  )

  if (nrow(s2) > 0) {
    s2$abstract_id <- row$abstract_id
    all_s2_results <- c(all_s2_results, list(s2))
    n_with_pmid <- sum(!is.na(s2$pmid) & s2$pmid != "")
    cli_alert_success("  {nrow(s2)} Semantic Scholar results ({n_with_pmid} with PMIDs)")
  }

  s2_completed <- c(s2_completed, row$abstract_id)

  if (i %% 20 == 0) {
    saveRDS(list(completed_ids = s2_completed, all_s2_results = all_s2_results),
            s2_checkpoint_path)
  }

  Sys.sleep(1)  # Semantic Scholar rate limits are stricter
}

saveRDS(list(completed_ids = s2_completed, all_s2_results = all_s2_results),
        s2_checkpoint_path)

s2_df <- bind_rows(all_s2_results)
if (nrow(s2_df) > 0) {
  write_csv(s2_df, here("data", "processed", "semantic_scholar_candidates.csv"))
  n_with_pmid <- sum(!is.na(s2_df$pmid) & s2_df$pmid != "")
  cli_alert_success("Semantic Scholar total: {nrow(s2_df)} candidates ({n_with_pmid} with PMIDs) across {n_distinct(s2_df$abstract_id)} abstracts")
} else {
  s2_df <- tibble()
  cli_alert_warning("Semantic Scholar returned 0 results")
}

# ============================================================
# Merge new PMIDs into the PubMed candidates pool
# ============================================================
cli_h3("Merging new candidates into PubMed pool")

new_pmid_count <- 0

# From OpenAlex
if (nrow(oa_df) > 0) {
  oa_with_pmid <- oa_df |>
    filter(!is.na(pmid) & pmid != "") |>
    filter(!pmid %in% pubmed_candidates$pmid)

  new_oa_pmids <- unique(oa_with_pmid$pmid)

  if (length(new_oa_pmids) > 0) {
    cli_alert_info("Fetching PubMed details for {length(new_oa_pmids)} new PMIDs from OpenAlex")
    oa_details <- fetch_pubmed_details(new_oa_pmids, cfg)

    if (nrow(oa_details) > 0) {
      oa_details <- oa_details |> mutate(across(everything(), as.character))
      pmid_to_abstract <- oa_with_pmid |>
        mutate(pmid = as.character(pmid)) |>
        select(pmid, abstract_id) |>
        distinct()
      oa_details <- oa_details |>
        inner_join(pmid_to_abstract, by = "pmid", relationship = "many-to-many") |>
        mutate(strategies = "openalex", n_strategies = "1") |>
        mutate(across(everything(), as.character))

      # Filter out JMIG supplement
      is_suppl <- vapply(seq_len(nrow(oa_details)), function(j) {
        is_jmig <- grepl("j minim invasive gynecol", tolower(oa_details$pub_journal_abbrev[j]), fixed = TRUE)
        is_vol <- !is.na(oa_details$pub_volume[j]) && oa_details$pub_volume[j] %in% as.character(cfg$pubmed$exclude_supplement_vol)
        is_yr <- !is.na(oa_details$pub_year[j]) && oa_details$pub_year[j] %in% as.character(cfg$pubmed$exclude_supplement_year)
        is_jmig && is_vol && is_yr
      }, logical(1))
      oa_details <- oa_details[!is_suppl, ]

      if (nrow(oa_details) > 0) {
        pubmed_candidates <- bind_rows(pubmed_candidates, oa_details) |>
          distinct(abstract_id, pmid, .keep_all = TRUE)
        new_pmid_count <- new_pmid_count + nrow(oa_details)
        cli_alert_success("Added {nrow(oa_details)} new OpenAlex candidates to pool")
      }
    }
  } else {
    cli_alert_info("No new PMIDs from OpenAlex (all already in pool)")
  }
}

# From Semantic Scholar
if (nrow(s2_df) > 0) {
  s2_with_pmid <- s2_df |>
    filter(!is.na(pmid) & pmid != "") |>
    filter(!pmid %in% pubmed_candidates$pmid)

  new_s2_pmids <- unique(s2_with_pmid$pmid)

  if (length(new_s2_pmids) > 0) {
    cli_alert_info("Fetching PubMed details for {length(new_s2_pmids)} new PMIDs from Semantic Scholar")
    s2_details <- fetch_pubmed_details(new_s2_pmids, cfg)

    if (nrow(s2_details) > 0) {
      s2_details <- s2_details |> mutate(across(everything(), as.character))
      pmid_to_abstract <- s2_with_pmid |>
        mutate(pmid = as.character(pmid)) |>
        select(pmid, abstract_id) |>
        distinct()
      s2_details <- s2_details |>
        inner_join(pmid_to_abstract, by = "pmid", relationship = "many-to-many") |>
        mutate(strategies = "semantic_scholar", n_strategies = "1") |>
        mutate(across(everything(), as.character))

      # Filter out JMIG supplement
      is_suppl <- vapply(seq_len(nrow(s2_details)), function(j) {
        is_jmig <- grepl("j minim invasive gynecol", tolower(s2_details$pub_journal_abbrev[j]), fixed = TRUE)
        is_vol <- !is.na(s2_details$pub_volume[j]) && s2_details$pub_volume[j] %in% as.character(cfg$pubmed$exclude_supplement_vol)
        is_yr <- !is.na(s2_details$pub_year[j]) && s2_details$pub_year[j] %in% as.character(cfg$pubmed$exclude_supplement_year)
        is_jmig && is_vol && is_yr
      }, logical(1))
      s2_details <- s2_details[!is_suppl, ]

      if (nrow(s2_details) > 0) {
        pubmed_candidates <- bind_rows(pubmed_candidates, s2_details) |>
          distinct(abstract_id, pmid, .keep_all = TRUE)
        new_pmid_count <- new_pmid_count + nrow(s2_details)
        cli_alert_success("Added {nrow(s2_details)} new Semantic Scholar candidates to pool")
      }
    }
  } else {
    cli_alert_info("No new PMIDs from Semantic Scholar (all already in pool)")
  }
}

# From Europe PMC
if (nrow(epmc_df) > 0) {
  epmc_with_pmid <- epmc_df |>
    filter(!is.na(pmid) & pmid != "") |>
    filter(!pmid %in% pubmed_candidates$pmid)

  new_epmc_pmids <- unique(epmc_with_pmid$pmid)

  if (length(new_epmc_pmids) > 0) {
    cli_alert_info("Fetching PubMed details for {length(new_epmc_pmids)} new PMIDs from Europe PMC")
    new_details <- fetch_pubmed_details(new_epmc_pmids, cfg)

    if (nrow(new_details) > 0) {
      new_details <- new_details |> mutate(across(everything(), as.character))
      # Map PMIDs back to abstract_ids (one PMID may match multiple abstracts)
      pmid_to_abstract <- epmc_with_pmid |>
        mutate(pmid = as.character(pmid)) |>
        select(pmid, abstract_id) |>
        distinct()
      new_details <- new_details |>
        inner_join(pmid_to_abstract, by = "pmid", relationship = "many-to-many") |>
        mutate(strategies = "europmc", n_strategies = "1") |>
        mutate(across(everything(), as.character))

      # Filter out JMIG supplement
      is_suppl <- vapply(seq_len(nrow(new_details)), function(j) {
        is_jmig <- grepl("j minim invasive gynecol", tolower(new_details$pub_journal_abbrev[j]), fixed = TRUE)
        is_vol <- !is.na(new_details$pub_volume[j]) && new_details$pub_volume[j] %in% as.character(cfg$pubmed$exclude_supplement_vol)
        is_yr <- !is.na(new_details$pub_year[j]) && new_details$pub_year[j] %in% as.character(cfg$pubmed$exclude_supplement_year)
        is_jmig && is_vol && is_yr
      }, logical(1))
      new_details <- new_details[!is_suppl, ]

      if (nrow(new_details) > 0) {
        pubmed_candidates <- bind_rows(pubmed_candidates, new_details) |>
          distinct(abstract_id, pmid, .keep_all = TRUE)
        new_pmid_count <- new_pmid_count + nrow(new_details)
        cli_alert_success("Added {nrow(new_details)} new Europe PMC candidates to pool")
      }
    }
  }
}

# From CrossRef: try DOI->PMID lookup via Europe PMC
if (nrow(cr_df) > 0) {
  cr_with_doi <- cr_df |>
    filter(!is.na(doi) & doi != "") |>
    select(abstract_id, doi) |>
    distinct()

  if (nrow(cr_with_doi) > 0) {
    cli_alert_info("Looking up {nrow(cr_with_doi)} CrossRef DOIs in Europe PMC for PMIDs")
    cr_pmids <- list()
    for (j in seq_len(nrow(cr_with_doi))) {
      doi_q <- paste0('DOI:"', cr_with_doi$doi[j], '"')
      r <- .epmc_query(doi_q, max_results = 1)
      if (nrow(r) > 0 && !is.na(r$pmid[1]) && r$pmid[1] != "") {
        cr_pmids <- c(cr_pmids, list(tibble::tibble(
          doi = cr_with_doi$doi[j],
          abstract_id = cr_with_doi$abstract_id[j],
          pmid = r$pmid[1]
        )))
      }
      Sys.sleep(0.2)
    }
    cr_pmid_df <- bind_rows(cr_pmids)

    if (nrow(cr_pmid_df) > 0) {
      # Fetch details for truly new PMIDs
      truly_new <- cr_pmid_df |>
        mutate(pmid = as.character(pmid)) |>
        filter(!pmid %in% pubmed_candidates$pmid)
      if (nrow(truly_new) > 0) {
        cr_details <- fetch_pubmed_details(unique(truly_new$pmid), cfg)
        if (nrow(cr_details) > 0) {
          cr_details <- cr_details |>
            mutate(across(everything(), as.character)) |>
            inner_join(truly_new |> select(pmid, abstract_id) |> distinct(),
                       by = "pmid", relationship = "many-to-many") |>
            mutate(strategies = "crossref_doi", n_strategies = "1") |>
            mutate(across(everything(), as.character))
          pubmed_candidates <- bind_rows(pubmed_candidates, cr_details) |>
            distinct(abstract_id, pmid, .keep_all = TRUE)
          new_pmid_count <- new_pmid_count + nrow(cr_details)
          cli_alert_success("Added {nrow(cr_details)} new CrossRef DOI-resolved candidates")
        }
      }
    }
  }
}

# From DOI-chain search (03c) — merge if output file exists
doi_chain_path <- here("data", "processed", "doi_chain_candidates.csv")
if (file.exists(doi_chain_path)) {
  doi_chain <- read_csv(doi_chain_path, show_col_types = FALSE) |>
    mutate(across(everything(), as.character))
  new_from_chain <- doi_chain |>
    filter(!pmid %in% as.character(pubmed_candidates$pmid))
  if (nrow(new_from_chain) > 0) {
    pubmed_candidates <- bind_rows(pubmed_candidates, new_from_chain) |>
      distinct(abstract_id, pmid, .keep_all = TRUE)
    new_pmid_count <- new_pmid_count + nrow(new_from_chain)
    cli_alert_success("Added {nrow(new_from_chain)} new DOI-chain candidates to pool")
  }
}

# Save updated candidates
write_csv(pubmed_candidates, here("data", "processed", "pubmed_candidates.csv"))
cli_alert_success("Supplementary search complete — {new_pmid_count} new candidates added (total: {nrow(pubmed_candidates)})")
