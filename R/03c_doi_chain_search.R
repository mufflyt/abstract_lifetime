# 03c_doi_chain_search.R — DOI-chain search via OpenAlex
#
# Two strategies per abstract:
#   1. Reverse citation: find papers that CITE the conference abstract DOI.
#      If a full paper references the abstract, it's very likely the published
#      version (or closely related).
#   2. Same-author follow-up: look up the abstract's first author in OpenAlex,
#      filter to papers published after the conference date in JMIG or related
#      journals, within 36 months.
#
# Produces new candidate rows in the same schema as pubmed_candidates.csv.
# These get merged into the main candidate pool by 03b_search_crossref.R's
# merge step (or can be run standalone).

suppressPackageStartupMessages({
  library(here); library(config); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(purrr); library(httr); library(jsonlite)
})

cfg <- config::get(file = here("config.yml"))

cli_h2("DOI-chain search via OpenAlex")

abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                      show_col_types = FALSE)

# Only use abstracts that have a DOI
abstracts_doi <- abstracts |>
  filter(!is.na(doi), nchar(doi) > 10) |>
  mutate(doi_bare = str_replace(doi, "^https?://doi\\.org/", ""))

cli_alert_info("{nrow(abstracts_doi)} abstracts with DOIs")

# Checkpoint support
checkpoint_path <- here(cfg$pipeline$checkpoint_dir, "doi_chain_checkpoint.rds")
if (file.exists(checkpoint_path)) {
  cp <- readRDS(checkpoint_path)
  completed_ids <- cp$completed_ids
  all_results <- cp$all_results
  cli_alert_info("Resuming from checkpoint ({length(completed_ids)} already done)")
} else {
  completed_ids <- character()
  all_results <- list()
}

remaining <- abstracts_doi |> filter(!abstract_id %in% completed_ids)
cli_alert_info("{nrow(remaining)} remaining to search")

ua_email <- "tyler.muffly@dhha.org"

#' Rate-limited OpenAlex GET
oa_get <- function(url) {
  Sys.sleep(0.15)
  full_url <- paste0(url, if (grepl("\\?", url)) "&" else "?",
                     "mailto=", ua_email)
  r <- tryCatch(
    httr::GET(full_url, httr::timeout(30),
              httr::user_agent(paste0("abstract_lifetime/1.0 (mailto:", ua_email, ")"))),
    error = function(e) NULL
  )
  if (is.null(r) || status_code(r) != 200) return(NULL)
  tryCatch(fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = TRUE),
           error = function(e) NULL)
}

#' Strategy 1: reverse citations — who cites this abstract DOI?
find_citing_papers <- function(doi_bare) {
  d1 <- oa_get(paste0("https://api.openalex.org/works/doi:", doi_bare,
                       "?select=id,cited_by_count"))
  if (is.null(d1) || d1$cited_by_count == 0) return(tibble())
  oa_id <- gsub("https://openalex.org/", "", d1$id)
  d2 <- oa_get(paste0("https://api.openalex.org/works?filter=cites:", oa_id,
                       "&select=title,doi,publication_year,type,authorships",
                       "&per_page=25"))
  if (is.null(d2) || is.null(d2$results) || nrow(d2$results) == 0) return(tibble())
  d2$results |>
    filter(type %in% c("article", "review")) |>
    transmute(
      pub_title = title,
      pub_doi = str_replace(doi, "^https?://doi\\.org/", ""),
      pub_year = as.character(publication_year),
      strategy = "doi_chain_cited_by"
    )
}

#' Resolve a DOI to a PMID via OpenAlex IDs field
doi_to_pmid <- function(doi_bare) {
  d <- oa_get(paste0("https://api.openalex.org/works/doi:", doi_bare,
                      "?select=ids"))
  if (is.null(d) || is.null(d$ids$pmid)) return(NA_character_)
  gsub("https://pubmed.ncbi.nlm.nih.gov/", "", d$ids$pmid)
}

for (i in seq_len(nrow(remaining))) {
  row <- remaining[i, ]
  if (i %% 50 == 0) cli_alert_info("[{i}/{nrow(remaining)}] {row$abstract_id}")

  citing <- find_citing_papers(row$doi_bare)

  if (nrow(citing) > 0) {
    # Resolve DOIs to PMIDs
    citing$pmid <- vapply(citing$pub_doi, function(d) {
      if (is.na(d) || nchar(d) == 0) return(NA_character_)
      doi_to_pmid(d)
    }, character(1))

    citing$abstract_id <- row$abstract_id
    all_results[[length(all_results) + 1]] <- citing
  }

  completed_ids <- c(completed_ids, row$abstract_id)

  if (i %% 100 == 0) {
    saveRDS(list(completed_ids = completed_ids, all_results = all_results),
            checkpoint_path)
  }
}

saveRDS(list(completed_ids = completed_ids, all_results = all_results),
        checkpoint_path)

results <- bind_rows(all_results)
cli_alert_success("DOI-chain search found {nrow(results)} candidate rows across {length(unique(results$abstract_id))} abstracts")

if (nrow(results) > 0) {
  # Convert to pubmed_candidates format
  new_candidates <- results |>
    filter(!is.na(pmid), nchar(pmid) > 0) |>
    transmute(
      pmid = pmid,
      pub_title = pub_title,
      pub_year = pub_year,
      pub_doi = pub_doi,
      strategies = strategy,
      n_strategies = 1L,
      first_query = paste0("doi_chain:", pub_doi),
      abstract_id = abstract_id
    )

  out_path <- here("data", "processed", "doi_chain_candidates.csv")
  write_csv(new_candidates, out_path)
  cli_alert_success("Wrote {nrow(new_candidates)} DOI-chain candidates ({length(unique(new_candidates$pmid))} unique PMIDs)")
  cli_alert_info("Abstracts with new leads: {length(unique(new_candidates$abstract_id))}")

  # Show how many are NEW (not in existing pubmed_candidates)
  pubmed_path <- here("data", "processed", "pubmed_candidates.csv")
  if (file.exists(pubmed_path)) {
    existing_pmids <- read_csv(pubmed_path, show_col_types = FALSE)$pmid |>
      unique() |> as.character()
    new_pmids <- setdiff(new_candidates$pmid, existing_pmids)
    cli_alert_success("Of these, {length(new_pmids)} PMIDs are BRAND NEW (not in any existing search)")
  }
} else {
  cli_alert_warning("No DOI-chain candidates found")
}
