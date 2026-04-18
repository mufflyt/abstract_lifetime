# 10b_resolve_names_openalex.R — Resolve initials to full names via OpenAlex
#
# Every AAGL abstract has a ScienceDirect DOI. OpenAlex indexes these and
# returns full author names (not just initials). This script queries OpenAlex
# for each abstract's DOI, extracts the first author's full name, and feeds
# it back into the NPI matching pipeline.
#
# This is the key unlock: "R.S. Guido" → "Richard S. Guido" → exact NPI match.
#
# Writes: data/processed/openalex_author_names.csv
# Then re-runs 10_npi_matching.R with upgraded names.

suppressPackageStartupMessages({
  library(here); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(httr); library(jsonlite)
})

cli_h1("Resolving author names via OpenAlex DOI lookup")

abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
ua_email <- Sys.getenv("PIPELINE_EMAIL", "abstract.lifetime@example.com")

# Only process abstracts with DOIs
with_doi <- abstracts |>
  filter(!is.na(doi), nchar(doi) > 10) |>
  mutate(doi_bare = str_replace(doi, "^https?://doi\\.org/", "")) |>
  select(abstract_id, doi_bare, author_name_first)

cli_alert_info("{nrow(with_doi)} abstracts with DOIs")

# Checkpoint
checkpoint_path <- here("data", "cache", "openalex_names_checkpoint.rds")
if (file.exists(checkpoint_path)) {
  cp <- readRDS(checkpoint_path)
  completed <- cp$completed
  results <- cp$results
  cli_alert_info("Resuming from checkpoint ({length(completed)} done)")
} else {
  completed <- character()
  results <- list()
}

remaining <- with_doi |> filter(!abstract_id %in% completed)
cli_alert_info("{nrow(remaining)} remaining")

for (i in seq_len(nrow(remaining))) {
  row <- remaining[i, ]
  if (i %% 50 == 0) cli_alert_info("  {i}/{nrow(remaining)}")

  Sys.sleep(0.12)  # OpenAlex polite rate
  url <- paste0("https://api.openalex.org/works/doi:", row$doi_bare,
                "?select=authorships&mailto=", ua_email)
  resp <- tryCatch(httr::GET(url, httr::timeout(15),
                              httr::user_agent(paste0("abstract_lifetime/1.0 (mailto:", ua_email, ")"))),
                   error = function(e) NULL)

  oa_first_name <- NA_character_
  oa_full_name <- NA_character_
  oa_orcid <- NA_character_
  oa_n_authors <- NA_integer_

  if (!is.null(resp) && status_code(resp) == 200) {
    d <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")), error = function(e) NULL)
    if (!is.null(d) && !is.null(d$authorships) && is.data.frame(d$authorships) && nrow(d$authorships) > 0) {
      first_auth <- d$authorships[1, ]
      oa_full_name <- first_auth$author$display_name
      oa_orcid <- first_auth$author$orcid
      oa_n_authors <- nrow(d$authorships)

      # Extract first name from "Richard S. Guido" → "Richard"
      if (!is.na(oa_full_name)) {
        parts <- str_split(trimws(oa_full_name), "\\s+")[[1]]
        if (length(parts) >= 2) {
          oa_first_name <- parts[1]
        }
      }
    }
  }

  results[[length(results) + 1]] <- tibble(
    abstract_id = row$abstract_id,
    aagl_author = row$author_name_first,
    oa_full_name = oa_full_name %||% NA_character_,
    oa_first_name = oa_first_name %||% NA_character_,
    oa_orcid = if (is.null(oa_orcid) || length(oa_orcid) == 0) NA_character_ else oa_orcid,
    oa_n_authors = oa_n_authors %||% NA_integer_
  )

  completed <- c(completed, row$abstract_id)

  if (i %% 100 == 0) {
    saveRDS(list(completed = completed, results = results), checkpoint_path)
  }
}

saveRDS(list(completed = completed, results = results), checkpoint_path)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

oa_names <- bind_rows(results)

# Validate: does the OpenAlex first author's last name match the AAGL author?
oa_names <- oa_names |>
  mutate(
    aagl_norm = vapply(aagl_author, function(x) {
      n <- tryCatch(humaniformat::parse_names(x)$last_name, error = function(e) NA_character_)
      toupper(trimws(gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\uFE58\uFE63\uFF0D]", "-", n)))
    }, character(1)),
    oa_last = toupper(trimws(gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\uFE58\uFE63\uFF0D]", "-",
                                   str_extract(oa_full_name, "\\S+$")))),
    name_agrees = !is.na(aagl_norm) & !is.na(oa_last) & aagl_norm == oa_last
  )

n_resolved <- sum(!is.na(oa_names$oa_first_name))
n_agrees <- sum(oa_names$name_agrees, na.rm = TRUE)
cli_alert_success("Resolved {n_resolved} / {nrow(oa_names)} first names from OpenAlex")
cli_alert_info("Last name agreement (AAGL vs OpenAlex): {n_agrees} / {n_resolved} ({round(n_agrees/max(n_resolved,1)*100,1)}%)")

# Save
out_path <- here("data", "processed", "openalex_author_names.csv")
write_csv(oa_names, out_path)
cli_alert_success("Saved: {out_path}")

# Summary of what this unlocks
n_with_full_before <- sum(oa_names$abstract_id %in%
  (read_csv(here("data/processed/npi_matches.csv"), show_col_types=FALSE) |>
    filter(npi_match_strategy == "exact") |> pull(abstract_id)))
cli_alert_info("Authors that had full names before: {n_with_full_before}")
cli_alert_info("Authors that NOW have full names: {n_agrees}")
cli_alert_info("New full names available: {n_agrees - n_with_full_before}")
