# 09d_enrich_metrics.R — Fetch citation count + journal impact proxy from
# OpenAlex for every matched DOI. Adds cited_by_count and journal_impact_proxy
# (2-year mean citedness, approximating journal impact factor) to the output.
#
# Writes: data/processed/publication_metrics.csv
# Merges into: output/abstracts_with_matches.csv

suppressPackageStartupMessages({
  library(here); library(config); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(httr); library(jsonlite)
})

cfg <- config::get(file = here("config.yml"))
ua_email <- Sys.getenv("PIPELINE_EMAIL", cfg$contact_email %||% "abstract.lifetime@example.com")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

cli_h1("Citation + journal-impact enrichment via OpenAlex")

matches <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)
target_dois <- matches |>
  filter(!is.na(pub_doi), nchar(pub_doi) > 5) |>
  transmute(abstract_id, doi_bare = str_replace(pub_doi, "^https?://doi\\.org/", "")) |>
  distinct(abstract_id, .keep_all = TRUE)

cli_alert_info("DOIs to query: {nrow(target_dois)}")

oa_get <- function(url) {
  Sys.sleep(0.12)
  full <- paste0(url, if (grepl("\\?", url)) "&" else "?", "mailto=", ua_email)
  r <- tryCatch(httr::GET(full, httr::timeout(20),
    httr::user_agent(paste0("abstract_lifetime/1.0 (mailto:", ua_email, ")"))),
    error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) return(NULL)
  tryCatch(fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = TRUE),
           error = function(e) NULL)
}

results <- vector("list", nrow(target_dois))
for (i in seq_len(nrow(target_dois))) {
  row <- target_dois[i, ]
  if (i %% 100 == 0) cli_alert_info("  {i}/{nrow(target_dois)}")

  d <- oa_get(paste0("https://api.openalex.org/works/doi:", row$doi_bare,
                      "?select=cited_by_count,primary_location"))

  cited <- NA_integer_
  journal_impact <- NA_real_
  journal_name <- NA_character_

  if (!is.null(d)) {
    cited <- d$cited_by_count %||% NA_integer_

    src <- d$primary_location$source
    if (!is.null(src)) {
      journal_name <- src$display_name %||% NA_character_
      src_id <- src$id
      if (!is.null(src_id)) {
        src_id_clean <- gsub("https://openalex.org/", "", src_id)
        sd <- oa_get(paste0("https://api.openalex.org/sources/", src_id_clean,
                            "?select=summary_stats"))
        if (!is.null(sd) && !is.null(sd$summary_stats)) {
          journal_impact <- sd$summary_stats$`2yr_mean_citedness` %||% NA_real_
        }
      }
    }
  }

  results[[i]] <- tibble(
    abstract_id = row$abstract_id,
    pub_doi = row$doi_bare,
    cited_by_count = cited,
    journal_impact_proxy = journal_impact,
    journal_name_oa = journal_name
  )
}

metrics <- bind_rows(results)
write_csv(metrics, here("data", "processed", "publication_metrics.csv"))
cli_alert_success("Wrote publication_metrics.csv ({nrow(metrics)} rows)")
cli_alert_info("Citation coverage: {sum(!is.na(metrics$cited_by_count))} / {nrow(metrics)}")
cli_alert_info("Journal impact coverage: {sum(!is.na(metrics$journal_impact_proxy))} / {nrow(metrics)}")

if (sum(!is.na(metrics$cited_by_count)) > 0) {
  cli_alert_info("Citation stats: median={median(metrics$cited_by_count, na.rm=TRUE)}, mean={round(mean(metrics$cited_by_count, na.rm=TRUE),1)}, max={max(metrics$cited_by_count, na.rm=TRUE)}")
}

# Merge into abstracts_with_matches
for (c in c("cited_by_count", "journal_impact_proxy")) {
  if (c %in% names(matches)) matches[[c]] <- NULL
}
matches <- matches |>
  left_join(metrics |> select(abstract_id, cited_by_count, journal_impact_proxy),
            by = "abstract_id") |>
  # Only report citation/impact metrics for definite/probable matches.
  # For no_match abstracts, these reflect a random wrong candidate's paper.
  mutate(
    cited_by_count = if_else(classification %in% c("definite", "probable"),
                              cited_by_count, NA_integer_),
    journal_impact_proxy = if_else(classification %in% c("definite", "probable"),
                                    journal_impact_proxy, NA_real_)
  )
write_csv(matches, here("output", "abstracts_with_matches.csv"))
cli_alert_success("Merged citation metrics into abstracts_with_matches.csv (definite/probable only)")
