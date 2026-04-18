# 09_enrich_authors.R — Stage 1 author enrichment
#
# For every PMID associated with an AAGL 2023 abstract (best_pmid in
# review_queue plus any reviewer-overridden manual_pmid), fetch the PubMed XML
# record and extract full author names + affiliations. PubMed's web view only
# shows initials, but the EFetch XML includes <ForeName> and <Affiliation>.
#
# Writes: data/processed/authors_pubmed.csv  (one row per author per PMID)
# Cache:  data/cache/pubmed_xml/<pmid>.xml   (per-PMID, resumable)

suppressPackageStartupMessages({
  library(here)
  library(config)
  library(cli)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(xml2)
  library(rentrez)
})

cfg <- config::get(file = here("config.yml"))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

cache_dir <- here("data", "cache", "pubmed_xml")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

out_path <- here("data", "processed", "authors_pubmed.csv")

# ---- Collect target PMIDs ------------------------------------------------

matches_path   <- here("output", "abstracts_with_matches.csv")
decisions_path <- here("output", "manual_review_decisions.csv")

matches <- if (file.exists(matches_path)) {
  read_csv(matches_path, show_col_types = FALSE)
} else tibble()

decisions <- if (file.exists(decisions_path)) {
  read_csv(decisions_path, show_col_types = FALSE)
} else tibble()

targets <- tibble(abstract_id = character(), pmid = character())

# Every abstract's top PubMed candidate — pull authors regardless of match
# classification so we can build a roster for the full AAGL 2023 set.
if (nrow(matches) > 0 && "best_pmid" %in% names(matches)) {
  targets <- bind_rows(targets, matches |>
    filter(!is.na(best_pmid), nchar(as.character(best_pmid)) > 0) |>
    transmute(abstract_id, pmid = as.character(best_pmid)))
}

# Reviewer-overridden PMIDs (may differ from best_pmid)
if (nrow(decisions) > 0 && "manual_pmid" %in% names(decisions)) {
  dec_pmids <- decisions |>
    filter(!is.na(manual_pmid), nchar(as.character(manual_pmid)) > 0) |>
    transmute(abstract_id, pmid = as.character(manual_pmid))
  targets <- bind_rows(targets, dec_pmids)
}

targets <- targets |>
  distinct(abstract_id, pmid) |>
  filter(grepl("^[0-9]+$", pmid))

cli_h1("Stage 1: Author enrichment from PubMed")
cli_alert_info("Unique PMID targets: {nrow(targets)} across {length(unique(targets$abstract_id))} abstracts")

if (nrow(targets) == 0) {
  cli_alert_warning("No PMIDs to enrich; exiting.")
  return(invisible(NULL))
}

# ---- Fetch XML (cached) --------------------------------------------------

has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
delay   <- if (has_key) 1 / cfg$pubmed$rate_limit_with_key else 1 / cfg$pubmed$rate_limit_per_sec

fetch_xml <- function(pmid) {
  path <- file.path(cache_dir, paste0(pmid, ".xml"))
  if (file.exists(path) && file.info(path)$size > 100) {
    return(read_file(path))
  }
  Sys.sleep(delay)
  raw <- tryCatch(
    rentrez::entrez_fetch(db = "pubmed", id = pmid, rettype = "xml"),
    error = function(e) {
      cli_alert_warning("fetch failed for PMID {pmid}: {e$message}")
      NA_character_
    }
  )
  if (!is.na(raw) && nchar(raw) > 100) write_file(raw, path)
  raw
}

# ---- Parse one PubmedArticle XML into author rows ------------------------

source(here("R", "utils_states.R"))

parse_affiliation <- function(aff) {
  if (is.na(aff) || nchar(aff) == 0) {
    return(list(city = NA_character_, state = NA_character_, country = NA_character_))
  }
  parts <- str_split(aff, ",\\s*")[[1]] |> str_squish()
  country <- if (length(parts) >= 1) tail(parts, 1) else NA_character_

  # Use the improved multi-strategy state parser
  state <- parse_us_state(aff)

  city <- if (length(parts) >= 2) parts[max(1, length(parts) - 2)] else NA_character_
  list(city = city, state = state, country = country)
}

parse_authors <- function(xml_text, pmid, abstract_id) {
  if (is.na(xml_text) || nchar(xml_text) < 100) return(tibble())
  doc <- tryCatch(read_xml(xml_text), error = function(e) NULL)
  if (is.null(doc)) return(tibble())

  art <- xml_find_first(doc, "//PubmedArticle")
  if (length(art) == 0 || is.na(art)) return(tibble())

  nodes <- xml_find_all(art, ".//AuthorList/Author")
  if (length(nodes) == 0) return(tibble())

  purrr::map(seq_along(nodes), function(i) {
    a <- nodes[[i]]
    last <- xml_text(xml_find_first(a, "LastName"))
    fore <- xml_text(xml_find_first(a, "ForeName"))
    init <- xml_text(xml_find_first(a, "Initials"))
    coll <- xml_text(xml_find_first(a, "CollectiveName"))
    affs <- xml_text(xml_find_all(a, "AffiliationInfo/Affiliation"))
    aff_combined <- if (length(affs) > 0) paste(affs, collapse = " | ") else NA_character_
    primary_aff <- if (length(affs) > 0) affs[1] else NA_character_
    loc <- parse_affiliation(primary_aff)

    tibble(
      abstract_id        = abstract_id,
      pmid               = pmid,
      position           = i,
      last_name          = last %||% NA_character_,
      first_name         = fore %||% NA_character_,
      initials           = init %||% NA_character_,
      collective_name    = coll %||% NA_character_,
      affiliation        = primary_aff %||% NA_character_,
      all_affiliations   = aff_combined,
      affiliation_city   = loc$city,
      affiliation_state  = loc$state,
      affiliation_country = loc$country
    )
  }) |> purrr::list_rbind()
}

# ---- Main loop -----------------------------------------------------------

cli_progress_bar("Fetching PubMed XML", total = nrow(targets))
author_rows <- vector("list", nrow(targets))
for (i in seq_len(nrow(targets))) {
  row <- targets[i, ]
  xml_text <- fetch_xml(row$pmid)
  author_rows[[i]] <- parse_authors(xml_text, row$pmid, row$abstract_id)
  cli_progress_update()
}
cli_progress_done()

authors <- bind_rows(author_rows) |>
  mutate(across(where(is.character), str_squish))

# Tag with match classification + reviewer decision so Stage 2 can filter to
# confirmed matches (authors of rejected PMIDs are for the wrong paper).
if (nrow(matches) > 0 && "classification" %in% names(matches)) {
  authors <- authors |> left_join(
    matches |> select(abstract_id, classification, best_score),
    by = "abstract_id"
  )
}
if (nrow(decisions) > 0) {
  latest_dec <- decisions |>
    group_by(abstract_id) |>
    arrange(desc(review_timestamp)) |>
    slice(1) |>
    ungroup() |>
    select(abstract_id, manual_decision)
  authors <- authors |> left_join(latest_dec, by = "abstract_id")
} else {
  authors$manual_decision <- NA_character_
}

authors <- authors |> mutate(
  is_confirmed_match = (classification %in% c("definite", "probable")) |
                       (manual_decision %in% "match")
)

cli_alert_success("Parsed {nrow(authors)} author rows ({length(unique(paste(authors$last_name, authors$first_name)))} unique name pairs)")
cli_alert_info("Confirmed-match author rows: {sum(authors$is_confirmed_match, na.rm=TRUE)} of {nrow(authors)}")

write_csv(authors, out_path)
cli_alert_success("Wrote {out_path}")

# Coverage summary
pct_full_name <- mean(!is.na(authors$first_name) & nchar(authors$first_name) > 1) * 100
pct_aff       <- mean(!is.na(authors$affiliation)) * 100
pct_us        <- mean(authors$affiliation_country %in% c("USA", "United States") |
                        !is.na(authors$affiliation_state), na.rm = TRUE) * 100
cli_alert_info("Coverage — full first names: {round(pct_full_name,1)}% | affiliations: {round(pct_aff,1)}% | US-inferred: {round(pct_us,1)}%")
