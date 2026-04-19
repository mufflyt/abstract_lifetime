#!/usr/bin/env Rscript
# 09f_enrich_gender_from_pubmed.R — Infer first-author gender via PubMed
#
# Strategy:
#   1. Parse last name + initials from author_name_first ("R.S. Guido" → last="Guido", init="RS")
#   2. Extract a distinctive institution keyword from affiliation_raw
#   3. Search PubMed: Guido RS[Author] AND "Pittsburgh"[Affiliation]
#   4. From returned XML, find author whose LastName+Initials match → extract ForeName
#   5. Run SSA + genderize.io on the full first name
#
# Output columns added to abstracts_with_matches.csv:
#   pubmed_full_first   — full first name recovered from PubMed (e.g., "Robert")
#   first_author_gender — male/female (filled where previously NA)
#   first_author_gender_p — probability
#
# Cache: data/cache/pubmed_author/<last>_<init>.rds
# Writes: data/processed/gender_from_pubmed.csv

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(stringr)
  library(purrr); library(cli); library(xml2); library(rentrez)
})

source(here("R", "utils_acog.R"))   # for any shared helpers

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

cache_dir <- here("data", "cache", "pubmed_author")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")
abstracts <- read_csv(abstracts_path, show_col_types = FALSE)
# NOTE: This script is a pure sidecar producer. It reads abstracts_cleaned.csv
# (not abstracts_with_matches.csv) and writes gender_from_pubmed.csv.
# 10e_merge_demographics.R handles the merge.

has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
delay   <- if (has_key) 0.11 else 0.34   # ~9/sec with key, ~3/sec without

cli_h1("Gender enrichment via PubMed author search")

#' Parse an AAGL author string into last name and initials
#'
#' @param x Character scalar. Author string (e.g., "R.S. Guido").
#' @return Named list with \code{last} (character) and \code{init} (character,
#'   periods stripped, e.g., "RS").
#' @keywords internal
parse_name <- function(x) {
  if (is.na(x) || nchar(x) < 2) return(list(last = NA_character_, init = NA_character_))
  parts <- str_squish(x) |> str_split("\\s+") |> _[[1]]
  last  <- tail(parts, 1)
  init_raw <- paste(head(parts, -1), collapse = "")
  init  <- str_remove_all(init_raw, "\\.")   # "R.S." → "RS"
  list(last = last, init = init)
}

#' Extract an institution keyword from a raw affiliation string
#'
#' Splits on "|" (first affiliation), then by comma. Skips department/
#' division prefixes and returns the first institution-looking token.
#' Used to build PubMed affiliation queries for name disambiguation.
#'
#' @param aff Character scalar. Raw affiliation string (pipe-delimited).
#' @return Character scalar. Institution name, or \code{NA_character_}.
#' @keywords internal
SKIP_PREFIXES <- paste0(
  "^(department|division|section|program|institute of|center for|",
  "obstetrics|gynecology|ob|gyn|medicine|surgery|women)"
)

extract_institution <- function(aff) {
  if (is.na(aff) || nchar(aff) < 5) return(NA_character_)
  first_aff <- str_split(aff, "\\|")[[1]][1]
  tokens    <- str_trim(str_split(first_aff, ",")[[1]])
  # Skip tokens that are just dept names, cities, states, or zip codes
  for (tok in tokens) {
    lc <- tolower(tok)
    if (str_detect(lc, SKIP_PREFIXES)) next
    if (str_detect(tok, "^[A-Z]{2}$|^\\d+$")) next    # state abbrev or zip
    if (nchar(tok) < 4) next
    return(tok)
  }
  # fallback: return city token (2nd-to-last comma piece)
  n <- length(tokens)
  if (n >= 2) tokens[max(1, n - 1)] else NA_character_
}

#' Fetch an author's full first name from PubMed
#'
#' Searches PubMed for \code{"LastName Initials"[Author]} with optional
#' institution affiliation filter. Extracts the full ForeName from the
#' first matching PubMed record's XML. Results are cached to disk.
#'
#' @param last Character. Author's last name.
#' @param init Character. Author's initials (e.g., "RS").
#' @param institution Character. Institution keyword for affiliation filter.
#' @return Character scalar. Full first name (e.g., "Richard"), or
#'   \code{NA_character_} if not found.
#' @keywords internal
fetch_full_name <- function(last, init, institution) {
  cache_key  <- paste0(tolower(last), "_", tolower(init))
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  # Build query: "Guido RS"[Author] AND "Pittsburgh"[Affiliation]
  author_term <- paste0('"', last, " ", init, '"[Author]')
  query <- if (!is.na(institution) && nchar(institution) > 3) {
    # Take first 3 words of institution to avoid over-specificity
    inst_words <- paste(str_split(institution, "\\s+")[[1]][1:min(3, str_count(institution, "\\s+") + 1)], collapse = " ")
    paste0(author_term, ' AND "', inst_words, '"[Affiliation]')
  } else {
    author_term
  }

  Sys.sleep(delay)
  ids <- tryCatch(
    entrez_search(db = "pubmed", term = query, retmax = 10)$ids,
    error = function(e) character(0)
  )
  if (length(ids) == 0) { saveRDS(NA_character_, cache_file); return(NA_character_) }

  Sys.sleep(delay)
  xml_txt <- tryCatch(
    entrez_fetch(db = "pubmed", id = ids[1:min(5, length(ids))], rettype = "xml"),
    error = function(e) NA_character_
  )
  if (is.na(xml_txt) || nchar(xml_txt) < 100) {
    saveRDS(NA_character_, cache_file); return(NA_character_)
  }

  # Parse XML: find author whose LastName + Initials match
  doc     <- tryCatch(read_xml(xml_txt), error = function(e) NULL)
  if (is.null(doc)) { saveRDS(NA_character_, cache_file); return(NA_character_) }

  authors <- xml_find_all(doc, ".//Author")
  fore    <- NA_character_
  for (auth in authors) {
    a_last <- xml_text(xml_find_first(auth, "LastName")) %||% ""
    a_init <- xml_text(xml_find_first(auth, "Initials")) %||% ""
    a_fore <- xml_text(xml_find_first(auth, "ForeName")) %||% ""
    if (tolower(a_last) == tolower(last) && tolower(a_init) == tolower(init) &&
        nchar(a_fore) >= 2) {
      fore <- str_split(str_squish(a_fore), "\\s+")[[1]][1]  # first word only
      break
    }
  }

  saveRDS(fore, cache_file)
  fore
}

# ── Build work list ────────────────────────────────────────────────────────────
# Only process abstracts that still have no gender and have affiliation_raw
targets <- abstracts |>
  filter(!is.na(author_name_first)) |>
  mutate(
    fa_last  = map_chr(author_name_first, ~ parse_name(.x)$last),
    fa_init  = map_chr(author_name_first, ~ parse_name(.x)$init),
    inst_key = map_chr(affiliation_raw,   extract_institution)
  ) |>
  filter(!is.na(fa_last), nchar(fa_last) > 1) |>
  # Only run where we have an institution keyword (otherwise too ambiguous)
  filter(!is.na(inst_key), nchar(inst_key) > 3) |>
  select(abstract_id, fa_last, fa_init, inst_key)

cli_alert_info("{nrow(targets)} abstracts with last name + institution to query")

# ── Main loop ──────────────────────────────────────────────────────────────────
n <- nrow(targets)
results <- vector("list", n)
cli_progress_bar("PubMed author search", total = n)

for (i in seq_len(n)) {
  row <- targets[i, ]
  full_first <- tryCatch(
    fetch_full_name(row$fa_last, row$fa_init, row$inst_key),
    error = function(e) NA_character_
  )
  results[[i]] <- tibble(
    abstract_id     = row$abstract_id,
    pubmed_full_first = full_first
  )
  cli_progress_update()
}
cli_progress_done()

name_tbl <- bind_rows(results) |>
  filter(!is.na(pubmed_full_first), nchar(pubmed_full_first) >= 2)

cli_alert_success("Full first names recovered: {nrow(name_tbl)} / {n} ({round(nrow(name_tbl)/n*100,1)}%)")

#' Title-case a first name for gender lookup
#' @param n Character scalar. First name.
#' @return Character scalar. Title-cased name or \code{NA_character_}.
#' @keywords internal
clean_first <- function(n) {
  if (is.na(n) || nchar(n) < 2) return(NA_character_)
  paste0(toupper(substr(n, 1, 1)), tolower(substr(n, 2, nchar(n))))
}

unique_names <- unique(name_tbl$pubmed_full_first)
unique_names <- unique_names[!is.na(unique_names) & nchar(unique_names) >= 2]

# Pass 1: SSA
ssa_result <- tryCatch(
  gender::gender(unique_names, years = c(1930, 2012), method = "ssa") |>
    select(pubmed_full_first = name, gender, proportion_male, proportion_female),
  error = function(e) { cli_alert_warning("SSA failed: {e$message}"); tibble() }
)
cli_alert_info("SSA resolved: {nrow(ssa_result)} / {length(unique_names)}")

# Pass 2: genderize.io for remainder
ssa_resolved <- if (nrow(ssa_result) > 0) tolower(ssa_result$pubmed_full_first) else character()
unresolved   <- unique_names[!tolower(unique_names) %in% ssa_resolved]

genderize_result <- tibble()
if (length(unresolved) > 0) {
  cli_alert_info("genderize.io for {length(unresolved)} remaining names...")
  api_key <- Sys.getenv("GENDERIZE_API_KEY", "")

  fetch_batch <- function(batch) {
    params <- paste0("name[]=", URLencode(batch, repeated = TRUE), collapse = "&")
    if (nchar(api_key) > 0) params <- paste0(params, "&apikey=", api_key)
    resp <- tryCatch(
      httr::GET(paste0("https://api.genderize.io/?", params), httr::timeout(15)),
      error = function(e) NULL
    )
    if (is.null(resp) || httr::status_code(resp) != 200) return(tibble())
    parsed <- tryCatch(httr::content(resp, "parsed"), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0) return(tibble())
    map(parsed, function(x) tibble(
      pubmed_full_first = x$name %||% NA_character_,
      gender            = x$gender %||% NA_character_,
      proportion_male   = if (!is.null(x$gender) && identical(x$gender, "male"))
                            x$probability %||% NA_real_ else 1 - (x$probability %||% NA_real_),
      proportion_female = if (!is.null(x$gender) && identical(x$gender, "female"))
                            x$probability %||% NA_real_ else 1 - (x$probability %||% NA_real_)
    )) |> list_rbind()
  }

  batches <- split(unresolved, ceiling(seq_along(unresolved) / 10))
  genderize_raw <- map(batches, function(b) {
    Sys.sleep(0.5)
    tryCatch(fetch_batch(b), error = function(e) tibble())
  }) |> list_rbind()
  genderize_result <- if (nrow(genderize_raw) > 0 && "pubmed_full_first" %in% names(genderize_raw)) {
    genderize_raw |> filter(!is.na(pubmed_full_first), !is.na(gender))
  } else tibble()

  cli_alert_info("genderize.io resolved: {nrow(genderize_result)} / {length(unresolved)}")
}

gender_lkp <- bind_rows(ssa_result, genderize_result) |>
  group_by(pubmed_full_first) |> slice(1) |> ungroup() |>
  mutate(
    first_author_gender   = gender,
    first_author_gender_p = pmax(proportion_male, proportion_female, na.rm = TRUE)
  ) |>
  select(pubmed_full_first, first_author_gender, first_author_gender_p)

gender_tbl <- name_tbl |>
  left_join(gender_lkp, by = "pubmed_full_first")

cli_alert_info("Gender resolved: {sum(!is.na(gender_tbl$first_author_gender))} / {nrow(gender_tbl)}")
cli_alert_info("Distribution:")
print(table(gender_tbl$first_author_gender, useNA = "ifany"))

write_csv(gender_tbl, here("data", "processed", "gender_from_pubmed.csv"))
cli_alert_success("Wrote gender_from_pubmed.csv ({nrow(gender_tbl)} rows)")

# NOTE: Merge into abstracts_with_matches.csv is handled by 10e_merge_demographics.R
# This script only writes its sidecar CSV (gender_from_pubmed.csv).
