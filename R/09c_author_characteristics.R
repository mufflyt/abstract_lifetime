# 09c_author_characteristics.R — Aggregate per-abstract author characteristics
# from the PubMed-sourced author roster + original AAGL listing.
#
# Output columns (per abstract_id):
#   n_authors              — count from PubMed author list (where available)
#   n_authors_aagl         — count from AAGL "authors_raw" comma-split fallback
#   n_unique_affiliations  — distinct affiliation strings across authors
#   first_author_last      — PubMed surname (first position)
#   first_author_first     — PubMed given name (first position)
#   first_author_state     — parsed 2-letter state from first author's affiliation
#   first_author_country   — parsed country from first author's affiliation
#   first_author_acog_district — mapped from state
#   first_author_gender    — inferred from given name via Social Security data
#   first_author_gender_p  — probability / confidence of gender call
#
# Writes: data/processed/author_characteristics.csv
# Also merges those columns into output/abstracts_with_matches.csv.

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(stringr)
  library(tibble); library(purrr); library(cli); library(httr)
})

source(here("R", "utils_acog.R"))
source(here("R", "utils_states.R"))
source(here("R", "utils_affiliation.R"))

authors_path <- here("data", "processed", "authors_pubmed.csv")
matches_path <- here("output", "abstracts_with_matches.csv")
abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")

authors <- read_csv(authors_path, show_col_types = FALSE)
matches <- read_csv(matches_path, show_col_types = FALSE)
abstracts <- read_csv(abstracts_path, show_col_types = FALSE)

cli_h1("Author characteristics")

# --- Gender inference: two-pass strategy ---
# Pass 1: SSA (US names, high accuracy)
# Pass 2: genderize.io (international names, broader coverage)
# Name cleaning: strip middle initials ("Donald I" → "Donald"),
# take first word of compound names ("Jong Woon" → "Jong").

clean_first_name <- function(n) {
  if (is.na(n) || nchar(n) < 2) return(NA_character_)
  n <- trimws(n)
  n <- sub("\\s+[A-Z]$", "", n)        # trailing single initial
  n <- sub("\\s+[A-Z]\\.$", "", n)     # trailing initial with period
  parts <- strsplit(n, "\\s+")[[1]]
  n <- parts[1]
  if (is.na(n) || nchar(n) < 2) return(NA_character_)
  paste0(toupper(substr(n, 1, 1)), tolower(substr(n, 2, nchar(n))))
}

first_names <- authors |>
  filter(position == 1, !is.na(first_name), nchar(first_name) >= 2) |>
  distinct(first_name) |>
  mutate(name_clean = vapply(first_name, clean_first_name, character(1))) |>
  filter(!is.na(name_clean))

# Pass 1: SSA (US Social Security data, 1930-2012)
ssa_result <- tryCatch({
  gender::gender(unique(first_names$name_clean),
                 years = c(1930, 2012), method = "ssa") |>
    select(name_clean = name, gender, proportion_male, proportion_female)
}, error = function(e) {
  cli_alert_warning("SSA gender failed: {e$message}")
  tibble()
})
cli_alert_info("SSA resolved: {nrow(ssa_result)} / {length(unique(first_names$name_clean))}")

# Pass 2: genderize.io for names SSA missed
# Uses GENDERIZE_API_KEY env var if set (higher rate limit: 1000/day free, unlimited paid).
# Batches requests at 10 names per call to stay within the API's URL length limits.
ssa_names <- if (nrow(ssa_result) > 0) tolower(ssa_result$name_clean) else character()
unresolved <- unique(first_names$name_clean[!tolower(first_names$name_clean) %in% ssa_names])
unresolved <- unresolved[!is.na(unresolved) & nchar(unresolved) >= 2]

genderize_result <- tibble()
if (length(unresolved) > 0) {
  cli_alert_info("Querying genderize.io for {length(unresolved)} remaining names...")
  api_key <- Sys.getenv("GENDERIZE_API_KEY", "")

  fetch_genderize_batch <- function(names_batch) {
    params <- paste0("name[]=", URLencode(names_batch), collapse = "&")
    if (nchar(api_key) > 0) params <- paste0(params, "&apikey=", api_key)
    url  <- paste0("https://api.genderize.io/?", params)
    resp <- tryCatch(httr::GET(url, httr::timeout(15)), error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) return(tibble())
    parsed <- tryCatch(httr::content(resp, "parsed"), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0) return(tibble())
    purrr::map(parsed, function(x) {
      tibble(
        name_clean         = x$name %||% NA_character_,
        gender             = x$gender %||% NA_character_,
        probability        = x$probability %||% NA_real_,
        proportion_male    = if (!is.null(x$gender) && !is.na(x$gender) && x$gender == "male")
                               x$probability else 1 - (x$probability %||% NA_real_),
        proportion_female  = if (!is.null(x$gender) && !is.na(x$gender) && x$gender == "female")
                               x$probability else 1 - (x$probability %||% NA_real_)
      )
    }) |> purrr::list_rbind()
  }

  batches <- split(unresolved, ceiling(seq_along(unresolved) / 10))
  genderize_rows <- vector("list", length(batches))
  for (i in seq_along(batches)) {
    genderize_rows[[i]] <- tryCatch(
      fetch_genderize_batch(batches[[i]]),
      error = function(e) { cli_alert_warning("genderize batch {i} failed: {e$message}"); tibble() }
    )
    Sys.sleep(0.5)  # polite: free tier allows ~1 req/sec
  }
  genderize_result <- tryCatch({
    bound <- bind_rows(genderize_rows)
    if (nrow(bound) > 0 && all(c("name_clean", "gender") %in% names(bound))) {
      bound |>
        filter(!is.na(name_clean), !is.na(gender)) |>
        select(name_clean, gender, proportion_male, proportion_female)
    } else tibble()
  }, error = function(e) {
    cli_alert_warning("genderize.io result merge failed: {e$message}")
    tibble()
  })

  cli_alert_info("genderize.io resolved: {nrow(genderize_result)} / {length(unresolved)}")
  if (nchar(api_key) == 0)
    cli_alert_info("Tip: set GENDERIZE_API_KEY in .Renviron for higher rate limits")
}

# Pass 3: Static international name lookup (curated list of ~150 Chinese,
# Korean, Japanese, Indian, European, Turkish, Arabic names common in
# medical publishing). Covers names that SSA and genderize.io miss.
intl_path <- here("data", "validation", "international_gender_lookup.csv")
intl_result <- tibble()
if (file.exists(intl_path)) {
  intl_lkp <- read_csv(intl_path, show_col_types = FALSE) |>
    mutate(name_clean = paste0(toupper(substr(name, 1, 1)),
                               tolower(substr(name, 2, nchar(name))))) |>
    transmute(name_clean, gender,
              proportion_male = if_else(gender == "male", 0.95, 0.05),
              proportion_female = if_else(gender == "female", 0.95, 0.05))

  # Only use for names not already resolved by SSA or genderize
  resolved_so_far <- c(
    if (nrow(ssa_result) > 0) tolower(ssa_result$name_clean) else character(),
    if (nrow(genderize_result) > 0) tolower(genderize_result$name_clean) else character()
  )
  intl_result <- intl_lkp |>
    filter(!tolower(name_clean) %in% resolved_so_far)
  cli_alert_info("International lookup resolved: {nrow(intl_result)} additional names")
}

# Combine all three passes (SSA > genderize > international static)
gender_combined <- bind_rows(ssa_result, genderize_result, intl_result) |>
  group_by(name_clean) |>
  slice(1) |>
  ungroup()

gender_lkp <- first_names |>
  left_join(gender_combined, by = "name_clean") |>
  mutate(
    first_author_gender = gender,
    first_author_gender_p = pmax(proportion_male, proportion_female, na.rm = TRUE)
  ) |>
  select(first_name, first_author_gender, first_author_gender_p)

cli_alert_info("Total gender coverage: {sum(!is.na(gender_lkp$first_author_gender))} / {nrow(gender_lkp)} ({round(mean(!is.na(gender_lkp$first_author_gender))*100,1)}%)")

# --- Per-abstract aggregation ---
first_auth <- authors |>
  filter(position == 1) |>
  group_by(abstract_id) |>
  slice(1) |>
  ungroup() |>
  transmute(
    abstract_id,
    first_author_last  = last_name,
    first_author_first = first_name,
    first_author_state_raw = affiliation_state,
    first_author_aff = affiliation,
    first_author_all_aff = all_affiliations,
    first_author_country = affiliation_country
  ) |>
  # Re-parse state with the improved multi-strategy parser if raw is NA
  mutate(
    first_author_state = if_else(
      !is.na(first_author_state_raw), first_author_state_raw,
      vapply(first_author_aff, parse_us_state, character(1))
    )
  ) |>
  select(-first_author_state_raw) |>
  left_join(gender_lkp, by = c("first_author_first" = "first_name")) |>
  mutate(
    first_author_acog_district = mapply(function(state, aff) {
      d <- acog_district_for_state(state)
      if (!is.na(d)) return(d)
      country <- parse_country(aff)
      if (!is.na(country) && country != "USA") country else NA_character_
    }, first_author_state, first_author_aff, USE.NAMES = FALSE),
    practice_type = mapply(classify_practice_type, first_author_aff, first_author_all_aff,
                           first_author_country, USE.NAMES = FALSE),
    subspecialty = vapply(first_author_aff, classify_subspecialty, character(1)),
    career_stage = vapply(first_author_aff, classify_career_stage, character(1))
  ) |>
  select(-first_author_aff, -first_author_all_aff)

agg <- authors |>
  group_by(abstract_id) |>
  summarise(
    n_authors = n_distinct(position),  # from PubMed matched pub (fallback only)
    n_unique_affiliations = n_distinct(affiliation[!is.na(affiliation) & nchar(affiliation) > 0]),
    .groups = "drop"
  )

# AAGL-side author count (fallback — works for every abstract even if no PMID match)
aagl_counts <- abstracts |>
  mutate(n_authors_aagl = purrr::map_int(authors_raw, function(s) {
    if (is.na(s) || nchar(s) == 0) return(NA_integer_)
    # Split on commas; if ellipsis present (", ... "), the count is lower bound.
    toks <- str_trim(str_split(s, ",\\s*")[[1]])
    sum(nchar(toks) > 0 & !str_detect(toks, "^\\.\\.\\.$"))
  })) |>
  select(abstract_id, n_authors_aagl)

char_tbl <- abstracts |>
  select(abstract_id) |>
  left_join(agg, by = "abstract_id") |>
  left_join(aagl_counts, by = "abstract_id") |>
  left_join(first_auth, by = "abstract_id") |>
  mutate(
    # Use AAGL author count only (from the abstract itself).
    # PubMed n_authors is from the matched publication — a different paper
    # with a different author list. Never use it.
    n_authors = n_authors_aagl
  )

write_csv(char_tbl, here("data", "processed", "author_characteristics.csv"))
cli_alert_success("Wrote author_characteristics.csv ({nrow(char_tbl)} rows)")

cli_alert_info("ACOG district distribution:")
print(table(char_tbl$first_author_acog_district, useNA = "ifany"))

cli_alert_info("Gender distribution:")
print(table(char_tbl$first_author_gender, useNA = "ifany"))

cli_alert_info("Practice type:")
print(table(char_tbl$practice_type, useNA = "ifany"))

cli_alert_info("Subspecialty:")
print(table(char_tbl$subspecialty, useNA = "ifany"))

cli_alert_info("Career stage:")
print(table(char_tbl$career_stage, useNA = "ifany"))

# NOTE: Merge into abstracts_with_matches.csv and conditional blanking are now
# handled by 10e_merge_demographics.R. This script only writes its sidecar CSV
# (author_characteristics.csv). The blanking of PubMed-derived demographics for
# no_match/possible/no_candidates is done reversibly in the orchestrator.
cli_alert_success("author_characteristics.csv ready for merge by 10e_merge_demographics.R")
