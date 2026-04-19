# 10_npi_matching.R — NPI matching for AAGL abstract first authors
#
# Matches US-based first authors to National Provider Identifiers using the
# isochrones ABOG-NPI canonical dataset (60,846 board-certified OB/GYNs with
# full names, NPIs, states, gender, and subspecialties). This is a much
# smaller and more relevant pool than raw NPPES (213K records).
#
# Huge bonus for subspecialty == MIG (MIGS) since AAGL is the MIGS society.
#
# Uses 3-tier classification (high/ambiguous/no match) with conservative scoring.
# Precision > recall.
#
# Requires: isochrones canonical ABOG-NPI file
# Writes: data/processed/npi_matches.csv

suppressPackageStartupMessages({
  library(here); library(config); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(humaniformat)
})

source(here("R", "utils_text.R"))
source(here("R", "utils_states.R"))
source(here("R", "utils_acog.R"))

#' Normalize Unicode dashes/hyphens to ASCII
#'
#' Replaces Unicode dash variants (U+2010 through U+FF0D) with the standard
#' ASCII hyphen-minus (U+002D). Critical for matching author names like
#' "Al\u2010Hendy" against ABOG pool entries stored with ASCII hyphens.
#'
#' @param x Character vector to normalize.
#' @return Character vector with all Unicode dashes replaced by ASCII hyphen.
#' @keywords internal
normalize_dashes <- function(x) {
  gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\uFE58\uFE63\uFF0D]", "-", x)
}

cfg <- config::get(file = here("config.yml"))

cli_h1("NPI Matching for AAGL First Authors")

# ---- Load ABOG-NPI pool ----
abog_path <- "/Users/tylermuffly/isochrones/data/canonical_abog/canonical_abog_npi_LATEST.csv"
if (!file.exists(abog_path)) {
  cli_alert_danger("ABOG-NPI file not found at {abog_path}")
  invisible(NULL)
}

pool <- read_csv(abog_path, show_col_types = FALSE) |>
  filter(!is.na(first_name), !is.na(last_name), nchar(first_name) > 0) |>
  transmute(
    npi = as.character(npi),
    pool_first = toupper(trimws(normalize_dashes(first_name))),
    pool_last = toupper(trimws(normalize_dashes(last_name))),
    pool_middle_initial = toupper(substr(trimws(coalesce(middle_name, npi_middle_name, middle, "")), 1, 1)),
    pool_gender = npi_gender,
    pool_state = state,
    pool_subspecialty = subspecialty,
    pool_first_initial = substr(pool_first, 1, 1)
  ) |>
  mutate(pool_middle_initial = if_else(pool_middle_initial == "", NA_character_, pool_middle_initial)) |>
  filter(!is.na(pool_last), nchar(pool_last) > 0)

cli_alert_info("ABOG-NPI pool: {nrow(pool)} physicians ({length(unique(pool$npi[!is.na(pool$npi)]))} with NPIs)")
cli_alert_info("Subspecialties: {paste(names(table(pool$pool_subspecialty)), collapse=', ')}")
cli_alert_info("MIG (MIGS): {sum(pool$pool_subspecialty == 'MIG', na.rm=TRUE)}")

# Precompute name frequency for rarity bonus
name_freq <- pool |>
  count(pool_last, pool_first_initial, name = "n_in_pool")

# ---- Load abstract data ----
cli_h2("Building author lookup table")

abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
char <- read_csv(here("data", "processed", "author_characteristics.csv"), show_col_types = FALSE)
matches_csv <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)

# OpenAlex full names (resolves initials for ~1000 authors)
oa_names <- tryCatch({
  oa <- read_csv(here("data", "processed", "openalex_author_names.csv"), show_col_types = FALSE)
  oa |> filter(name_agrees == TRUE, !is.na(oa_first_name), nchar(oa_first_name) > 1) |>
    select(abstract_id, oa_first_name, oa_full_name)
}, error = function(e) tibble(abstract_id = character()))
cli_alert_info("OpenAlex full names available: {nrow(oa_names)}")

# PubMed full names for confirmed matches
pubmed_fa <- tryCatch({
  a <- read_csv(here("data", "processed", "authors_pubmed.csv"), show_col_types = FALSE)
  a |>
    filter(position == 1) |>
    group_by(abstract_id) |> slice(1) |> ungroup() |>
    inner_join(
      matches_csv |> filter(classification %in% c("definite", "probable")) |> select(abstract_id),
      by = "abstract_id"
    ) |>
    select(abstract_id, pubmed_first_name = first_name, pubmed_last_name = last_name,
           pubmed_city = affiliation_city, pubmed_state = affiliation_state,
           pubmed_aff = affiliation)
}, error = function(e) tibble(abstract_id = character()))

# Additional full names from gender enrichment sidecars
g_pub_names <- tryCatch({
  read_csv(here("data", "processed", "gender_from_pubmed.csv"), show_col_types = FALSE) |>
    filter(!is.na(pubmed_full_first), nchar(pubmed_full_first) > 1) |>
    select(abstract_id, g_pub_first = pubmed_full_first) |>
    distinct(abstract_id, .keep_all = TRUE)
}, error = function(e) tibble(abstract_id = character()))
cli_alert_info("PubMed full name search names: {nrow(g_pub_names)}")

g_obg_names <- tryCatch({
  read_csv(here("data", "processed", "gender_from_obgyn_pubs.csv"), show_col_types = FALSE) |>
    filter(!is.na(obgyn_first_name), nchar(obgyn_first_name) > 1) |>
    select(abstract_id, g_obg_first = obgyn_first_name) |>
    distinct(abstract_id, .keep_all = TRUE)
}, error = function(e) tibble(abstract_id = character()))
cli_alert_info("OB/GYN pubs names: {nrow(g_obg_names)}")

g_oax_names <- tryCatch({
  read_csv(here("data", "processed", "gender_from_openalex.csv"), show_col_types = FALSE) |>
    filter(!is.na(openalex_first_name), nchar(openalex_first_name) > 1) |>
    select(abstract_id, g_oax_first = openalex_first_name) |>
    distinct(abstract_id, .keep_all = TRUE)
}, error = function(e) tibble(abstract_id = character()))
cli_alert_info("OpenAlex search names: {nrow(g_oax_names)}")

g_opm_names <- tryCatch({
  read_csv(here("data", "processed", "gender_from_open_payments.csv"), show_col_types = FALSE) |>
    filter(!is.na(op_first_name), nchar(op_first_name) > 1) |>
    select(abstract_id, g_opm_first = op_first_name) |>
    distinct(abstract_id, .keep_all = TRUE)
}, error = function(e) tibble(abstract_id = character()))
cli_alert_info("Open Payments names: {nrow(g_opm_names)}")

g_orc_names <- tryCatch({
  read_csv(here("data", "processed", "gender_from_orcid.csv"), show_col_types = FALSE) |>
    filter(!is.na(orcid_first_name), nchar(orcid_first_name) > 1) |>
    select(abstract_id, g_orc_first = orcid_first_name) |>
    distinct(abstract_id, .keep_all = TRUE)
}, error = function(e) tibble(abstract_id = character()))
cli_alert_info("ORCID names: {nrow(g_orc_names)}")

# Build lookup
lookup <- abstracts |>
  filter(is_us_based == TRUE) |>
  select(abstract_id, congress_year, author_name_first, primary_procedure) |>
  mutate(
    # Use humaniformat for robust name parsing (handles "El Hachem", "Chapman-Davis", etc.)
    parsed = humaniformat::parse_names(author_name_first),
    last_name = trimws(parsed$last_name),
    first_initial = toupper(substr(trimws(parsed$first_name), 1, 1)),
    # Middle initial: from humaniformat or from "C.J. Min" pattern
    middle_initial = coalesce(
      toupper(substr(trimws(parsed$middle_name), 1, 1)),
      {
        # Extract 2nd initial from patterns like "C.J. Min", "E.G. Braxton"
        inits <- str_extract_all(author_name_first, "[A-Z](?=[. ])")
        sapply(inits, function(p) if (length(p) >= 2) p[2] else NA_character_)
      }
    ),
    # Fallback to normalize_author for edge cases
    author_norm = vapply(author_name_first, normalize_author, character(1)),
    last_name = if_else(is.na(last_name) | nchar(last_name) == 0,
                        trimws(str_extract(author_norm, "^[a-z][a-z '-]+")),
                        last_name),
    first_initial = if_else(is.na(first_initial) | nchar(first_initial) == 0,
                            toupper(str_extract(author_norm, "[A-Za-z]$")),
                            first_initial)
  ) |>
  select(-parsed, -author_norm) |>
  filter(!is.na(last_name), !is.na(first_initial), nchar(last_name) > 0) |>
  left_join(pubmed_fa, by = "abstract_id") |>
  left_join(oa_names, by = "abstract_id") |>
  left_join(g_pub_names, by = "abstract_id") |>
  left_join(g_obg_names, by = "abstract_id") |>
  left_join(g_oax_names, by = "abstract_id") |>
  left_join(g_opm_names, by = "abstract_id") |>
  left_join(g_orc_names, by = "abstract_id") |>
  mutate(
    pubmed_name_matches_aagl = !is.na(pubmed_last_name) &
      tolower(trimws(pubmed_last_name)) == tolower(trimws(last_name)),
    # Full name priority: OpenAlex > PubMed confirmed > gender enrichment sidecars
    full_first_name = normalize_dashes(coalesce(
      oa_first_name,
      if_else(pubmed_name_matches_aagl, pubmed_first_name, NA_character_),
      g_pub_first,
      g_obg_first,
      g_oax_first,
      g_opm_first,
      g_orc_first
    )),
    has_full_name = !is.na(full_first_name) & nchar(full_first_name) > 1,
    last_name_upper = toupper(normalize_dashes(last_name))
  ) |>
  left_join(
    char |> select(abstract_id, first_author_state, first_author_gender),
    by = "abstract_id"
  ) |>
  mutate(
    state_hint = coalesce(
      if_else(pubmed_name_matches_aagl, pubmed_state, NA_character_),
      if_else(pubmed_name_matches_aagl,
              vapply(coalesce(pubmed_aff, ""), parse_us_state, character(1)),
              NA_character_),
      first_author_state
    ),
    city_hint = if_else(pubmed_name_matches_aagl, pubmed_city, NA_character_)
  )

n_us <- nrow(lookup)
n_full <- sum(lookup$has_full_name)
cli_alert_info("US-based authors: {n_us}")
cli_alert_info("  With full first name: {n_full}")
cli_alert_info("  With initial only: {n_us - n_full}")

# ---- Match against ABOG pool ----
cli_h2("Matching against ABOG-NPI pool")

#' Score an author against the ABOG-NPI candidate pool
#'
#' Matches a single AAGL abstract author against all ABOG-NPI pool entries
#' sharing the same last name (and first name or initial). Returns the best
#' candidate with a confidence classification (high/ambiguous/low).
#'
#' @param author_row A single-row tibble from the lookup table with columns:
#'   \code{abstract_id}, \code{congress_year}, \code{has_full_name},
#'   \code{full_first_name}, \code{last_name_upper}, \code{first_initial},
#'   \code{middle_initial}, \code{state_hint}, \code{city_hint},
#'   \code{first_author_gender}, \code{primary_procedure}.
#' @return A single-row tibble with NPI match result columns:
#'   \code{npi_number}, \code{npi_match_score}, \code{npi_n_candidates},
#'   \code{npi_match_strategy}, \code{npi_match_confidence},
#'   \code{npi_gender}, \code{npi_state}, \code{npi_subspecialty},
#'   \code{npi_full_name}.
#' @details Scoring components: exact name (50 pts) or initial (15 pts),
#'   state match (20 pts), gender match (10 pts), MIGS bonus (25 pts),
#'   middle initial (5 pts), name rarity (10 pts). Single-candidate
#'   relaxation accepts sole candidates at >= 35 pts.
#' @keywords internal
score_author <- function(author_row) {
  abs_id <- author_row$abstract_id
  congress_yr <- author_row$congress_year
  has_full <- author_row$has_full_name
  state_hint <- author_row$state_hint
  city_hint_val <- author_row$city_hint
  gender_hint <- author_row$first_author_gender
  last_up <- author_row$last_name_upper
  fi <- author_row$first_initial
  mi <- author_row$middle_initial
  procedure <- author_row$primary_procedure

  empty_result <- tibble(
    abstract_id = abs_id, npi_number = NA_character_,
    npi_match_score = 0L, npi_n_candidates = 0L,
    npi_match_strategy = NA_character_,
    npi_match_confidence = "low",
    npi_gender = NA_character_, npi_state = NA_character_,
    npi_subspecialty = NA_character_,
    npi_years_in_practice = NA_integer_,
    npi_full_name = NA_character_
  )

  # Find candidates in ABOG pool — exact last name first, fuzzy fallback
  if (has_full) {
    full_up <- toupper(trimws(normalize_dashes(author_row$full_first_name %||% "")))
    my_cands <- pool |> filter(pool_last == last_up, pool_first == full_up)
    if (nrow(my_cands) == 0) {
      my_cands <- pool |> filter(pool_last == last_up, pool_first_initial == fi)
    }
  } else {
    my_cands <- pool |> filter(pool_last == last_up, pool_first_initial == fi)
  }

  # Fuzzy last-name fallback for hyphenated/compound names (Chapman-Davis, El Hachem)
  if (nrow(my_cands) == 0 && nchar(last_up) >= 4) {
    # Try removing hyphens and spaces
    last_simplified <- gsub("[- ]", "", last_up)
    fuzzy_cands <- pool |>
      filter(gsub("[- ]", "", pool_last) == last_simplified, pool_first_initial == fi)
    if (nrow(fuzzy_cands) > 0) my_cands <- fuzzy_cands
  }

  if (nrow(my_cands) == 0) return(empty_result)

  # Gender gate
  if (!is.na(gender_hint)) {
    gender_map <- c("female" = "F", "male" = "M")
    expected_g <- unname(gender_map[gender_hint])
    if (!is.na(expected_g)) {
      filtered <- my_cands |> filter(is.na(pool_gender) | pool_gender == expected_g)
      if (nrow(filtered) > 0) my_cands <- filtered
    }
  }

  # Score each candidate
  my_cands <- my_cands |>
    mutate(
      # Name match
      pts_name = if_else(
        has_full & pool_first == toupper(trimws(author_row$full_first_name %||% "")),
        50L, 15L
      ),

      # State match
      pts_state = if_else(!is.na(state_hint) & !is.na(pool_state) &
                            toupper(state_hint) == toupper(pool_state), 20L, 0L),

      # Gender match
      pts_gender = {
        if (is.na(gender_hint)) 0L
        else {
          gm <- c("female" = "F", "male" = "M")
          expected <- unname(gm[gender_hint])
          if_else(!is.na(expected) & !is.na(pool_gender) & pool_gender == expected, 10L, 0L)
        }
      },

      # MIGS bonus — AAGL is the MIGS society, huge signal
      pts_migs = case_when(
        pool_subspecialty == "MIG" ~ 25L,
        pool_subspecialty == "Female Pelvic Medicine & Reconstructive Surgery" &
          !is.na(procedure) & procedure %in% c("sacrocolpopexy", "pelvic_floor") ~ 15L,
        pool_subspecialty == "Gynecologic Oncology" &
          !is.na(procedure) & procedure == "gynecologic_oncology" ~ 15L,
        pool_subspecialty == "Reproductive Endocrinology and Infertility" &
          !is.na(procedure) & procedure == "ectopic_pregnancy" ~ 10L,
        TRUE ~ 0L
      ),

      # Middle initial agreement (disambiguates common names)
      pts_middle = if_else(
        !is.na(mi) & !is.na(pool_middle_initial) & mi == pool_middle_initial,
        5L, 0L
      ),

      # Name rarity
      rarity = name_freq$n_in_pool[match(
        paste(pool_last, pool_first_initial),
        paste(name_freq$pool_last, name_freq$pool_first_initial)
      )],
      pts_rarity = if_else(!is.na(rarity) & rarity <= 3, 10L, 0L),

      total_score = pts_name + pts_state + pts_gender + pts_migs + pts_rarity + pts_middle
    ) |>
    arrange(desc(total_score))

  n_cands <- nrow(my_cands)
  best <- my_cands[1, ]
  second_score <- if (n_cands > 1) my_cands$total_score[2] else 0L

  # 3-tier classification
  # Single-candidate relaxation: if there's exactly 1 OB/GYN in the entire
  # ABOG pool with this last name + initial, the match is very likely correct
  # even at lower scores. Accept at >= 35 for sole candidates.
  is_sole_candidate <- n_cands == 1
  confidence <- case_when(
    best$total_score >= 50 & (best$total_score - second_score) >= 10 ~ "high",
    is_sole_candidate & best$total_score >= 35 ~ "high",
    best$total_score >= 30 ~ "ambiguous",
    TRUE ~ "low"
  )

  tibble(
    abstract_id = abs_id,
    npi_number = if (confidence == "high") best$npi else NA_character_,
    npi_match_score = best$total_score,
    npi_n_candidates = n_cands,
    npi_match_strategy = if (has_full) "exact" else "initial",
    npi_match_confidence = confidence,
    npi_gender = if (confidence == "high") best$pool_gender else NA_character_,
    npi_state = if (confidence == "high") best$pool_state else NA_character_,
    npi_subspecialty = if (confidence == "high") best$pool_subspecialty else NA_character_,
    npi_years_in_practice = NA_integer_,
    npi_full_name = if (confidence == "high") paste(best$pool_first, best$pool_last) else NA_character_
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# Score each author
cli_alert_info("Scoring {nrow(lookup)} authors against {nrow(pool)} ABOG physicians...")
results <- vector("list", nrow(lookup))
for (i in seq_len(nrow(lookup))) {
  results[[i]] <- score_author(lookup[i, ])
  if (i %% 50 == 0) cli_alert_info("  {i}/{nrow(lookup)}")
}

npi_matches <- bind_rows(results)

# ---- One-to-one enforcement ----
cli_h2("One-to-one enforcement")

tier_a <- npi_matches |> filter(npi_match_confidence == "high", !is.na(npi_number))
dupes <- tier_a |> count(npi_number) |> filter(n > 1)
if (nrow(dupes) > 0) {
  cli_alert_info("Resolving {nrow(dupes)} NPI collisions...")
  for (dup_npi in dupes$npi_number) {
    conflict <- tier_a |> filter(npi_number == dup_npi) |> arrange(desc(npi_match_score))
    losers <- conflict$abstract_id[-1]
    npi_matches <- npi_matches |>
      mutate(
        npi_match_confidence = if_else(abstract_id %in% losers, "ambiguous", npi_match_confidence),
        npi_number = if_else(abstract_id %in% losers, NA_character_, npi_number),
        npi_gender = if_else(abstract_id %in% losers, NA_character_, npi_gender),
        npi_state = if_else(abstract_id %in% losers, NA_character_, npi_state),
        npi_subspecialty = if_else(abstract_id %in% losers, NA_character_, npi_subspecialty),
        npi_full_name = if_else(abstract_id %in% losers, NA_character_, npi_full_name)
      )
  }
}

# ---- Fallback: NPPES with OB/GYN taxonomy filtering (isochrones approach) ----
cli_h2("Fallback: NPPES + Physician Compare with taxonomy filtering")

db_path <- "/Volumes/MufflySamsung/DuckDB/nber_my_duckdb.duckdb"
still_unmatched <- npi_matches |>
  filter(npi_match_confidence != "high") |>
  inner_join(lookup |> select(abstract_id, congress_year, last_name_upper, full_first_name,
                               has_full_name, state_hint, city_hint,
                               first_author_gender, first_initial, middle_initial),
             by = "abstract_id")

# OB/GYN taxonomy codes (from isochrones strategy_01)
OBGYN_TAXONOMY <- "207V%"  # 207V00000X family covers all OB/GYN subspecialties

if (nrow(still_unmatched) > 0 && file.exists(db_path)) {
  cli_alert_info("Searching NPPES for {nrow(still_unmatched)} unmatched authors ({sum(still_unmatched$has_full_name)} with full names)...")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

  fallback_results <- list()
  for (i in seq_len(nrow(still_unmatched))) {
    row <- still_unmatched[i, ]
    last_up <- row$last_name_upper

    # Build name match clause: exact first name if available, else initial match
    if (row$has_full_name) {
      first_up <- toupper(trimws(row$full_first_name))
      name_clause <- sprintf("UPPER(TRIM(first_name)) = '%s'", gsub("'", "''", first_up))
    } else {
      name_clause <- sprintf("UPPER(SUBSTR(TRIM(first_name), 1, 1)) = '%s'",
                             gsub("'", "''", row$first_initial))
    }

    # Query NPPES temporal table with OB/GYN taxonomy filter
    q_nppes <- sprintf(
      "SELECT DISTINCT npi, first_name, last_name, middle_name, gender,
              practice_address_state AS state, practice_address_city AS city,
              taxonomy_1, taxonomy_2, taxonomy_3,
              npi_enumeration_date
       FROM temporal_all_years_fixed
       WHERE entity_type = '1'
         AND UPPER(TRIM(last_name)) = '%s'
         AND %s
         AND (taxonomy_1 LIKE '%s' OR taxonomy_2 LIKE '%s' OR taxonomy_3 LIKE '%s')
         AND npi_deactivation_date IS NULL",
      gsub("'", "''", last_up), name_clause,
      OBGYN_TAXONOMY, OBGYN_TAXONOMY, OBGYN_TAXONOMY
    )
    cands <- tryCatch(DBI::dbGetQuery(con, q_nppes) |> as_tibble(), error = function(e) tibble())

    # Fallback: try Physician Compare if NPPES had nothing
    if (nrow(cands) == 0) {
      q_pc <- sprintf(
        "SELECT DISTINCT npi, first_name, last_name, gender, state, primary_specialty
         FROM pc_all_years_mat
         WHERE UPPER(TRIM(last_name)) = '%s' AND %s",
        gsub("'", "''", last_up),
        if (row$has_full_name) {
          sprintf("UPPER(TRIM(first_name)) = '%s'", gsub("'", "''", first_up))
        } else {
          sprintf("UPPER(SUBSTR(TRIM(first_name), 1, 1)) = '%s'",
                  gsub("'", "''", row$first_initial))
        }
      )
      cands <- tryCatch(DBI::dbGetQuery(con, q_pc) |> as_tibble(), error = function(e) tibble())
    }

    if (nrow(cands) == 0) next

    # Deduplicate by NPI
    cands <- cands |> distinct(npi, .keep_all = TRUE)

    # Ensure columns exist (NPPES has city/middle/temporal; PC may not)
    if (!"city" %in% names(cands)) cands$city <- NA_character_
    if (!"middle_name" %in% names(cands)) cands$middle_name <- NA_character_
    if (!"npi_enumeration_date" %in% names(cands)) cands$npi_enumeration_date <- NA_character_

    # Score with taxonomy + temporal + middle initial + city signals
    congress_yr <- row$congress_year
    cands <- cands |>
      mutate(
        npi = as.character(npi),
        pts_name = if (row$has_full_name) 50L else 15L,

        # State match (+20)
        pts_state = if_else(!is.na(row$state_hint) & !is.na(state) &
                              toupper(row$state_hint) == toupper(state), 20L, 0L),

        # City match (+10)
        pts_city = if_else(
          !is.na(row$city_hint) & !is.na(city) &
            toupper(trimws(row$city_hint)) == toupper(trimws(city)),
          10L, 0L
        ),

        # Gender match (+10)
        pts_gender = {
          if (is.na(row$first_author_gender)) 0L
          else {
            gm <- c("female" = "F", "male" = "M")
            expected <- unname(gm[row$first_author_gender])
            if_else(!is.na(expected) & !is.na(gender) & gender == expected, 10L, 0L)
          }
        },

        # Middle initial agreement (+5)
        pts_middle = if_else(
          !is.na(row$middle_initial) & !is.na(middle_name) &
            toupper(substr(trimws(middle_name), 1, 1)) == row$middle_initial,
          5L, 0L
        ),

        # OB/GYN taxonomy bonus (+10)
        has_obgyn_tax = if ("taxonomy_1" %in% names(cands)) {
          grepl("^207V", coalesce(taxonomy_1, "")) |
          grepl("^207V", coalesce(taxonomy_2, "")) |
          grepl("^207V", coalesce(taxonomy_3, ""))
        } else if ("primary_specialty" %in% names(cands)) {
          grepl("obgyn|obstetric|gynecol", tolower(coalesce(primary_specialty, "")))
        } else FALSE,
        pts_spec = if_else(has_obgyn_tax, 10L, 0L),

        # Temporal scoring: NPI enumerated before congress year (+5)
        pts_temporal = if_else(
          !is.na(npi_enumeration_date) & !is.na(congress_yr) &
            as.numeric(substr(as.character(npi_enumeration_date), 1, 4)) <= congress_yr,
          5L, 0L
        ),

        total_score = pts_name + pts_state + pts_city + pts_gender +
                      pts_middle + pts_spec + pts_temporal
      ) |>
      arrange(desc(total_score))

    n_cands <- nrow(cands)
    best <- cands[1, ]
    second_score <- if (n_cands > 1) cands$total_score[2] else 0L
    is_sole <- n_cands == 1

    confidence <- case_when(
      best$total_score >= 50 & (best$total_score - second_score) >= 10 ~ "high",
      is_sole & best$total_score >= 35 ~ "high",
      best$total_score >= 30 ~ "ambiguous",
      TRUE ~ "low"
    )

    if (confidence == "high") {
      idx <- which(npi_matches$abstract_id == row$abstract_id)
      npi_matches$npi_number[idx] <- best$npi
      npi_matches$npi_match_score[idx] <- best$total_score
      npi_matches$npi_n_candidates[idx] <- n_cands
      npi_matches$npi_match_strategy[idx] <- "fallback_nppes_taxonomy"
      npi_matches$npi_match_confidence[idx] <- "high"
      npi_matches$npi_gender[idx] <- best$gender
      npi_matches$npi_state[idx] <- best$state
      npi_matches$npi_subspecialty[idx] <- NA_character_
      npi_matches$npi_full_name[idx] <- paste(best$first_name, best$last_name)

      fallback_results[[length(fallback_results) + 1]] <- tibble(
        abstract_id = row$abstract_id, npi = best$npi, source = "nppes_taxonomy"
      )
    }

    if (i %% 50 == 0) cli_alert_info("  {i}/{nrow(still_unmatched)}")
  }

  DBI::dbDisconnect(con, shutdown = TRUE)

  n_fallback <- length(fallback_results)
  cli_alert_success("Fallback matched: {n_fallback} additional physicians")
} else {
  cli_alert_info("No fallback candidates or DuckDB not mounted")
}

# Re-enforce one-to-one after fallback
tier_a <- npi_matches |> filter(npi_match_confidence == "high", !is.na(npi_number))
dupes <- tier_a |> count(npi_number) |> filter(n > 1)
if (nrow(dupes) > 0) {
  cli_alert_info("Resolving {nrow(dupes)} post-fallback NPI collisions...")
  for (dup_npi in dupes$npi_number) {
    conflict <- tier_a |> filter(npi_number == dup_npi) |> arrange(desc(npi_match_score))
    losers <- conflict$abstract_id[-1]
    npi_matches <- npi_matches |>
      mutate(
        npi_match_confidence = if_else(abstract_id %in% losers, "ambiguous", npi_match_confidence),
        npi_number = if_else(abstract_id %in% losers, NA_character_, npi_number),
        npi_gender = if_else(abstract_id %in% losers, NA_character_, npi_gender),
        npi_state = if_else(abstract_id %in% losers, NA_character_, npi_state),
        npi_subspecialty = if_else(abstract_id %in% losers, NA_character_, npi_subspecialty),
        npi_full_name = if_else(abstract_id %in% losers, NA_character_, npi_full_name)
      )
  }
}

# Add ACOG district
npi_matches <- npi_matches |>
  mutate(npi_acog_district = vapply(npi_state, acog_district_for_state, character(1)))

# ---- Summary ----
cli_h2("Results")

tier_summary <- npi_matches |> count(npi_match_confidence)
for (i in seq_len(nrow(tier_summary))) {
  cli_alert_info("{tier_summary$npi_match_confidence[i]}: {tier_summary$n[i]}")
}

n_matched <- sum(npi_matches$npi_match_confidence == "high")
cli_alert_success("High-confidence NPI matches: {n_matched} / {nrow(npi_matches)} US authors ({round(n_matched/nrow(npi_matches)*100,1)}%)")
cli_alert_info("Unique NPIs assigned: {length(unique(npi_matches$npi_number[!is.na(npi_matches$npi_number)]))}")

med_cands <- median(npi_matches$npi_n_candidates[npi_matches$npi_match_confidence == "high"])
cli_alert_info("Median candidates per Tier A: {med_cands}")

# Subspecialty breakdown of Tier A
if (n_matched > 0) {
  cli_alert_info("Tier A subspecialties:")
  print(table(npi_matches$npi_subspecialty[npi_matches$npi_match_confidence == "high"], useNA = "ifany"))
}

# MIGS bonus impact
migs_matches <- sum(npi_matches$npi_subspecialty == "MIG", na.rm = TRUE)
cli_alert_info("MIGS specialists matched: {migs_matches}")

# Save
out_path <- here("data", "processed", "npi_matches.csv")
write_csv(npi_matches, out_path)
cli_alert_success("Saved: {out_path}")
