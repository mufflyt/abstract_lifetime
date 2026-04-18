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
  library(stringr); library(tibble)
})

source(here("R", "utils_text.R"))
source(here("R", "utils_states.R"))
source(here("R", "utils_acog.R"))

cfg <- config::get(file = here("config.yml"))

cli_h1("NPI Matching for AAGL First Authors")

# ---- Load ABOG-NPI pool ----
abog_path <- "/Users/tylermuffly/isochrones/data/canonical_abog/canonical_abog_npi_LATEST.csv"
if (!file.exists(abog_path)) {
  cli_alert_danger("ABOG-NPI file not found at {abog_path}")
  quit(save = "no")
}

pool <- read_csv(abog_path, show_col_types = FALSE) |>
  filter(!is.na(first_name), !is.na(last_name), nchar(first_name) > 0) |>
  transmute(
    npi = as.character(npi),
    pool_first = toupper(trimws(first_name)),
    pool_last = toupper(trimws(last_name)),
    pool_gender = npi_gender,
    pool_state = state,
    pool_subspecialty = subspecialty,
    pool_first_initial = substr(pool_first, 1, 1)
  ) |>
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

# Build lookup
lookup <- abstracts |>
  filter(is_us_based == TRUE) |>
  select(abstract_id, congress_year, author_name_first, primary_procedure) |>
  mutate(
    author_norm = vapply(author_name_first, normalize_author, character(1)),
    last_name = trimws(str_extract(author_norm, "^[a-z][a-z '-]+")),
    first_initial = toupper(str_extract(author_norm, "[A-Za-z]$"))
  ) |>
  filter(!is.na(last_name), !is.na(first_initial)) |>
  left_join(pubmed_fa, by = "abstract_id") |>
  mutate(
    pubmed_name_matches_aagl = !is.na(pubmed_last_name) &
      tolower(trimws(pubmed_last_name)) == tolower(trimws(last_name)),
    has_full_name = pubmed_name_matches_aagl &
      !is.na(pubmed_first_name) & nchar(pubmed_first_name) > 2,
    full_first_name = if_else(has_full_name, pubmed_first_name, NA_character_),
    last_name_upper = toupper(last_name)
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

score_author <- function(author_row) {
  abs_id <- author_row$abstract_id
  congress_yr <- author_row$congress_year
  has_full <- author_row$has_full_name
  state_hint <- author_row$state_hint
  gender_hint <- author_row$first_author_gender
  last_up <- author_row$last_name_upper
  fi <- author_row$first_initial
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

  # Find candidates in ABOG pool
  if (has_full) {
    full_up <- toupper(trimws(author_row$full_first_name))
    my_cands <- pool |> filter(pool_last == last_up, pool_first == full_up)
    if (nrow(my_cands) == 0) {
      # Fallback to initial match
      my_cands <- pool |> filter(pool_last == last_up, pool_first_initial == fi)
    }
  } else {
    my_cands <- pool |> filter(pool_last == last_up, pool_first_initial == fi)
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

      # Name rarity
      rarity = name_freq$n_in_pool[match(
        paste(pool_last, pool_first_initial),
        paste(name_freq$pool_last, name_freq$pool_first_initial)
      )],
      pts_rarity = if_else(!is.na(rarity) & rarity <= 3, 10L, 0L),

      total_score = pts_name + pts_state + pts_gender + pts_migs + pts_rarity
    ) |>
    arrange(desc(total_score))

  n_cands <- nrow(my_cands)
  best <- my_cands[1, ]
  second_score <- if (n_cands > 1) my_cands$total_score[2] else 0L

  # 3-tier classification
  confidence <- case_when(
    best$total_score >= 50 & (best$total_score - second_score) >= 10 ~ "high",
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
