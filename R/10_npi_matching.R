# 10_npi_matching.R — NPI matching for AAGL abstract first authors
#
# Matches US-based first authors to National Provider Identifiers via the
# isochrones DuckDB (NPPES/Physician Compare). Restricted to OB/GYN taxonomy.
# Uses 3-tier classification (high/ambiguous/no match) with conservative scoring.
#
# Requires: /Volumes/MufflySamsung/DuckDB/nber_my_duckdb.duckdb (Mac-local)
# Writes: data/processed/npi_matches.csv

suppressPackageStartupMessages({
  library(here); library(config); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(DBI); library(duckdb)
})

source(here("R", "utils_text.R"))
source(here("R", "utils_states.R"))
source(here("R", "utils_acog.R"))

cfg <- config::get(file = here("config.yml"))

cli_h1("NPI Matching for AAGL First Authors")

# ---- Check DuckDB availability ----
db_path <- "/Volumes/MufflySamsung/DuckDB/nber_my_duckdb.duckdb"
if (!file.exists(db_path)) {
  cli_alert_danger("DuckDB not found at {db_path} — is the drive mounted?")
  cli_alert_info("Skipping NPI matching")
  quit(save = "no")
}

# ---- Load data ----
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
char <- read_csv(here("data", "processed", "author_characteristics.csv"), show_col_types = FALSE)
matches <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)

# PubMed full names for confirmed matches only
pubmed_fa <- tryCatch({
  a <- read_csv(here("data", "processed", "authors_pubmed.csv"), show_col_types = FALSE)
  a |>
    filter(position == 1) |>
    group_by(abstract_id) |>
    slice(1) |>
    ungroup() |>
    inner_join(
      matches |> filter(classification %in% c("definite", "probable")) |> select(abstract_id),
      by = "abstract_id"
    ) |>
    select(abstract_id, pubmed_first_name = first_name, pubmed_last_name = last_name,
           pubmed_city = affiliation_city, pubmed_state = affiliation_state,
           pubmed_aff = affiliation)
}, error = function(e) tibble(abstract_id = character()))

# ---- Step 1: Build lookup table ----
cli_h2("Step 1: Building author lookup table")

# Parse AAGL author names
lookup <- abstracts |>
  filter(is_us_based == TRUE) |>
  select(abstract_id, congress_year, author_name_first, primary_procedure) |>
  mutate(
    # normalize_author returns "lastname F" format
    author_norm = vapply(author_name_first, normalize_author, character(1)),
    # Split into last_name and first_initial
    last_name = trimws(str_extract(author_norm, "^[a-z][a-z '-]+")),
    first_initial = toupper(str_extract(author_norm, "[A-Za-z]$"))
  ) |>
  filter(!is.na(last_name), !is.na(first_initial)) |>
  # Join PubMed full names for confirmed matches

  left_join(pubmed_fa, by = "abstract_id") |>
  mutate(
    # Only use PubMed full name when the PubMed last name matches the AAGL
    # last name — the matched paper's first author may be a different person.
    pubmed_name_matches_aagl = !is.na(pubmed_last_name) &
      tolower(trimws(pubmed_last_name)) == tolower(trimws(last_name)),
    has_full_name = pubmed_name_matches_aagl &
      !is.na(pubmed_first_name) & nchar(pubmed_first_name) > 2,
    full_first_name = if_else(has_full_name, pubmed_first_name, NA_character_),
    last_name_upper = toupper(last_name)
  ) |>
  # Join existing demographics for disambiguation
  left_join(
    char |> select(abstract_id, first_author_state, first_author_gender),
    by = "abstract_id"
  ) |>
  mutate(
    # Only use PubMed affiliation data when the PubMed author IS the AAGL author
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
n_initial <- n_us - n_full
cli_alert_info("US-based authors: {n_us}")
cli_alert_info("  With full first name (confirmed matches): {n_full}")
cli_alert_info("  With initial only: {n_initial}")

# ---- Step 2: Connect to DuckDB ----
cli_h2("Step 2: Querying DuckDB")

con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

# ---- Precompute name rarity ----
cli_alert_info("Computing name rarity across OB/GYN NPPES...")
name_freq <- dbGetQuery(con, "
  SELECT UPPER(TRIM(last_name)) AS last_name,
         LEFT(UPPER(TRIM(first_name)), 1) AS first_initial,
         COUNT(DISTINCT npi) AS n_obgyn
  FROM temporal_all_years_fixed
  WHERE taxonomy_1 LIKE '207V%'
     OR taxonomy_1 IN ('390200000X', '174400000X')
  GROUP BY 1, 2
") |> as_tibble()
cli_alert_success("Name rarity: {nrow(name_freq)} (last, initial) combinations")

# ---- Strategy A: Exact full name match ----
cli_h3("Strategy A: Exact full name match ({n_full} authors)")

full_authors <- lookup |> filter(has_full_name)
candidates_a <- tibble()

if (nrow(full_authors) > 0) {
  unique_names <- full_authors |>
    distinct(last_name_upper, full_first_name) |>
    mutate(first_upper = toupper(full_first_name))

  # Batch query
  chunks <- split(seq_len(nrow(unique_names)), ceiling(seq_len(nrow(unique_names)) / 50))
  all_cands_a <- list()

  for (chunk_idx in seq_along(chunks)) {
    rows <- unique_names[chunks[[chunk_idx]], ]
    # Build parameterized-like query using IN clauses
    last_vals <- paste0("'", gsub("'", "''", rows$last_name_upper), "'", collapse = ", ")
    first_vals <- paste0("'", gsub("'", "''", rows$first_upper), "'", collapse = ", ")

    q <- sprintf("
      SELECT DISTINCT npi, first_name, last_name, gender,
             practice_address_state, practice_address_city,
             taxonomy_1, npi_enumeration_date
      FROM temporal_all_years_fixed
      WHERE UPPER(TRIM(last_name)) IN (%s)
        AND UPPER(TRIM(first_name)) IN (%s)
        AND (taxonomy_1 LIKE '207V%%' OR taxonomy_1 IN ('390200000X', '174400000X'))
    ", last_vals, first_vals)

    res <- tryCatch(dbGetQuery(con, q), error = function(e) {
      cli_alert_warning("Strategy A batch {chunk_idx} failed: {e$message}")
      tibble()
    })
    if (nrow(res) > 0) all_cands_a[[length(all_cands_a) + 1]] <- as_tibble(res)
  }

  if (length(all_cands_a) > 0) {
    candidates_a <- bind_rows(all_cands_a) |>
      mutate(
        npi = as.character(npi),
        last_upper = toupper(trimws(last_name)),
        first_upper = toupper(trimws(first_name)),
        strategy = "exact"
      )
  }
  cli_alert_success("Strategy A: {nrow(candidates_a)} candidate rows")
}

# ---- Strategy B: Initial + last name match ----
cli_h3("Strategy B: Initial + last name ({n_initial} authors)")

initial_authors <- lookup |> filter(!has_full_name)
candidates_b <- tibble()

if (nrow(initial_authors) > 0) {
  unique_lasts <- unique(initial_authors$last_name_upper)
  chunks <- split(unique_lasts, ceiling(seq_along(unique_lasts) / 50))
  all_cands_b <- list()

  for (chunk_idx in seq_along(chunks)) {
    last_vals <- paste0("'", gsub("'", "''", chunks[[chunk_idx]]), "'", collapse = ", ")

    q <- sprintf("
      SELECT DISTINCT npi, first_name, last_name, gender,
             practice_address_state, practice_address_city,
             taxonomy_1, npi_enumeration_date
      FROM temporal_all_years_fixed
      WHERE UPPER(TRIM(last_name)) IN (%s)
        AND (taxonomy_1 LIKE '207V%%' OR taxonomy_1 IN ('390200000X', '174400000X'))
    ", last_vals)

    res <- tryCatch(dbGetQuery(con, q), error = function(e) {
      cli_alert_warning("Strategy B batch {chunk_idx} failed: {e$message}")
      tibble()
    })
    if (nrow(res) > 0) all_cands_b[[length(all_cands_b) + 1]] <- as_tibble(res)
    if (chunk_idx %% 5 == 0) cli_alert_info("  Batch {chunk_idx}/{length(chunks)}")
  }

  if (length(all_cands_b) > 0) {
    candidates_b <- bind_rows(all_cands_b) |>
      mutate(
        npi = as.character(npi),
        last_upper = toupper(trimws(last_name)),
        first_upper = toupper(trimws(first_name)),
        first_init = substr(first_upper, 1, 1),
        strategy = "initial"
      )
  }
  cli_alert_success("Strategy B: {nrow(candidates_b)} candidate rows")
}

# ---- Step 3-4: Match candidates to authors + prune + score ----
cli_h2("Step 3: Matching, pruning, scoring")

score_author <- function(author_row, cands, name_freq_tbl) {
  abs_id <- author_row$abstract_id
  congress_yr <- author_row$congress_year
  has_full <- author_row$has_full_name
  state_hint <- author_row$state_hint
  city_hint <- author_row$city_hint
  procedure <- author_row$primary_procedure
  gender_hint <- author_row$first_author_gender
  last_up <- author_row$last_name_upper
  fi <- author_row$first_initial

  # Filter candidates to this author
  if (has_full) {
    my_cands <- cands |>
      filter(last_upper == last_up, first_upper == toupper(author_row$full_first_name))
  } else {
    my_cands <- cands |>
      filter(last_upper == last_up, first_init == fi)
  }

  if (nrow(my_cands) == 0) {
    return(tibble(abstract_id = abs_id, npi_number = NA_character_,
                  npi_match_score = 0, npi_n_candidates = 0L,
                  npi_match_strategy = NA_character_,
                  npi_match_confidence = "low",
                  npi_gender = NA_character_, npi_state = NA_character_,
                  npi_taxonomy = NA_character_,
                  npi_enumeration_date = as.Date(NA),
                  npi_years_in_practice = NA_integer_))
  }

  # Prune: career window
  my_cands <- my_cands |>
    mutate(
      enum_year = as.integer(format(as.Date(npi_enumeration_date), "%Y")),
      years_in_practice = congress_yr - enum_year
    ) |>
    filter(is.na(years_in_practice) | (years_in_practice >= 0 & years_in_practice <= 45))

  # Prune: gender gate
  if (!is.na(gender_hint)) {
    gender_map <- c("female" = "F", "male" = "M")
    expected_g <- unname(gender_map[gender_hint])
    if (!is.na(expected_g)) {
      my_cands <- my_cands |>
        filter(is.na(gender) | gender == expected_g)
    }
  }

  if (nrow(my_cands) == 0) {
    return(tibble(abstract_id = abs_id, npi_number = NA_character_,
                  npi_match_score = 0, npi_n_candidates = 0L,
                  npi_match_strategy = NA_character_,
                  npi_match_confidence = "low",
                  npi_gender = NA_character_, npi_state = NA_character_,
                  npi_taxonomy = NA_character_,
                  npi_enumeration_date = as.Date(NA),
                  npi_years_in_practice = NA_integer_))
  }

  # Score each candidate
  my_cands <- my_cands |>
    mutate(
      # Name score
      pts_name = if (has_full) 50L else 15L,

      # State match
      pts_state = if_else(!is.na(state_hint) & !is.na(practice_address_state) &
                            toupper(state_hint) == practice_address_state, 20L, 0L),

      # Gender match
      pts_gender = if_else(!is.na(gender_hint) & !is.na(gender), {
        gender_map <- c("female" = "F", "male" = "M")
        expected <- unname(gender_map[gender_hint])
        if_else(!is.na(expected) & gender == expected, 10L, 0L)
      }, 0L),

      # Career plausibility
      pts_career = if_else(!is.na(years_in_practice) &
                             years_in_practice >= 0 & years_in_practice <= 45, 5L, 0L),

      # Name rarity
      rarity = name_freq_tbl$n_obgyn[match(
        paste(last_upper, fi),
        paste(name_freq_tbl$last_name, name_freq_tbl$first_initial)
      )],
      pts_rarity = if_else(!is.na(rarity) & rarity < 5, 10L, 0L),

      # City match
      pts_city = if_else(
        !is.na(city_hint) & !is.na(practice_address_city) &
          tolower(trimws(city_hint)) == tolower(trimws(practice_address_city)),
        10L, 0L
      ),

      # Subspecialty preference (small tiebreaker, never excludes)
      pts_subspecialty = if_else(
        !is.na(procedure) & procedure != "NA" & !is.na(taxonomy_1),
        case_when(
          procedure %in% c("sacrocolpopexy", "pelvic_floor") & taxonomy_1 == "207VF0040X" ~ 5L,
          procedure == "gynecologic_oncology" & taxonomy_1 == "207VG0400X" ~ 5L,
          TRUE ~ 0L
        ),
        0L
      ),

      total_score = pts_name + pts_state + pts_gender + pts_career +
                    pts_rarity + pts_city + pts_subspecialty
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
    npi_gender = if (confidence == "high") best$gender else NA_character_,
    npi_state = if (confidence == "high") best$practice_address_state else NA_character_,
    npi_taxonomy = if (confidence == "high") best$taxonomy_1 else NA_character_,
    npi_enumeration_date = if (confidence == "high") as.Date(best$npi_enumeration_date) else as.Date(NA),
    npi_years_in_practice = if (confidence == "high") best$years_in_practice else NA_integer_
  )
}

# Combine all candidates — ensure columns exist even if one strategy returned 0
empty_cand <- tibble(npi = character(), first_name = character(), last_name = character(),
                     gender = character(), practice_address_state = character(),
                     practice_address_city = character(),
                     taxonomy_1 = character(), npi_enumeration_date = as.Date(character()),
                     last_upper = character(), first_upper = character(), strategy = character())
all_candidates <- bind_rows(
  if (nrow(candidates_a) > 0) candidates_a else empty_cand,
  if (nrow(candidates_b) > 0) candidates_b else empty_cand
) |>
  mutate(first_init = substr(first_upper, 1, 1))

# Score each author
cli_alert_info("Scoring {nrow(lookup)} authors...")
results <- vector("list", nrow(lookup))
for (i in seq_len(nrow(lookup))) {
  results[[i]] <- score_author(lookup[i, ], all_candidates, name_freq)
  if (i %% 50 == 0) cli_alert_info("  {i}/{nrow(lookup)}")
}

npi_matches <- bind_rows(results)

# ---- Step 5: One-to-one enforcement ----
cli_h2("Step 4: One-to-one enforcement")

tier_a <- npi_matches |> filter(npi_match_confidence == "high", !is.na(npi_number))
dupes <- tier_a |> count(npi_number) |> filter(n > 1)
if (nrow(dupes) > 0) {
  cli_alert_info("Resolving {nrow(dupes)} NPI collisions...")
  for (dup_npi in dupes$npi_number) {
    conflict <- tier_a |> filter(npi_number == dup_npi) |> arrange(desc(npi_match_score))
    losers <- conflict$abstract_id[-1]  # keep highest score
    npi_matches <- npi_matches |>
      mutate(
        npi_match_confidence = if_else(abstract_id %in% losers, "ambiguous", npi_match_confidence),
        npi_number = if_else(abstract_id %in% losers, NA_character_, npi_number),
        npi_gender = if_else(abstract_id %in% losers, NA_character_, npi_gender),
        npi_state = if_else(abstract_id %in% losers, NA_character_, npi_state),
        npi_taxonomy = if_else(abstract_id %in% losers, NA_character_, npi_taxonomy),
        npi_enumeration_date = if_else(abstract_id %in% losers, as.Date(NA), npi_enumeration_date),
        npi_years_in_practice = if_else(abstract_id %in% losers, NA_integer_, npi_years_in_practice)
      )
  }
}

# ---- Add ACOG district from NPI state ----
npi_matches <- npi_matches |>
  mutate(
    npi_acog_district = vapply(npi_state, acog_district_for_state, character(1)),
    npi_subspecialty = case_when(
      str_detect(npi_taxonomy, "207VX0000X") ~ "general_OBGYN",
      str_detect(npi_taxonomy, "207VG0400X") ~ "GYN_ONC",
      str_detect(npi_taxonomy, "207VM0101X") ~ "MFM",
      str_detect(npi_taxonomy, "207VF0040X") ~ "FPMRS",
      str_detect(npi_taxonomy, "207VE0102X") ~ "REI",
      str_detect(npi_taxonomy, "207V") ~ "general_OBGYN",
      TRUE ~ NA_character_
    )
  )

# ---- Summary ----
cli_h2("Results")

tier_summary <- npi_matches |> count(npi_match_confidence)
for (i in seq_len(nrow(tier_summary))) {
  cli_alert_info("{tier_summary$npi_match_confidence[i]}: {tier_summary$n[i]}")
}

n_matched <- sum(npi_matches$npi_match_confidence == "high")
cli_alert_success("High-confidence NPI matches: {n_matched} / {nrow(npi_matches)} US authors ({round(n_matched/nrow(npi_matches)*100,1)}%)")

# Collision check
n_unique_npi <- length(unique(npi_matches$npi_number[!is.na(npi_matches$npi_number)]))
cli_alert_info("Unique NPIs assigned: {n_unique_npi}")

# ---- Save ----
out_path <- here("data", "processed", "npi_matches.csv")
write_csv(npi_matches, out_path)
cli_alert_success("Saved: {out_path}")

# Median candidates
med_cands <- median(npi_matches$npi_n_candidates[npi_matches$npi_match_confidence == "high"])
cli_alert_info("Median candidates per Tier A match: {med_cands}")

if (med_cands > 3) {
  cli_alert_warning("Median > 3 — filter may be too weak")
}
