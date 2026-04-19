# 10e_merge_demographics.R — SOLE MERGE POINT for all demographics
#
# This is the only script that writes to output/abstracts_with_matches.csv.
# All demographic producer scripts write sidecar CSVs to data/processed/;
# this script reads them all and merges with a clear priority waterfall.
#
# Gender priority (highest → lowest):
#   1. NPI (authoritative identity match)
#   2. OpenAlex full name from abstract DOI (10b)
#   3. PubMed full name search (09f)
#   4. OB/GYN publication search (09h)
#   5. OpenAlex author search (09i)
#   6. ORCID (09g)
#   7. Open Payments (09j)
#   8. Senior author triangulation (10f)
#   9. Second author triangulation (10g)
#  10. SSA/genderize/intl from 09c (base)
#
# Sidecar CSVs consumed:
#   data/processed/author_characteristics.csv   (09c)
#   data/processed/openalex_author_names.csv    (10b)
#   data/processed/npi_matches.csv              (10_npi)
#   data/processed/orcid_demographics.csv       (10d)
#   data/processed/senior_triangulation.csv     (10f)
#   data/processed/second_author_triangulation.csv (10g)
#   data/processed/gender_from_pubmed.csv       (09f)
#   data/processed/gender_from_orcid.csv        (09g)
#   data/processed/gender_from_obgyn_pubs.csv   (09h)
#   data/processed/gender_from_openalex.csv     (09i)
#   data/processed/gender_from_open_payments.csv (09j)

suppressPackageStartupMessages({
  library(here); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble)
})

cli_h1("Merging all demographic sources (sole merge point)")

#' Read a sidecar CSV with graceful fallback
#'
#' @param path Character. File path to the sidecar CSV.
#' @param label Character. Human-readable label for CLI messages.
#' @return A tibble (empty if file not found).
#' @keywords internal
read_sidecar <- function(path, label) {
  if (file.exists(path)) {
    d <- read_csv(path, show_col_types = FALSE)
    cli_alert_success("{label}: {nrow(d)} rows")
    d
  } else {
    cli_alert_warning("{label}: not found ({path})")
    tibble()
  }
}

#' Assert that a sidecar has no duplicate abstract_id keys
#'
#' Stops with an error if duplicates are found, preventing silent data loss
#' from \code{distinct()}. If duplicates exist, they indicate an upstream bug.
#'
#' @param tbl Tibble with an \code{abstract_id} column.
#' @param source_label Character. Name of the sidecar for the error message.
#' @return The input tibble (invisible), for pipe chaining.
#' @keywords internal
assert_unique_keys <- function(tbl, source_label) {
  if (nrow(tbl) == 0) return(invisible(tbl))
  dupes <- sum(duplicated(tbl$abstract_id))
  if (dupes > 0) {
    dup_ids <- tbl$abstract_id[duplicated(tbl$abstract_id)] |> unique() |> head(5)
    stop(sprintf("Duplicate abstract_id in %s: %d duplicates (e.g., %s)",
                 source_label, dupes, paste(dup_ids, collapse = ", ")))
  }
  invisible(tbl)
}

#' Assert that a tibble has exactly the expected number of rows
#'
#' Stops with an informative error if a left join introduced row
#' multiplication (duplicate keys in the sidecar).
#'
#' @param tbl Tibble. The table to check.
#' @param source_label Character. Name of the merge source for the error message.
#' @return The input tibble (invisible), for pipe chaining.
#' @keywords internal
assert_rows <- function(tbl, source_label) {
  if (nrow(tbl) != base_n) {
    stop(sprintf("Row count changed after merging %s: expected %d, got %d",
                 source_label, base_n, nrow(tbl)))
  }
  invisible(tbl)
}

# ── Load base: abstracts_with_matches (without demographics) ─────────────────
matches_path <- here("output", "abstracts_with_matches.csv")
matches <- read_csv(matches_path, show_col_types = FALSE)
base_n <- nrow(matches)
cli_alert_info("Base: {base_n} abstracts, {ncol(matches)} columns — row count locked")

# Strip all demographic columns — we rebuild them fresh each run
demo_cols_to_drop <- c(
  # Author characteristics (from 09c)
  "n_authors", "n_authors_aagl", "n_unique_affiliations",
  "first_author_last", "first_author_first",
  "first_author_state", "first_author_country",
  "first_author_acog_district",
  "first_author_gender", "first_author_gender_p",
  "practice_type", "subspecialty", "career_stage",
  # NPI
  "npi_number", "npi_gender", "npi_state", "npi_subspecialty",
  "npi_match_score", "npi_match_confidence", "npi_match_strategy",
  "npi_full_name", "npi_acog_district", "npi_n_candidates",
  "npi_years_in_practice",
  # ORCID
  "orcid_country", "orcid_institution",
  # Gender unified
  "oa_gender", "gender_unified", "gender_source", "gender_confidence",
  "npi_gender_norm",
  # Gender enrichment tracking columns
  "pubmed_full_first", "orcid_first_name", "obgyn_first_name",
  "openalex_first_name", "op_first_name",
  # Unified columns
  "state_unified", "subspecialty_unified",
  "demographics_from_matched_pub"
)
for (col in intersect(demo_cols_to_drop, names(matches))) matches[[col]] <- NULL
cli_alert_info("Stripped stale demographic columns → {ncol(matches)} columns")

# ── Load all sidecar CSVs ────────────────────────────────────────────────────
char    <- read_sidecar(here("data", "processed", "author_characteristics.csv"), "09c author_characteristics")
oa      <- read_sidecar(here("data", "processed", "openalex_author_names.csv"), "10b OpenAlex names")
npi     <- read_sidecar(here("data", "processed", "npi_matches.csv"), "10_npi matches")
orcid   <- read_sidecar(here("data", "processed", "orcid_demographics.csv"), "10d ORCID demographics")
tri_sr  <- read_sidecar(here("data", "processed", "senior_triangulation.csv"), "10f senior triangulation")
tri_2nd <- read_sidecar(here("data", "processed", "second_author_triangulation.csv"), "10g second author triangulation")
g_pub   <- read_sidecar(here("data", "processed", "gender_from_pubmed.csv"), "09f gender from PubMed")
g_orc   <- read_sidecar(here("data", "processed", "gender_from_orcid.csv"), "09g gender from ORCID")
g_obg   <- read_sidecar(here("data", "processed", "gender_from_obgyn_pubs.csv"), "09h gender from OB/GYN pubs")
g_oax   <- read_sidecar(here("data", "processed", "gender_from_openalex.csv"), "09i gender from OpenAlex search")
g_opm   <- read_sidecar(here("data", "processed", "gender_from_open_payments.csv"), "09j gender from Open Payments")

# ── 1. Merge author characteristics (09c) ────────────────────────────────────
cli_h2("1. Author characteristics from 09c")
if (nrow(char) > 0) {
  char_cols <- c("abstract_id", "n_authors", "n_authors_aagl", "n_unique_affiliations",
                 "first_author_last", "first_author_first",
                 "first_author_state", "first_author_country",
                 "first_author_acog_district",
                 "first_author_gender", "first_author_gender_p",
                 "practice_type", "subspecialty", "career_stage")
  char_slim <- char |> select(any_of(char_cols)) |> assert_unique_keys("09c author_characteristics")
  matches <- matches |>
    left_join(char_slim, by = "abstract_id") |>
    assert_rows("09c author_characteristics")

  # Reversible blanking: flag rows where demographics came from wrong match
  matches <- matches |>
    mutate(
      demographics_from_matched_pub = classification %in% c("definite", "probable")
    )

  # Blank PubMed-derived fields for non-confirmed matches (reversible: re-run restores)
  pubmed_derived <- c("first_author_last", "first_author_first",
                      "first_author_country", "first_author_state",
                      "first_author_acog_district",
                      "practice_type", "subspecialty", "career_stage",
                      "n_unique_affiliations")
  wrong_match <- !matches$demographics_from_matched_pub
  n_blanked <- sum(wrong_match, na.rm = TRUE)
  for (col in intersect(pubmed_derived, names(matches))) {
    matches[[col]][wrong_match] <- NA
  }
  cli_alert_info("Blanked {n_blanked} rows of PubMed-derived demographics (non-confirmed matches)")
  # NOTE: first_author_gender is NOT blanked here — it's derived from the name,
  # not the matched publication. It serves as the base SSA gender (tier 10).
}

# ── 2. Build gender waterfall ────────────────────────────────────────────────
cli_h2("2. Gender waterfall (10 tiers)")

# Tier 1: NPI gender (authoritative identity)
npi_gender_tbl <- if (nrow(npi) > 0) {
  npi |>
    filter(npi_match_confidence == "high") |>
    transmute(abstract_id,
              gender_npi = case_when(npi_gender == "F" ~ "female",
                                    npi_gender == "M" ~ "male",
                                    TRUE ~ NA_character_)) |>
    filter(!is.na(gender_npi)) |>
    distinct(abstract_id, .keep_all = TRUE)
} else tibble(abstract_id = character(), gender_npi = character())

#' Clean a first name for gender lookup
#'
#' Strips trailing middle initials, extracts the first token, and
#' title-cases the result (e.g., "Hsiang-Ying A" -> "Hsiang-ying").
#'
#' @param n Character scalar. Raw first name string.
#' @return Character scalar. Cleaned first name, or \code{NA_character_}
#'   if the name is too short or missing.
#' @keywords internal
clean_first_name <- function(n) {
  if (is.na(n) || nchar(n) < 2) return(NA_character_)
  n <- trimws(sub("\\s+[A-Z]\\.?$", "", n))
  n <- strsplit(n, "\\s+")[[1]][1]
  if (is.na(n) || nchar(n) < 2) return(NA_character_)
  paste0(toupper(substr(n, 1, 1)), tolower(substr(n, 2, nchar(n))))
}

oa_gender_tbl <- if (nrow(oa) > 0) {
  oa_names <- oa |>
    filter(name_agrees == TRUE, !is.na(oa_first_name), nchar(oa_first_name) > 1) |>
    mutate(name_clean = vapply(oa_first_name, clean_first_name, character(1))) |>
    filter(!is.na(name_clean))

  # SSA pass
  ssa <- tryCatch({
    gender::gender(unique(oa_names$name_clean), years = c(1930, 2012), method = "ssa") |>
      select(name_clean = name, gender_oa = gender)
  }, error = function(e) tibble())

  # International lookup
  intl_path <- here("data", "validation", "international_gender_lookup.csv")
  intl <- if (file.exists(intl_path)) {
    read_csv(intl_path, show_col_types = FALSE) |>
      mutate(name_clean = paste0(toupper(substr(name, 1, 1)),
                                  tolower(substr(name, 2, nchar(name))))) |>
      transmute(name_clean, gender_oa = gender) |>
      filter(!tolower(name_clean) %in% tolower(if (nrow(ssa) > 0) ssa$name_clean else character()))
  } else tibble()

  lkp <- bind_rows(ssa, intl) |> distinct(name_clean, .keep_all = TRUE)
  oa_names |>
    left_join(lkp, by = "name_clean") |>
    filter(!is.na(gender_oa)) |>
    select(abstract_id, gender_oa) |>
    distinct(abstract_id, .keep_all = TRUE)
} else tibble(abstract_id = character(), gender_oa = character())

#' Extract gender from a sidecar tibble into a standard format
#'
#' @param tbl Tibble. A sidecar CSV with \code{abstract_id} and a gender column.
#' @param gender_col Character. Name of the gender column in \code{tbl}.
#' @param new_name Character. Name for the output gender column.
#' @return Tibble with columns \code{abstract_id} and \code{new_name},
#'   one row per abstract (deduplicated), NAs removed.
#' @keywords internal
extract_gender <- function(tbl, gender_col, new_name) {
  if (nrow(tbl) == 0 || !gender_col %in% names(tbl)) {
    return(tibble(abstract_id = character(), !!new_name := character()))
  }
  tbl |>
    filter(!is.na(.data[[gender_col]])) |>
    transmute(abstract_id, !!new_name := .data[[gender_col]]) |>
    distinct(abstract_id, .keep_all = TRUE)
}

g3 <- extract_gender(g_pub, "first_author_gender", "gender_pubmed")
g4 <- extract_gender(g_obg, "obgyn_gender", "gender_obgyn")
g5 <- extract_gender(g_oax, "openalex_gender", "gender_oax")
g6 <- extract_gender(g_orc, "orcid_gender", "gender_orcid")
g7 <- extract_gender(g_opm, "op_gender", "gender_opm")
g8 <- extract_gender(tri_sr, "tri_gender", "gender_tri_sr")
g9 <- extract_gender(tri_2nd, "tri_gender", "gender_tri_2nd")

# Join all gender sources (extract_gender already deduplicates by abstract_id)
matches <- matches |>
  left_join(npi_gender_tbl, by = "abstract_id") |>
  left_join(oa_gender_tbl, by = "abstract_id") |>
  left_join(g3, by = "abstract_id") |>
  left_join(g4, by = "abstract_id") |>
  left_join(g5, by = "abstract_id") |>
  left_join(g6, by = "abstract_id") |>
  left_join(g7, by = "abstract_id") |>
  left_join(g8, by = "abstract_id") |>
  left_join(g9, by = "abstract_id") |>
  assert_rows("gender waterfall joins")

# ── Cross-source conflict detection ──────────────────────────────────────────
# Detect abstracts where multiple sources disagree on gender.
# This is logged as a warning, not a hard stop — the waterfall priority
# resolves conflicts deterministically, but disagreements should be auditable.
gender_cols <- c("gender_npi", "gender_oa", "gender_pubmed", "gender_obgyn",
                 "gender_oax", "gender_orcid", "gender_opm",
                 "gender_tri_sr", "gender_tri_2nd", "first_author_gender")
gender_cols_present <- intersect(gender_cols, names(matches))

conflicts <- matches |>
  select(abstract_id, all_of(gender_cols_present)) |>
  rowwise() |>
  mutate(
    gender_values = list(unique(na.omit(c_across(all_of(gender_cols_present))))),
    n_distinct_gender = length(gender_values)
  ) |>
  ungroup() |>
  filter(n_distinct_gender > 1)

if (nrow(conflicts) > 0) {
  cli_alert_info("Gender cross-source disagreements: {nrow(conflicts)} abstracts")
  cli_alert_info("  (Expected: SSA initial-based guesses corrected by full-name sources)")
  conflict_detail <- conflicts |>
    mutate(values = sapply(gender_values, paste, collapse = " vs ")) |>
    select(abstract_id, values)
  write_csv(conflict_detail, here("data", "processed", "gender_conflicts.csv"))
  cli_alert_info("  Conflict log: data/processed/gender_conflicts.csv")
} else {
  cli_alert_success("No gender conflicts across sources")
}

# Also check state conflicts (NPI state vs PubMed-derived state)
if ("first_author_state" %in% names(matches) && nrow(npi) > 0) {
  npi_state_tbl <- npi |>
    filter(npi_match_confidence == "high", !is.na(npi_state)) |>
    select(abstract_id, npi_state) |>
    assert_unique_keys("NPI state check")
  state_check <- matches |>
    select(abstract_id, first_author_state) |>
    inner_join(npi_state_tbl, by = "abstract_id") |>
    filter(!is.na(first_author_state), !is.na(npi_state),
           toupper(first_author_state) != toupper(npi_state))
  if (nrow(state_check) > 0) {
    cli_alert_warning("State conflicts: {nrow(state_check)} abstracts (PubMed vs NPI)")
  } else {
    cli_alert_success("No state conflicts (PubMed vs NPI)")
  }
}

# Build unified gender with priority waterfall
matches <- matches |>
  mutate(
    gender_unified = coalesce(
      gender_npi,              # 1: NPI
      gender_oa,               # 2: OpenAlex full name
      gender_pubmed,           # 3: PubMed full name search
      gender_obgyn,            # 4: OB/GYN publication search
      gender_oax,              # 5: OpenAlex author search
      gender_orcid,            # 6: ORCID
      gender_opm,              # 7: Open Payments
      gender_tri_sr,           # 8: Senior author triangulation
      gender_tri_2nd,          # 9: Second author triangulation
      first_author_gender      # 10: SSA/genderize from 09c (base)
    ),
    gender_source = case_when(
      !is.na(gender_npi)     ~ "npi",
      !is.na(gender_oa)      ~ "openalex",
      !is.na(gender_pubmed)  ~ "pubmed_fullname",
      !is.na(gender_obgyn)   ~ "obgyn_pubs",
      !is.na(gender_oax)     ~ "openalex_search",
      !is.na(gender_orcid)   ~ "orcid",
      !is.na(gender_opm)     ~ "open_payments",
      !is.na(gender_tri_sr)  ~ "senior_triangulation",
      !is.na(gender_tri_2nd) ~ "second_triangulation",
      !is.na(first_author_gender) ~ "ssa",
      TRUE ~ NA_character_
    ),
    # Count how many independent sources contributed a gender value
    gender_n_sources = rowSums(!is.na(across(all_of(gender_cols_present)))),
    # Flag abstracts where sources disagreed
    gender_conflict = gender_n_sources >= 2 & abstract_id %in% conflicts$abstract_id
  ) |>
  # Drop intermediate gender columns — keep unified + source + conflict metadata
  select(-any_of(c("gender_npi", "gender_oa", "gender_pubmed", "gender_obgyn",
                    "gender_oax", "gender_orcid", "gender_opm",
                    "gender_tri_sr", "gender_tri_2nd",
                    "first_author_gender", "first_author_gender_p")))

n_gender <- sum(!is.na(matches$gender_unified))
n_conflict <- sum(matches$gender_conflict, na.rm = TRUE)
n_concordant <- n_gender - n_conflict
pct_concordant <- round(n_concordant / n_gender * 100, 1)
cli_alert_success("Unified gender: {n_gender} / {nrow(matches)} ({round(n_gender/nrow(matches)*100,1)}%)")
cli_alert_info("  Concordant (all sources agree): {n_concordant} ({pct_concordant}%)")
cli_alert_info("  Conflicts resolved by priority: {n_conflict} ({round(n_conflict/n_gender*100,1)}%)")
cat("By source:\n")
print(table(matches$gender_source, useNA = "ifany"))
cat("\nDistribution:\n")
print(table(matches$gender_unified, useNA = "ifany"))
cat("\nSources per abstract:\n")
print(table(matches$gender_n_sources, useNA = "ifany"))

# ── 3. Merge NPI columns ────────────────────────────────────────────────────
cli_h2("3. NPI columns")
if (nrow(npi) > 0) {
  npi_cols <- npi |>
    select(abstract_id, npi_number, npi_gender, npi_state, npi_subspecialty,
           npi_match_score, npi_match_confidence, npi_match_strategy,
           npi_full_name, npi_acog_district) |>
    assert_unique_keys("NPI sidecar")
  matches <- matches |>
    left_join(npi_cols, by = "abstract_id") |>
    assert_rows("NPI columns")
  cli_alert_info("NPI by confidence:")
  print(table(matches$npi_match_confidence, useNA = "ifany"))
}

# ── 4. Merge ORCID columns ──────────────────────────────────────────────────
cli_h2("4. ORCID columns")
if (nrow(orcid) > 0 && "orcid_country" %in% names(orcid)) {
  orcid_slim <- orcid |>
    select(abstract_id, any_of(c("orcid_country", "orcid_institution"))) |>
    assert_unique_keys("ORCID sidecar")
  matches <- matches |>
    left_join(orcid_slim, by = "abstract_id") |>
    assert_rows("ORCID columns")
  cli_alert_info("ORCID country: {sum(!is.na(matches$orcid_country))}")
}

# ── 5. Unified state and subspecialty ────────────────────────────────────────
cli_h2("5. Unified state & subspecialty")
matches <- matches |>
  mutate(
    state_unified = coalesce(npi_state, first_author_state),
    subspecialty_unified = coalesce(npi_subspecialty, subspecialty)
  )
cli_alert_info("State coverage: {sum(!is.na(matches$state_unified))} / {nrow(matches)}")
cli_alert_info("Subspecialty coverage: {sum(!is.na(matches$subspecialty_unified))} / {nrow(matches)}")

# ── 6. Write final output ───────────────────────────────────────────────────
cli_h2("6. Writing final output")
write_csv(matches, matches_path)
cli_alert_success("Saved abstracts_with_matches.csv: {nrow(matches)} rows × {ncol(matches)} columns")

# ── Summary ──────────────────────────────────────────────────────────────────
cli_h2("Demographics summary")
demo_summary <- tibble(
  column = c("gender_unified", "state_unified", "subspecialty_unified",
             "practice_type", "career_stage", "npi_match_confidence (high)"),
  coverage = c(
    sum(!is.na(matches$gender_unified)),
    sum(!is.na(matches$state_unified)),
    sum(!is.na(matches$subspecialty_unified)),
    sum(!is.na(matches$practice_type)),
    sum(!is.na(matches$career_stage)),
    sum(matches$npi_match_confidence == "high", na.rm = TRUE)
  ),
  pct = round(coverage / nrow(matches) * 100, 1)
)
print(demo_summary)
