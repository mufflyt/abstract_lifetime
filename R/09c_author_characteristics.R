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
  library(tibble); library(purrr); library(cli)
})

source(here("R", "utils_acog.R"))
source(here("R", "utils_states.R"))

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
ssa_names <- if (nrow(ssa_result) > 0) tolower(ssa_result$name_clean) else character()
unresolved <- unique(first_names$name_clean[!tolower(first_names$name_clean) %in% ssa_names])
unresolved <- unresolved[!is.na(unresolved) & nchar(unresolved) >= 2]

genderize_result <- tibble()
if (length(unresolved) > 0) {
  cli_alert_info("Querying genderize.io for {length(unresolved)} remaining names...")
  genderize_result <- tryCatch({
    # genderize method in gender package handles the API call
    gender::gender(unresolved, method = "genderize") |>
      select(name_clean = name, gender, proportion_male, proportion_female)
  }, error = function(e) {
    cli_alert_warning("genderize.io failed: {e$message}")
    tibble()
  })
  cli_alert_info("genderize.io resolved: {nrow(genderize_result)} / {length(unresolved)}")
}

# Combine both passes (SSA takes priority where both resolve)
gender_combined <- bind_rows(ssa_result, genderize_result) |>
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
    first_author_country = affiliation_country
  ) |>
  # Re-parse state with the improved multi-strategy parser if raw is NA
  mutate(
    first_author_state = if_else(
      !is.na(first_author_state_raw), first_author_state_raw,
      vapply(first_author_aff, parse_us_state, character(1))
    )
  ) |>
  select(-first_author_state_raw, -first_author_aff) |>
  left_join(gender_lkp, by = c("first_author_first" = "first_name")) |>
  mutate(first_author_acog_district =
           vapply(first_author_state, acog_district_for_state, character(1)))

agg <- authors |>
  group_by(abstract_id) |>
  summarise(
    n_authors = n_distinct(position),
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
    n_authors = coalesce(n_authors, n_authors_aagl)
  )

write_csv(char_tbl, here("data", "processed", "author_characteristics.csv"))
cli_alert_success("Wrote author_characteristics.csv ({nrow(char_tbl)} rows)")

cli_alert_info("ACOG district distribution:")
print(table(char_tbl$first_author_acog_district, useNA = "ifany"))

cli_alert_info("Gender distribution:")
print(table(char_tbl$first_author_gender, useNA = "ifany"))

# --- Merge into abstracts_with_matches.csv ---
stale_cols <- intersect(names(matches),
                        c("n_authors", "n_authors_aagl", "n_unique_affiliations",
                          "first_author_last", "first_author_first",
                          "first_author_state", "first_author_country",
                          "first_author_acog_district",
                          "first_author_gender", "first_author_gender_p"))
for (c in stale_cols) matches[[c]] <- NULL
matches <- matches |> left_join(char_tbl, by = "abstract_id")
write_csv(matches, matches_path)
cli_alert_success("Merged {ncol(char_tbl) - 1} author columns into abstracts_with_matches.csv")
