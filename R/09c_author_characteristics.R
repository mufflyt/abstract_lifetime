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

authors_path <- here("data", "processed", "authors_pubmed.csv")
matches_path <- here("output", "abstracts_with_matches.csv")
abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")

authors <- read_csv(authors_path, show_col_types = FALSE)
matches <- read_csv(matches_path, show_col_types = FALSE)
abstracts <- read_csv(abstracts_path, show_col_types = FALSE)

cli_h1("Author characteristics")

# --- Gender inference (SSA data via `gender` package) ---
# We use the ssa method over the period 1930-2012 to cover most first authors.
first_names <- authors |>
  filter(position == 1, !is.na(first_name), nchar(first_name) >= 2) |>
  distinct(first_name) |>
  mutate(name_key = str_to_title(first_name))

gender_result <- tryCatch({
  if (requireNamespace("gender", quietly = TRUE)) {
    gender::gender(first_names$name_key,
                   years = c(1930, 2012),
                   method = "ssa") |>
      select(name_key = name, gender, proportion_male, proportion_female)
  } else tibble()
}, error = function(e) {
  cli_alert_warning("gender::gender failed: {e$message}")
  tibble()
})

if (nrow(gender_result) > 0) {
  gender_lkp <- first_names |>
    left_join(gender_result, by = "name_key") |>
    mutate(
      first_author_gender = gender,
      first_author_gender_p = pmax(proportion_male, proportion_female, na.rm = TRUE)
    ) |>
    select(first_name, first_author_gender, first_author_gender_p)
} else {
  gender_lkp <- tibble(first_name = character(),
                       first_author_gender = character(),
                       first_author_gender_p = numeric())
}

cli_alert_info("Gender calls: {sum(!is.na(gender_lkp$first_author_gender))} / {nrow(gender_lkp)}")

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
    first_author_state = affiliation_state,
    first_author_country = affiliation_country
  ) |>
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
