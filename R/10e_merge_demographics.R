# 10e_merge_demographics.R — Merge all demographic sources into abstracts_with_matches.csv
#
# Sources (priority order for each field):
# 1. NPI match (Tier A) — npi_gender, npi_state, npi_subspecialty (255 US authors)
# 2. OpenAlex name → gender inference — works for ALL authors regardless of pub match
# 3. ORCID demographics — country, institution (185 authors)
# 4. Existing PubMed-derived (definite/probable only, already in file)
#
# Key fix: gender is now computed from OpenAlex full first names (the abstract's
# own DOI, not the matched publication). This means gender is available for all
# 1063 authors with OpenAlex names, not just 188 confirmed matches.

suppressPackageStartupMessages({
  library(here); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble)
})

cli_h1("Merging all demographic sources")

matches <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)
npi <- read_csv(here("data", "processed", "npi_matches.csv"), show_col_types = FALSE)
oa <- read_csv(here("data", "processed", "openalex_author_names.csv"), show_col_types = FALSE)

# ---- 1. Gender from OpenAlex names (abstract DOI = correct person) ----
cli_h2("Gender from OpenAlex names")

oa_with_names <- oa |>
  filter(name_agrees == TRUE, !is.na(oa_first_name), nchar(oa_first_name) > 1)

# Clean first names for gender lookup
clean_first_name <- function(n) {
  if (is.na(n) || nchar(n) < 2) return(NA_character_)
  n <- trimws(n)
  n <- sub("\\s+[A-Z]$", "", n)
  n <- sub("\\s+[A-Z]\\.$", "", n)
  parts <- strsplit(n, "\\s+")[[1]]
  n <- parts[1]
  if (is.na(n) || nchar(n) < 2) return(NA_character_)
  paste0(toupper(substr(n, 1, 1)), tolower(substr(n, 2, nchar(n))))
}

oa_names_clean <- oa_with_names |>
  mutate(name_clean = vapply(oa_first_name, clean_first_name, character(1))) |>
  filter(!is.na(name_clean))

# SSA pass
ssa_result <- tryCatch({
  gender::gender(unique(oa_names_clean$name_clean),
                 years = c(1930, 2012), method = "ssa") |>
    select(name_clean = name, oa_gender = gender)
}, error = function(e) {
  cli_alert_warning("SSA failed: {e$message}")
  tibble()
})
cli_alert_info("SSA resolved: {nrow(ssa_result)} / {length(unique(oa_names_clean$name_clean))}")

# International lookup pass
intl_path <- here("data", "validation", "international_gender_lookup.csv")
intl_result <- tibble()
if (file.exists(intl_path)) {
  intl_lkp <- read_csv(intl_path, show_col_types = FALSE) |>
    mutate(name_clean = paste0(toupper(substr(name, 1, 1)),
                               tolower(substr(name, 2, nchar(name))))) |>
    transmute(name_clean, oa_gender = gender)
  resolved_so_far <- if (nrow(ssa_result) > 0) tolower(ssa_result$name_clean) else character()
  intl_result <- intl_lkp |> filter(!tolower(name_clean) %in% resolved_so_far)
  cli_alert_info("International lookup: {nrow(intl_result)} additional")
}

gender_lkp <- bind_rows(ssa_result, intl_result) |>
  distinct(name_clean, .keep_all = TRUE)

oa_gender <- oa_names_clean |>
  left_join(gender_lkp, by = "name_clean") |>
  filter(!is.na(oa_gender)) |>
  select(abstract_id, oa_gender) |>
  distinct(abstract_id, .keep_all = TRUE)

cli_alert_info("OpenAlex-derived gender: {nrow(oa_gender)} / {nrow(oa_with_names)}")

# ---- 2. Merge NPI demographics ----
cli_h2("NPI demographics")

npi_demo <- npi |>
  filter(npi_match_confidence == "high") |>
  select(abstract_id, npi_number, npi_gender, npi_state, npi_subspecialty,
         npi_match_score, npi_match_confidence, npi_match_strategy,
         npi_full_name, npi_acog_district)

cli_alert_info("NPI Tier A demographics: {nrow(npi_demo)}")

# ---- 3. ORCID demographics ----
orcid_demo <- tibble()
orcid_path <- here("data", "processed", "orcid_demographics.csv")
if (file.exists(orcid_path)) {
  orcid_demo <- read_csv(orcid_path, show_col_types = FALSE) |>
    select(abstract_id, orcid_country = orcid_country, orcid_institution = orcid_institution)
  cli_alert_info("ORCID demographics: {sum(!is.na(orcid_demo$orcid_country))}")
}

# ---- 4. Merge into matches ----
cli_h2("Merging into abstracts_with_matches.csv")

# Remove old columns that will be replaced
drop_cols <- c("npi_number", "npi_gender", "npi_state", "npi_subspecialty",
               "npi_match_score", "npi_match_confidence", "npi_match_strategy",
               "npi_full_name", "npi_acog_district",
               "orcid_country", "orcid_institution",
               "oa_gender")
for (col in intersect(drop_cols, names(matches))) matches[[col]] <- NULL

# Merge OpenAlex gender (available for all authors, not gated on pub match)
matches <- matches |>
  left_join(oa_gender, by = "abstract_id")

# Merge NPI (Tier A only)
matches <- matches |>
  left_join(npi_demo, by = "abstract_id")

# Merge ORCID country
if (nrow(orcid_demo) > 0) {
  matches <- matches |>
    left_join(orcid_demo, by = "abstract_id")
}

# Unified gender: NPI > OpenAlex > existing PubMed (definite/probable only)
matches <- matches |>
  mutate(
    # Normalize NPI M/F to female/male
    npi_gender_norm = case_when(npi_gender == "F" ~ "female", npi_gender == "M" ~ "male", TRUE ~ NA_character_),
    gender_unified = coalesce(npi_gender_norm, oa_gender, first_author_gender),
    gender_source = case_when(
      !is.na(npi_gender_norm) ~ "npi",
      !is.na(oa_gender) ~ "openalex",
      !is.na(first_author_gender) ~ "pubmed",
      TRUE ~ NA_character_
    )
  ) |>
  select(-npi_gender_norm)

# Summary
n_gender <- sum(!is.na(matches$gender_unified))
cli_alert_success("Unified gender coverage: {n_gender} / {nrow(matches)} ({round(n_gender/nrow(matches)*100,1)}%)")
cat("By source:\n")
print(table(matches$gender_source, useNA = "ifany"))
cat("\nGender distribution:\n")
print(table(matches$gender_unified, useNA = "ifany"))

cat("\nNPI coverage:\n")
print(table(matches$npi_match_confidence, useNA = "ifany"))

cat("\nNPI subspecialty:\n")
print(table(matches$npi_subspecialty, useNA = "ifany"))

write_csv(matches, here("output", "abstracts_with_matches.csv"))
cli_alert_success("Saved abstracts_with_matches.csv with {ncol(matches)} columns")
