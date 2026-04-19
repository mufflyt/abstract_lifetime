#!/usr/bin/env Rscript
# 09g_gender_from_orcid.R — Infer first-author gender from ORCID person records
#
# For each abstract with a matched ORCID iD (from 09e_enrich_orcid.R):
#   1. Read cached _person.rds file from data/cache/orcid/
#   2. Extract given-names → take first word as first name
#   3. Run SSA + genderize.io on first name
#   4. Coalesce with existing first_author_gender (never overwrite known values)
#
# Adds ~151 new gender values for no_match/possible abstracts where
# 09c couldn't assign gender (ORCID found but no publication match).
#
# Writes: data/processed/gender_from_orcid.csv
# Updates: output/abstracts_with_matches.csv

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(stringr)
  library(purrr); library(cli); library(httr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

cache_dir    <- here("data", "cache", "orcid")
orcid_path   <- here("data", "processed", "orcid_enrichment.csv")
matches_path <- here("output", "abstracts_with_matches.csv")
out_path     <- here("data", "processed", "gender_from_orcid.csv")

orcid_tbl <- read_csv(orcid_path,   show_col_types = FALSE)
matches   <- read_csv(matches_path, show_col_types = FALSE)

cli_h1("Gender enrichment from ORCID person records")

# ── Extract first name from cached person RDS ──────────────────────────────────
extract_first_name <- function(orcid_id) {
  if (is.na(orcid_id)) return(NA_character_)
  cache_file <- file.path(cache_dir, paste0(orcid_id, "_person.rds"))
  if (!file.exists(cache_file)) return(NA_character_)
  p <- tryCatch(readRDS(cache_file), error = function(e) NULL)
  if (is.null(p)) return(NA_character_)
  given <- p[["name"]][["given-names"]][["value"]] %||% NA_character_
  if (is.na(given) || nchar(given) < 2) return(NA_character_)
  # Take first word only: "Deborah A" → "Deborah", "Mary-Jane" → "Mary-Jane"
  str_split(str_squish(given), "\\s+")[[1]][1]
}

# Work only on abstracts with an ORCID match
orcid_with_id <- orcid_tbl |>
  filter(!is.na(orcid_id), !orcid_false_positive) |>
  select(abstract_id, orcid_id)

cli_alert_info("{nrow(orcid_with_id)} ORCID-matched abstracts to process")

# Extract first names from cache (no API calls — all local reads)
orcid_names <- orcid_with_id |>
  mutate(orcid_first_name = map_chr(orcid_id, extract_first_name)) |>
  filter(!is.na(orcid_first_name), nchar(orcid_first_name) >= 2)

cli_alert_success("First names extracted from cache: {nrow(orcid_names)} / {nrow(orcid_with_id)}")

# ── Gender inference ───────────────────────────────────────────────────────────
unique_names <- unique(orcid_names$orcid_first_name)
cli_alert_info("{length(unique_names)} unique first names to gender-classify")

# Pass 1: SSA
ssa_result <- tryCatch(
  gender::gender(unique_names, years = c(1930, 2012), method = "ssa") |>
    transmute(
      orcid_first_name  = name,
      gender            = gender,
      proportion_male   = proportion_male,
      proportion_female = proportion_female
    ),
  error = function(e) {
    cli_alert_warning("SSA failed: {e$message}")
    tibble()
  }
)
cli_alert_info("SSA resolved: {nrow(ssa_result)} / {length(unique_names)}")

# Pass 2: genderize.io for remainder
ssa_resolved <- if (nrow(ssa_result) > 0) tolower(ssa_result$orcid_first_name) else character()
unresolved   <- unique_names[!tolower(unique_names) %in% ssa_resolved]

genderize_result <- tibble()
if (length(unresolved) > 0) {
  cli_alert_info("genderize.io for {length(unresolved)} remaining names...")
  api_key <- Sys.getenv("GENDERIZE_API_KEY", "")

  fetch_batch <- function(batch) {
    params <- paste0("name[]=", URLencode(batch, repeated = TRUE), collapse = "&")
    if (nchar(api_key) > 0) params <- paste0(params, "&apikey=", api_key)
    resp <- tryCatch(
      GET(paste0("https://api.genderize.io/?", params), timeout(15)),
      error = function(e) NULL
    )
    if (is.null(resp) || status_code(resp) != 200) return(tibble())
    parsed <- tryCatch(content(resp, "parsed"), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0) return(tibble())
    map(parsed, function(x) {
      tibble(
        orcid_first_name  = x$name       %||% NA_character_,
        gender            = x$gender     %||% NA_character_,
        proportion_male   = if (identical(x$gender, "male"))
                              x$probability %||% NA_real_ else 1 - (x$probability %||% NA_real_),
        proportion_female = if (identical(x$gender, "female"))
                              x$probability %||% NA_real_ else 1 - (x$probability %||% NA_real_)
      )
    }) |> list_rbind()
  }

  batches      <- split(unresolved, ceiling(seq_along(unresolved) / 10))
  genderize_raw <- map(batches, function(b) {
    Sys.sleep(0.5)
    tryCatch(fetch_batch(b), error = function(e) tibble())
  }) |> list_rbind()

  genderize_result <- if (nrow(genderize_raw) > 0 &&
                          "orcid_first_name" %in% names(genderize_raw)) {
    genderize_raw |> filter(!is.na(orcid_first_name), !is.na(gender))
  } else tibble()

  cli_alert_info("genderize.io resolved: {nrow(genderize_result)} / {length(unresolved)}")
}

# Combine passes
gender_lkp <- bind_rows(ssa_result, genderize_result) |>
  group_by(orcid_first_name) |> slice(1) |> ungroup() |>
  mutate(
    orcid_gender   = gender,
    orcid_gender_p = pmax(proportion_male, proportion_female, na.rm = TRUE)
  ) |>
  select(orcid_first_name, orcid_gender, orcid_gender_p)

# Join back to abstract_ids
gender_tbl <- orcid_names |>
  left_join(gender_lkp, by = "orcid_first_name") |>
  filter(!is.na(orcid_gender))

cli_alert_success("Gender resolved: {nrow(gender_tbl)} abstracts")
cli_alert_info("Distribution:")
print(table(gender_tbl$orcid_gender, useNA = "ifany"))

write_csv(gender_tbl, out_path)
cli_alert_success("Wrote {out_path}")

# NOTE: Merge into abstracts_with_matches.csv is handled by 10e_merge_demographics.R
# This script only writes its sidecar CSV (gender_from_orcid.csv).
