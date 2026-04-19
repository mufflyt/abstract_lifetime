#!/usr/bin/env Rscript
# 09i_gender_from_openalex.R — Gender inference via OpenAlex works search
#
# Complements 09h (PubMed): searches the same 17 OB/GYN + MIS journals
# using OpenAlex's works API (filter: journal ISSN + raw_author_name.search).
# OpenAlex may index papers absent from MEDLINE, yielding additional names.
#
# Query: primary_location.source.issn:<ISSNs> AND raw_author_name.search:<last>
# Match: authorship whose raw_author_name ends with <last> and whose initials
#        share the same first character as our <init>.
# Extract: author.display_name → first word = first name
#
# Cache: data/cache/openalex_author/<last>_<init>.rds
# Writes: data/processed/gender_from_openalex.csv
# Updates: output/abstracts_with_matches.csv

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(stringr)
  library(purrr); library(cli); library(httr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

cache_dir    <- here("data", "cache", "openalex_author")
matches_path <- here("output", "abstracts_with_matches.csv")
out_path     <- here("data", "processed", "gender_from_openalex.csv")

dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

matches   <- read_csv(matches_path, show_col_types = FALSE)
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                      show_col_types = FALSE)

MAILTO <- Sys.getenv("OPENALEX_MAILTO", "tyler.muffly@dhha.org")

# ISSNs for 17 OB/GYN + minimally invasive surgery journals
JOURNAL_ISSNS <- paste(c(
  "0002-9378",  # Am J Obstet Gynecol
  "0029-7844",  # Obstet Gynecol
  "1553-4650",  # J Minim Invasive Gynecol
  "0015-0282",  # Fertil Steril
  "0090-8258",  # Gynecol Oncol
  "2154-4212",  # Female Pelvic Med Reconstr Surg
  "0020-7292",  # Int J Gynaecol Obstet
  "1470-0328",  # BJOG
  "0301-2115",  # Eur J Obstet Gynecol Reprod Biol
  "2005-0380",  # J Gynecol Oncol
  "1933-7191",  # Reprod Sci
  "0010-7824",  # Contraception
  "0268-1161",  # Hum Reprod
  "1072-3714",  # Menopause
  "0960-7692",  # Ultrasound Obstet Gynecol
  "1086-8089"   # JSLS
), collapse = "|")

#' @rdname parse_name
#' @keywords internal
parse_name <- function(x) {
  if (is.na(x) || nchar(x) < 2) return(list(last = NA_character_, init = NA_character_))
  parts <- str_squish(x) |> str_split("\\s+") |> _[[1]]
  init  <- str_remove_all(paste(head(parts, -1), collapse = ""), "\\.")
  list(last = tail(parts, 1), init = init)
}

#' Extract first initial from an OpenAlex display name
#'
#' Given "Richard S. Guido" and last = "Guido", returns "R".
#'
#' @param display_name Character. OpenAlex author display name.
#' @param last Character. Known last name to strip.
#' @return Character scalar. First initial (uppercase), or \code{NA_character_}.
#' @keywords internal
display_to_first_init <- function(display_name, last) {
  parts <- str_squish(display_name) |> str_split("\\s+") |> _[[1]]
  # Remove the last name token(s) — take everything before last word matching <last>
  last_pos <- which(str_to_lower(parts) == str_to_lower(last))
  if (length(last_pos) == 0) last_pos <- length(parts)
  prefix <- parts[seq_len(min(last_pos) - 1)]
  if (length(prefix) == 0) return(NA_character_)
  toupper(substr(str_remove_all(prefix[1], "\\."), 1, 1))
}

#' Search OpenAlex for an author's full first name
#'
#' Queries the OpenAlex works API filtered by OB/GYN journal ISSNs and
#' author last name. Matches initials against display names to extract
#' the full first name. Results are cached per (last, init) pair.
#'
#' @param last Character. Author last name.
#' @param init Character. Author initials (e.g., "RS").
#' @return Character scalar. Full first name or \code{NA_character_}.
#' @keywords internal
fetch_openalex_first_name <- function(last, init) {
  cache_key  <- paste0(tolower(last), "_", tolower(init))
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  url <- paste0(
    "https://api.openalex.org/works",
    "?filter=primary_location.source.issn:", JOURNAL_ISSNS,
    ",raw_author_name.search:", URLencode(last, reserved = TRUE),
    "&select=id,authorships",
    "&per_page=10",
    "&mailto=", MAILTO
  )

  Sys.sleep(0.12)   # polite pool: ~8 req/sec
  resp <- tryCatch(GET(url, timeout(20)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) {
    saveRDS(NA_character_, cache_file); return(NA_character_)
  }

  parsed <- tryCatch(content(resp, "parsed", simplifyVector = FALSE),
                     error = function(e) NULL)
  results <- parsed[["results"]] %||% list()
  if (length(results) == 0) {
    saveRDS(NA_character_, cache_file); return(NA_character_)
  }

  first_init <- toupper(substr(init, 1, 1))
  fore <- NA_character_

  for (work in results) {
    for (auth in (work[["authorships"]] %||% list())) {
      raw  <- auth[["raw_author_name"]] %||% ""
      disp <- auth[["author"]][["display_name"]] %||% ""
      if (nchar(disp) < 3) next

      # Last name must appear at the end of raw_author_name (case-insensitive)
      if (!str_detect(tolower(raw), paste0("\\b", tolower(last), "$"))) next

      # First initial of display_name must match first character of our init
      d_init <- display_to_first_init(disp, last)
      if (is.na(d_init) || d_init != first_init) next

      # Extract first word of display_name as the first name
      parts <- str_split(str_squish(disp), "\\s+")[[1]]
      candidate <- str_remove_all(parts[1], "\\.")
      if (nchar(candidate) >= 2) { fore <- candidate; break }
    }
    if (!is.na(fore)) break
  }

  saveRDS(fore, cache_file)
  fore
}

# ── Build target list: abstracts still missing gender ─────────────────────────
no_gender <- matches |>
  filter(is.na(first_author_gender)) |>
  left_join(abstracts |> select(abstract_id, author_name_first), by = "abstract_id") |>
  filter(!is.na(author_name_first), nchar(author_name_first) > 2) |>
  mutate(
    fa_last = map_chr(author_name_first, ~ parse_name(.x)$last),
    fa_init = map_chr(author_name_first, ~ parse_name(.x)$init)
  ) |>
  filter(!is.na(fa_last), nchar(fa_last) > 1, !is.na(fa_init), nchar(fa_init) >= 1) |>
  distinct(fa_last, fa_init, .keep_all = TRUE) |>
  select(abstract_id, fa_last, fa_init)

cli_h1("Gender enrichment from OpenAlex works search")
cli_alert_info("{nrow(no_gender)} unique authors still missing gender")

# ── Main loop ──────────────────────────────────────────────────────────────────
n       <- nrow(no_gender)
results <- vector("list", n)
cli_progress_bar("OpenAlex author search", total = n)

for (i in seq_len(n)) {
  row <- no_gender[i, ]
  fore <- tryCatch(
    fetch_openalex_first_name(row$fa_last, row$fa_init),
    error = function(e) NA_character_
  )
  results[[i]] <- tibble(abstract_id = row$abstract_id, openalex_first_name = fore)
  cli_progress_update()
}
cli_progress_done()

name_tbl <- bind_rows(results) |>
  filter(!is.na(openalex_first_name), nchar(openalex_first_name) >= 2)

cli_alert_success("Full first names recovered: {nrow(name_tbl)} / {n} ({round(nrow(name_tbl)/n*100,1)}%)")

if (nrow(name_tbl) == 0) {
  cli_alert_warning("No names recovered — skipping gender inference")
  write_csv(tibble(), out_path)
  invisible(NULL)
} else {

# ── Gender inference ───────────────────────────────────────────────────────────
unique_names <- unique(name_tbl$openalex_first_name)

ssa_result <- tryCatch(
  gender::gender(unique_names, years = c(1930, 2012), method = "ssa") |>
    transmute(openalex_first_name = name, gender,
              proportion_male, proportion_female),
  error = function(e) { cli_alert_warning("SSA: {e$message}"); tibble() }
)
cli_alert_info("SSA resolved: {nrow(ssa_result)} / {length(unique_names)}")

ssa_resolved <- if (nrow(ssa_result) > 0) tolower(ssa_result$openalex_first_name) else character()
unresolved   <- unique_names[!tolower(unique_names) %in% ssa_resolved]

genderize_result <- tibble()
if (length(unresolved) > 0) {
  cli_alert_info("genderize.io for {length(unresolved)} names...")
  api_key <- Sys.getenv("GENDERIZE_API_KEY", "")

  fetch_batch <- function(batch) {
    params <- paste0("name[]=", URLencode(batch, repeated = TRUE), collapse = "&")
    if (nchar(api_key) > 0) params <- paste0(params, "&apikey=", api_key)
    resp <- tryCatch(GET(paste0("https://api.genderize.io/?", params), timeout(15)),
                     error = function(e) NULL)
    if (is.null(resp) || status_code(resp) != 200) return(tibble())
    parsed <- tryCatch(content(resp, "parsed"), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0) return(tibble())
    map(parsed, function(x) tibble(
      openalex_first_name = x$name    %||% NA_character_,
      gender              = x$gender  %||% NA_character_,
      proportion_male     = if (identical(x$gender, "male"))
                              x$probability %||% NA_real_ else 1 - (x$probability %||% NA_real_),
      proportion_female   = if (identical(x$gender, "female"))
                              x$probability %||% NA_real_ else 1 - (x$probability %||% NA_real_)
    )) |> list_rbind()
  }

  batches <- split(unresolved, ceiling(seq_along(unresolved) / 10))
  gz_raw  <- map(batches, ~ { Sys.sleep(0.5); tryCatch(fetch_batch(.x), error = function(e) tibble()) }) |>
    list_rbind()
  genderize_result <- if (nrow(gz_raw) > 0 && "openalex_first_name" %in% names(gz_raw))
    gz_raw |> filter(!is.na(openalex_first_name), !is.na(gender)) else tibble()

  cli_alert_info("genderize.io resolved: {nrow(genderize_result)} / {length(unresolved)}")
}

gender_lkp <- bind_rows(ssa_result, genderize_result) |>
  group_by(openalex_first_name) |> slice(1) |> ungroup() |>
  mutate(
    openalex_gender   = gender,
    openalex_gender_p = pmax(proportion_male, proportion_female, na.rm = TRUE)
  ) |>
  select(openalex_first_name, openalex_gender, openalex_gender_p)

gender_tbl <- name_tbl |>
  left_join(gender_lkp, by = "openalex_first_name") |>
  filter(!is.na(openalex_gender))

cli_alert_success("Gender resolved: {nrow(gender_tbl)} abstracts")
print(table(gender_tbl$openalex_gender, useNA = "ifany"))

write_csv(gender_tbl, out_path)
cli_alert_success("Wrote {out_path}")

# NOTE: Merge into abstracts_with_matches.csv is handled by 10e_merge_demographics.R
# This script only writes its sidecar CSV (gender_from_openalex.csv).

} # end nrow(name_tbl) > 0
