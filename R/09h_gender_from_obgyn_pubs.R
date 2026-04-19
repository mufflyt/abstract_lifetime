#!/usr/bin/env Rscript
# 09h_gender_from_obgyn_pubs.R — Gender inference via any OB/GYN publication
#
# For abstracts still missing gender after 09c/09f/09g:
#   1. Search PubMed: "Last Init"[Author] AND (obgyn journal list)
#   2. Extract full ForeName from matched author in returned XML
#   3. Infer gender via SSA + genderize.io
#
# This covers ~411 authors with no affiliation (09f couldn't help) and
# ~423 with affiliation where 09f found no PubMed match.
#
# Cache: data/cache/pubmed_obgyn/<last>_<init>.rds
# Writes: data/processed/gender_from_obgyn_pubs.csv

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(stringr)
  library(purrr); library(cli); library(xml2); library(rentrez); library(httr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

cache_dir    <- here("data", "cache", "pubmed_obgyn")
matches_path <- here("output", "abstracts_with_matches.csv")
out_path     <- here("data", "processed", "gender_from_obgyn_pubs.csv")

dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

matches   <- read_csv(matches_path, show_col_types = FALSE)
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                      show_col_types = FALSE)

has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
delay   <- if (has_key) 0.11 else 0.34

cli_h1("Gender enrichment from OB/GYN publication search")

# ── Top OB/GYN journal title abbreviations for PubMed [ta] field ──────────────
OBGYN_JOURNALS <- paste(
  '"Am J Obstet Gynecol"[ta]',
  '"Obstet Gynecol"[ta]',
  '"J Minim Invasive Gynecol"[ta]',
  '"Fertil Steril"[ta]',
  '"Gynecol Oncol"[ta]',
  '"Female Pelvic Med Reconstr Surg"[ta]',
  '"Int J Gynaecol Obstet"[ta]',
  '"BJOG"[ta]',
  '"Eur J Obstet Gynecol Reprod Biol"[ta]',
  '"J Gynecol Oncol"[ta]',
  '"Reprod Sci"[ta]',
  '"Contraception"[ta]',
  '"Hum Reprod"[ta]',
  '"Menopause"[ta]',
  '"Ultrasound Obstet Gynecol"[ta]',
  '"JSLS"[ta]',
  sep = " OR "
)
JOURNAL_FILTER <- paste0("(", OBGYN_JOURNALS, ")")

#' Parse an AAGL author string into last name and initials
#' @param x Character scalar. Author string (e.g., "R.S. Guido").
#' @return Named list with \code{last} and \code{init}.
#' @keywords internal
parse_name <- function(x) {
  if (is.na(x) || nchar(x) < 2) return(list(last = NA_character_, init = NA_character_))
  parts <- str_squish(x) |> str_split("\\s+") |> _[[1]]
  init  <- str_remove_all(paste(head(parts, -1), collapse = ""), "\\.")
  list(last = tail(parts, 1), init = init)
}

#' Search PubMed OB/GYN journals for an author's full first name
#'
#' Queries PubMed with \code{"LastName Initials"[Author]} filtered to a
#' curated list of OB/GYN journals. Extracts the ForeName from the XML.
#' Results are cached per (last, init) pair.
#'
#' @param last Character. Author last name.
#' @param init Character. Author initials (e.g., "RS").
#' @return Character scalar. Full first name or \code{NA_character_}.
#' @keywords internal
fetch_obgyn_first_name <- function(last, init) {
  cache_key  <- paste0(tolower(last), "_", tolower(init))
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  query <- paste0('"', last, " ", init, '"[Author] AND ', JOURNAL_FILTER)

  Sys.sleep(delay)
  ids <- tryCatch(
    entrez_search(db = "pubmed", term = query, retmax = 5)$ids,
    error = function(e) character(0)
  )
  if (length(ids) == 0) { saveRDS(NA_character_, cache_file); return(NA_character_) }

  Sys.sleep(delay)
  xml_txt <- tryCatch(
    entrez_fetch(db = "pubmed", id = ids[1:min(3, length(ids))], rettype = "xml"),
    error = function(e) NA_character_
  )
  if (is.na(xml_txt) || nchar(xml_txt) < 100) {
    saveRDS(NA_character_, cache_file); return(NA_character_)
  }

  doc <- tryCatch(read_xml(xml_txt), error = function(e) NULL)
  if (is.null(doc)) { saveRDS(NA_character_, cache_file); return(NA_character_) }

  fore <- NA_character_
  for (auth in xml_find_all(doc, ".//Author")) {
    a_last <- xml_text(xml_find_first(auth, "LastName"))  %||% ""
    a_init <- xml_text(xml_find_first(auth, "Initials"))  %||% ""
    a_fore <- xml_text(xml_find_first(auth, "ForeName"))  %||% ""
    if (tolower(a_last) == tolower(last) &&
        tolower(a_init) == tolower(init) &&
        nchar(a_fore) >= 2) {
      fore <- str_split(str_squish(a_fore), "\\s+")[[1]][1]
      break
    }
  }

  saveRDS(fore, cache_file)
  fore
}

# ── Build target list: abstracts still missing gender ─────────────────────────
# Handle column names: gender_unified, first_author_gender, or neither (process all)
gender_col <- if ("gender_unified" %in% names(matches)) {
  "gender_unified"
} else if ("first_author_gender" %in% names(matches)) {
  "first_author_gender"
} else {
  NULL
}
no_gender <- if (!is.null(gender_col)) {
  matches |> filter(is.na(.data[[gender_col]]))
} else {
  matches  # no gender column yet — process all
}
no_gender <- no_gender |>
  left_join(abstracts |> select(abstract_id, author_name_first), by = "abstract_id") |>
  filter(!is.na(author_name_first), nchar(author_name_first) > 2) |>
  mutate(
    fa_last = map_chr(author_name_first, ~ parse_name(.x)$last),
    fa_init = map_chr(author_name_first, ~ parse_name(.x)$init)
  ) |>
  filter(!is.na(fa_last), nchar(fa_last) > 1, !is.na(fa_init), nchar(fa_init) >= 1) |>
  # Deduplicate by author — one search per unique last+init pair
  distinct(fa_last, fa_init, .keep_all = TRUE) |>
  select(abstract_id, fa_last, fa_init)

cli_alert_info("{nrow(no_gender)} unique authors still missing gender")

# ── Main loop ──────────────────────────────────────────────────────────────────
n <- nrow(no_gender)
results <- vector("list", n)
cli_progress_bar("OB/GYN pub search", total = n)

for (i in seq_len(n)) {
  row <- no_gender[i, ]
  fore <- tryCatch(
    fetch_obgyn_first_name(row$fa_last, row$fa_init),
    error = function(e) NA_character_
  )
  results[[i]] <- tibble(abstract_id = row$abstract_id,
                         obgyn_first_name = fore)
  cli_progress_update()
}
cli_progress_done()

name_tbl <- bind_rows(results) |>
  filter(!is.na(obgyn_first_name), nchar(obgyn_first_name) >= 2)

cli_alert_success("Full first names recovered: {nrow(name_tbl)} / {n} ({round(nrow(name_tbl)/n*100,1)}%)")

if (nrow(name_tbl) == 0) {
  cli_alert_warning("No names recovered — skipping gender inference")
  write_csv(tibble(), out_path)
} else {

# ── Gender inference ───────────────────────────────────────────────────────────
unique_names <- unique(name_tbl$obgyn_first_name)

ssa_result <- tryCatch(
  gender::gender(unique_names, years = c(1930, 2012), method = "ssa") |>
    transmute(obgyn_first_name = name, gender,
              proportion_male, proportion_female),
  error = function(e) { cli_alert_warning("SSA: {e$message}"); tibble() }
)
cli_alert_info("SSA resolved: {nrow(ssa_result)} / {length(unique_names)}")

ssa_resolved <- if (nrow(ssa_result) > 0) tolower(ssa_result$obgyn_first_name) else character()
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
    if (is.null(parsed)) return(tibble())
    map(parsed, function(x) tibble(
      obgyn_first_name  = x$name    %||% NA_character_,
      gender            = x$gender  %||% NA_character_,
      proportion_male   = if (identical(x$gender, "male"))   x$probability %||% NA_real_
                          else 1 - (x$probability %||% NA_real_),
      proportion_female = if (identical(x$gender, "female")) x$probability %||% NA_real_
                          else 1 - (x$probability %||% NA_real_)
    )) |> list_rbind()
  }

  batches <- split(unresolved, ceiling(seq_along(unresolved) / 10))
  gz_raw  <- map(batches, ~ { Sys.sleep(0.5); tryCatch(fetch_batch(.x), error = function(e) tibble()) }) |>
    list_rbind()
  genderize_result <- if (nrow(gz_raw) > 0 && "obgyn_first_name" %in% names(gz_raw))
    gz_raw |> filter(!is.na(obgyn_first_name), !is.na(gender)) else tibble()

  cli_alert_info("genderize.io resolved: {nrow(genderize_result)} / {length(unresolved)}")
}

gender_lkp <- bind_rows(ssa_result, genderize_result) |>
  group_by(obgyn_first_name) |> slice(1) |> ungroup() |>
  mutate(
    obgyn_gender   = gender,
    obgyn_gender_p = pmax(proportion_male, proportion_female, na.rm = TRUE)
  ) |>
  select(obgyn_first_name, obgyn_gender, obgyn_gender_p)

gender_tbl <- name_tbl |>
  left_join(gender_lkp, by = "obgyn_first_name") |>
  filter(!is.na(obgyn_gender))

cli_alert_success("Gender resolved: {nrow(gender_tbl)} abstracts")
print(table(gender_tbl$obgyn_gender, useNA = "ifany"))

write_csv(gender_tbl, out_path)
cli_alert_success("Wrote {out_path}")

# NOTE: Merge into abstracts_with_matches.csv is handled by 10e_merge_demographics.R
# This script only writes its sidecar CSV (gender_from_obgyn_pubs.csv).

} # end nrow(name_tbl) > 0
