#!/usr/bin/env Rscript
# 09e_enrich_orcid.R — Enrich first-author records with ORCID data
#
# Source: abstracts_cleaned.csv (the actual conference presenters), NOT
# authors_pubmed.csv (which contains publication authors, potentially different people).
#
# For each first author in abstracts_cleaned.csv:
#   1. Parse last name + initial from author_name_first (e.g. "R.S. Guido" → last="Guido", init="R")
#   2. Search ORCID public API: family-name + given-names initial
#   3. Disambiguate among candidates using PubMed affiliation string overlap (when available)
#   4. Fetch employment (role, department, organization) and works count
#   5. Flag non-medical roles as likely false positives
#
# New columns added to abstracts_with_matches.csv:
#   orcid_id              — ORCID iD (0000-0000-0000-0000)
#   orcid_role            — most recent self-reported job title
#   orcid_department      — most recent self-reported department
#   orcid_org             — most recent self-reported organization
#   orcid_n_works         — number of works on ORCID (proxy for productivity)
#   orcid_career_stage    — inferred: student/resident/fellow/early_faculty/senior_faculty
#   orcid_subspecialty    — inferred from role/dept when affiliation string is vague
#   orcid_false_positive  — TRUE if role appears non-medical
#
# Cache: data/cache/orcid/<orcid_id>.rds  (one RDS per profile)
# Rate limit: ~1 req/sec (ORCID public API is generous; no key required)

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(stringr)
  library(purrr); library(cli); library(httr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

cache_dir <- here("data", "cache", "orcid")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")
matches_path   <- here("output", "abstracts_with_matches.csv")
out_path       <- here("data", "processed", "orcid_enrichment.csv")

abstracts <- read_csv(abstracts_path, show_col_types = FALSE)
matches   <- read_csv(matches_path,   show_col_types = FALSE)


# Parse "R.S. Guido" → last="Guido", initials="R.S."
# For ORCID given-names query we pass all initials (e.g. "R.S.") not just the first,
# which narrows candidates and reduces false-positive matches.
parse_abstract_author <- function(x) {
  if (is.na(x) || nchar(x) < 2) return(list(last = NA_character_, initial = NA_character_))
  x     <- str_squish(x)
  parts <- str_split(x, "\\s+")[[1]]
  # Everything before the last token is the name/initials block
  init_parts <- head(parts, -1)
  initials   <- if (length(init_parts) > 0) paste(init_parts, collapse = " ") else NA_character_
  list(
    last    = tail(parts, 1),
    initial = initials
  )
}

first_authors <- abstracts |>
  filter(!is.na(author_name_first)) |>
  mutate(
    fa_last    = sapply(author_name_first, function(x) parse_abstract_author(x)$last),
    fa_initial = sapply(author_name_first, function(x) parse_abstract_author(x)$initial)
  ) |>
  filter(!is.na(fa_last), nchar(fa_last) > 1) |>
  mutate(
    disambig_aff = if_else(!is.na(affiliation_raw) & nchar(affiliation_raw) > 5,
                           affiliation_raw, NA_character_)
  ) |>
  select(abstract_id, fa_last, fa_initial, disambig_aff)

cli_h1("ORCID enrichment")
cli_alert_info("{nrow(first_authors)} first authors to process")

# ── Domain filter ─────────────────────────────────────────────────────────────
# Roles clearly outside medicine → flag as false positive
NON_MEDICAL_TERMS <- c(
  "bathymetry", "geology", "physics", "chemistry", "engineering", "software",
  "computer", "economics", "finance", "marketing", "attorney", "lawyer",
  "architect", "musician", "journalist", "agriculture", "dentist"
)

is_non_medical <- function(role) {
  if (is.na(role) || nchar(role) == 0) return(FALSE)
  any(str_detect(tolower(role), fixed(NON_MEDICAL_TERMS)))
}

# ── Career stage inference ────────────────────────────────────────────────────
infer_career_stage <- function(role, dept) {
  txt <- tolower(paste(role %||% "", dept %||% ""))
  if (str_detect(txt, "student|medical student|ms[0-9]|ms \\d")) return("student")
  if (str_detect(txt, "resident|pgy|house officer|intern\\b")) return("resident")
  if (str_detect(txt, "fellow|fellowship")) return("fellow")
  if (str_detect(txt, "assistant professor|lecturer|instructor|junior")) return("early_faculty")
  if (str_detect(txt, "associate professor|professor|director|chief|chair|division head|vice chair"))
    return("senior_faculty")
  if (str_detect(txt, "attending|clinician|physician|surgeon")) return("faculty")
  NA_character_
}

# ── Subspecialty inference from ORCID role/dept ───────────────────────────────
infer_subspecialty_orcid <- function(role, dept) {
  txt <- tolower(paste(role %||% "", dept %||% ""))
  if (str_detect(txt, "minimally invasive|migs|laparoscop|robotic gyn")) return("MIGS")
  if (str_detect(txt, "reproductive endocrin|rei|infertil|ivf")) return("REI")
  if (str_detect(txt, "oncolog|gyn onc|gynecologic oncology")) return("GYN_ONC")
  if (str_detect(txt, "pelvic floor|urogynecol|fpmrs|incontinence|prolapse")) return("FPMRS")
  if (str_detect(txt, "maternal.fetal|mfm|perinatol|high.risk")) return("MFM")
  if (str_detect(txt, "family planning|contraception|abortion")) return("family_planning")
  if (str_detect(txt, "urology|urologic")) return("urology")
  if (str_detect(txt, "obstetric|obstetrics")) return("obstetrics")
  if (str_detect(txt, "gynecol|obgyn|ob.gyn")) return("general_OBGYN")
  NA_character_
}

# ── ORCID search ──────────────────────────────────────────────────────────────
search_orcid <- function(last, first_word, aff = NA_character_) {
  q <- paste0("family-name:", URLencode(last, repeated = TRUE),
              "+AND+given-names:", URLencode(first_word, repeated = TRUE))
  url <- paste0("https://pub.orcid.org/v3.0/search/?q=", q, "&rows=10")
  resp <- tryCatch(
    GET(url, add_headers(Accept = "application/json"), timeout(15)),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NA_character_)
  body <- tryCatch(content(resp, "parsed"), error = function(e) NULL)
  if (is.null(body) || body$`num-found` == 0) return(NA_character_)

  orcids <- sapply(body$result, function(r) r$`orcid-identifier`$path)
  if (length(orcids) == 1) return(orcids[1])

  # Disambiguate: score each candidate by affiliation word overlap
  if (!is.na(aff) && nchar(aff) > 5) {
    aff_words <- unique(str_split(
      tolower(gsub("[^a-z0-9 ]", " ", aff)), "\\s+"
    )[[1]])
    aff_words <- aff_words[nchar(aff_words) > 3]

    scores <- map_dbl(orcids[seq_len(min(5, length(orcids)))], function(oid) {
      p <- fetch_orcid_person(oid)
      if (is.null(p)) return(0)
      bio <- tolower(p$biography$content %||% "")
      kws <- tolower(paste(
        sapply(p$keywords$keyword %||% list(), function(k) k$content %||% ""),
        collapse = " "
      ))
      combined <- paste(bio, kws)
      if (nchar(combined) < 3) return(0)
      sum(sapply(aff_words, function(w) grepl(w, combined, fixed = TRUE))) / length(aff_words)
    })
    return(orcids[which.max(scores)])
  }
  orcids[1]
}

# ── Cached ORCID endpoint fetchers ───────────────────────────────────────────
.orcid_get <- function(orcid, endpoint) {
  cache_file <- file.path(cache_dir, paste0(orcid, "_", endpoint, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))
  Sys.sleep(0.25)
  resp <- tryCatch(
    GET(paste0("https://pub.orcid.org/v3.0/", orcid, "/", endpoint),
        add_headers(Accept = "application/json"), timeout(15)),
    error = function(e) NULL
  )
  result <- if (!is.null(resp) && status_code(resp) == 200)
    tryCatch(content(resp, "parsed"), error = function(e) NULL) else NULL
  saveRDS(result, cache_file)
  result
}

fetch_orcid_person      <- function(oid) .orcid_get(oid, "person")
fetch_orcid_employments <- function(oid) .orcid_get(oid, "employments")
fetch_orcid_works       <- function(oid) .orcid_get(oid, "works")

# ── Main loop ─────────────────────────────────────────────────────────────────
n  <- nrow(first_authors)
results <- vector("list", n)

cli_progress_bar("Querying ORCID", total = n)
for (i in seq_len(n)) {
  row <- first_authors[i, ]
  rec <- tibble(
    abstract_id          = row$abstract_id,
    orcid_id             = NA_character_,
    orcid_role           = NA_character_,
    orcid_department     = NA_character_,
    orcid_org            = NA_character_,
    orcid_n_works        = NA_integer_,
    orcid_career_stage   = NA_character_,
    orcid_subspecialty   = NA_character_,
    orcid_false_positive = FALSE
  )

  oid <- tryCatch(
    search_orcid(row$fa_last, row$fa_initial %||% row$fa_last, row$disambig_aff),
    error = function(e) NA_character_
  )

  if (!is.na(oid)) {
    rec$orcid_id <- oid

    emp   <- fetch_orcid_employments(oid)
    works <- fetch_orcid_works(oid)

    rec$orcid_n_works <- if (!is.null(works)) length(works$group) else NA_integer_

    if (!is.null(emp) && length(emp$`affiliation-group`) > 0) {
      latest <- emp$`affiliation-group`[[1]]$summaries[[1]]$`employment-summary`
      rec$orcid_role       <- latest$`role-title`     %||% NA_character_
      rec$orcid_department <- latest$department        %||% NA_character_
      rec$orcid_org        <- latest$organization$name %||% NA_character_
    }

    rec$orcid_false_positive <- is_non_medical(rec$orcid_role)
    if (!rec$orcid_false_positive) {
      rec$orcid_career_stage <- infer_career_stage(rec$orcid_role, rec$orcid_department)
      rec$orcid_subspecialty <- infer_subspecialty_orcid(rec$orcid_role, rec$orcid_department)
    }
  }

  results[[i]] <- rec
  cli_progress_update()
}
cli_progress_done()

orcid_tbl <- bind_rows(results)

# ── Coverage report ───────────────────────────────────────────────────────────
cli_alert_success("ORCID found: {sum(!is.na(orcid_tbl$orcid_id))} / {n} ({round(mean(!is.na(orcid_tbl$orcid_id))*100,1)}%)")
cli_alert_info("False positives flagged: {sum(orcid_tbl$orcid_false_positive, na.rm=TRUE)}")
cli_alert_info("Career stage resolved: {sum(!is.na(orcid_tbl$orcid_career_stage))} ({round(mean(!is.na(orcid_tbl$orcid_career_stage))*100,1)}%)")
cli_alert_info("Subspecialty resolved: {sum(!is.na(orcid_tbl$orcid_subspecialty))} ({round(mean(!is.na(orcid_tbl$orcid_subspecialty))*100,1)}%)")
cli_alert_info("Works count available: {sum(!is.na(orcid_tbl$orcid_n_works))}")

cli_alert_info("Career stage distribution:")
print(table(orcid_tbl$orcid_career_stage, useNA = "ifany"))

cli_alert_info("Subspecialty distribution:")
print(table(orcid_tbl$orcid_subspecialty, useNA = "ifany"))

write_csv(orcid_tbl, out_path)
cli_alert_success("Wrote {out_path}")

# ── Merge into abstracts_with_matches.csv ─────────────────────────────────────
stale <- intersect(names(matches),
                   c("orcid_id", "orcid_role", "orcid_department", "orcid_org",
                     "orcid_n_works", "orcid_career_stage", "orcid_subspecialty",
                     "orcid_false_positive"))
for (col in stale) matches[[col]] <- NULL

matches <- matches |> left_join(orcid_tbl, by = "abstract_id")
write_csv(matches, matches_path)
cli_alert_success("Merged {ncol(orcid_tbl)-1} ORCID columns into abstracts_with_matches.csv")
