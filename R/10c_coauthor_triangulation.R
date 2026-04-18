# 10c_coauthor_triangulation.R — Coauthor triangulation for NPI disambiguation
#
# For ambiguous NPI matches (multiple ABOG candidates), searches PubMed for
# papers where the first author co-published with AAGL coauthors. If a unique
# match is found, extracts the full name and institution to disambiguate.
#
# Also recovers names with accent mismatches (Şahin vs SAHIN) by finding
# the ASCII-normalized name via PubMed.

suppressPackageStartupMessages({
  library(here); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(rentrez); library(xml2)
})

source(here("R", "utils_text.R"))

cli_h1("Coauthor Triangulation")

npi <- read_csv(here("data", "processed", "npi_matches.csv"), show_col_types = FALSE)
abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
pool <- read_csv("/Users/tylermuffly/isochrones/data/canonical_abog/canonical_abog_npi_LATEST.csv",
                 show_col_types = FALSE)

# Target: ambiguous + low-scoring authors with coauthors
targets <- npi |>
  filter(npi_match_confidence %in% c("ambiguous", "low")) |>
  inner_join(abs |> select(abstract_id, authors_raw, author_name_first), by = "abstract_id") |>
  filter(!is.na(authors_raw), str_count(authors_raw, ",") >= 1)

cli_alert_info("Targets for coauthor triangulation: {nrow(targets)}")

# Parse coauthor last names
parse_coauthor_lastnames <- function(authors_raw, skip_first = TRUE) {
  parts <- str_split(authors_raw, ",\\s*")[[1]]
  parts <- str_squish(parts[!str_detect(parts, "\\.\\.\\.")])
  parts <- parts[nchar(parts) > 1]
  if (skip_first && length(parts) > 1) parts <- parts[-1]
  # Extract last name from each "FI LastName" format
  vapply(parts, function(p) {
    norm <- normalize_author(p)
    if (is.na(norm)) return(NA_character_)
    last <- trimws(str_extract(norm, "^[a-z][a-z '-]+"))
    if (is.na(last) || nchar(last) == 0) return(NA_character_)
    last
  }, character(1), USE.NAMES = FALSE) |>
    na.omit() |>
    as.character()
}

has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
delay <- if (has_key) 0.1 else 0.35

resolved <- list()
n_resolved <- 0

for (i in seq_len(nrow(targets))) {
  row <- targets[i, ]
  if (i %% 50 == 0) cli_alert_info("  {i}/{nrow(targets)}")

  # Parse first author last name and 2 coauthor last names
  first_norm <- normalize_author(row$author_name_first)
  if (is.na(first_norm)) next
  first_last <- trimws(str_extract(first_norm, "^[a-z][a-z '-]+"))
  if (is.na(first_last)) next

  coauth_lasts <- parse_coauthor_lastnames(row$authors_raw)
  if (length(coauth_lasts) == 0) next
  coauth_lasts <- head(coauth_lasts, 3)  # max 3 coauthors

  # Build PubMed query: first author + 1-2 coauthors
  au_terms <- paste0('"', first_last, '"[AU]')
  for (cl in head(coauth_lasts, 2)) {
    au_terms <- paste(au_terms, paste0('"', cl, '"[AU]'), sep = " AND ")
  }

  Sys.sleep(delay)
  result <- tryCatch(
    rentrez::entrez_search(db = "pubmed", term = au_terms, retmax = 5),
    error = function(e) NULL
  )

  if (is.null(result) || length(result$ids) == 0) next
  if (length(result$ids) > 5) next  # too many — not specific enough

  # Fetch first result and extract first author's full name
  Sys.sleep(delay)
  xml_text <- tryCatch(
    rentrez::entrez_fetch(db = "pubmed", id = result$ids[1], rettype = "xml"),
    error = function(e) NA_character_
  )
  if (is.na(xml_text)) next

  doc <- tryCatch(read_xml(xml_text), error = function(e) NULL)
  if (is.null(doc)) next

  # Find the author whose last name matches our first author
  author_nodes <- xml_find_all(doc, "//Author")
  matched_author <- NULL
  for (a in author_nodes) {
    a_last <- xml_text(xml_find_first(a, "LastName"))
    if (!is.na(a_last) && tolower(trimws(a_last)) == tolower(first_last)) {
      a_fore <- xml_text(xml_find_first(a, "ForeName"))
      if (!is.na(a_fore) && nchar(a_fore) > 1) {
        matched_author <- list(first = a_fore, last = a_last)
        break
      }
    }
  }

  if (is.null(matched_author)) next

  # Try to match this full name against the ABOG pool
  full_first_up <- toupper(trimws(matched_author$first))
  full_last_up <- toupper(trimws(matched_author$last))

  pool_match <- pool |>
    filter(toupper(trimws(last_name)) == full_last_up,
           toupper(trimws(first_name)) == full_first_up)

  if (nrow(pool_match) == 1) {
    # Unique ABOG match via coauthor triangulation!
    n_resolved <- n_resolved + 1
    resolved[[length(resolved) + 1]] <- tibble(
      abstract_id = row$abstract_id,
      coauth_npi = as.character(pool_match$npi[1]),
      coauth_full_name = paste(matched_author$first, matched_author$last),
      coauth_state = pool_match$state[1],
      coauth_gender = pool_match$npi_gender[1],
      coauth_subspecialty = pool_match$subspecialty[1],
      coauth_method = "pubmed_coauthor",
      coauth_pmid = result$ids[1],
      coauth_n_results = length(result$ids)
    )
  }
}

cli_alert_success("Coauthor triangulation resolved: {n_resolved} / {nrow(targets)}")

if (n_resolved > 0) {
  coauth_results <- bind_rows(resolved)

  # Update NPI matches
  for (j in seq_len(nrow(coauth_results))) {
    cr <- coauth_results[j, ]
    idx <- which(npi$abstract_id == cr$abstract_id)
    if (length(idx) == 1) {
      npi$npi_number[idx] <- cr$coauth_npi
      npi$npi_match_score[idx] <- 75L  # exact name via coauthor = high confidence
      npi$npi_match_strategy[idx] <- "coauthor_triangulation"
      npi$npi_match_confidence[idx] <- "high"
      npi$npi_gender[idx] <- cr$coauth_gender
      npi$npi_state[idx] <- cr$coauth_state
      npi$npi_subspecialty[idx] <- cr$coauth_subspecialty
      npi$npi_full_name[idx] <- cr$coauth_full_name
    }
  }

  write_csv(npi, here("data", "processed", "npi_matches.csv"))
  write_csv(coauth_results, here("data", "processed", "coauthor_triangulation.csv"))

  cli_alert_success("Updated NPI matches + saved coauthor_triangulation.csv")
  cli_alert_info("Subspecialties found:")
  print(table(coauth_results$coauth_subspecialty, useNA = "ifany"))
} else {
  cli_alert_info("No additional matches from coauthor triangulation")
}

# Final count
n_high <- sum(npi$npi_match_confidence == "high")
cli_alert_success("Total Tier A after triangulation: {n_high} / {nrow(npi)} ({round(n_high/nrow(npi)*100,1)}%)")
