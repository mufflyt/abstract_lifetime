# 10g_second_author_triangulation.R — Resolve first author full names via
# PubMed co-author search with the SECOND listed author.
#
# For authors missing gender where we have a second coauthor name:
# Search PubMed for "FirstAuthorLastName[AU] AND SecondAuthorLastName[AU]"
# Extract the first author's full first name from the PubMed XML result.
# Then run gender inference on that name.
#
# Rationale: the second author is typically a closer collaborator (co-resident,
# research partner) than the senior author, so co-publications are more likely.
#
# Writes: data/processed/second_author_triangulation.csv
# Updates: output/abstracts_with_matches.csv (gender_unified)

suppressPackageStartupMessages({
  library(here); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(rentrez); library(xml2)
})

source(here("R", "utils_text.R"))

cli_h1("Second Author Triangulation for Name Resolution")

d <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)
abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)

# Target: missing gender with parseable second author
gender_col <- if ("gender_unified" %in% names(d)) "gender_unified" else "first_author_gender"
missing <- d |> filter(is.na(.data[[gender_col]]))
missing_abs <- abs |>
  filter(abstract_id %in% missing$abstract_id,
         !is.na(authors_raw), str_count(authors_raw, ",") >= 1) |>
  mutate(
    parts = str_split(authors_raw, ",\\s*"),
    second_raw = sapply(parts, function(p) {
      p <- str_squish(p[!str_detect(p, "\\.\\.\\.")])
      p <- p[nchar(p) > 1]
      if (length(p) >= 2) p[2] else NA_character_
    }),
    first_norm = vapply(author_name_first, normalize_author, character(1)),
    second_norm = vapply(second_raw, normalize_author, character(1)),
    first_last = trimws(str_extract(first_norm, "^[a-z][a-z '-]+")),
    second_last = trimws(str_extract(second_norm, "^[a-z][a-z '-]+"))
  ) |>
  filter(!is.na(first_last), !is.na(second_last),
         nchar(first_last) >= 2, nchar(second_last) >= 2,
         first_last != second_last) |>  # skip same-surname pairs (false matches)
  select(abstract_id, author_name_first, second_raw, first_last, second_last)

cli_alert_info("Targets: {nrow(missing_abs)} authors with second author")

has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
delay <- if (has_key) 0.1 else 0.35

results <- list()
for (i in seq_len(nrow(missing_abs))) {
  row <- missing_abs[i, ]
  if (i %% 50 == 0) cli_alert_info("  {i}/{nrow(missing_abs)}")

  # Search PubMed: first author + second author
  query <- sprintf('"%s"[AU] AND "%s"[AU]', row$first_last, row$second_last)

  Sys.sleep(delay)
  search_result <- tryCatch(
    rentrez::entrez_search(db = "pubmed", term = query, retmax = 5),
    error = function(e) NULL
  )

  if (is.null(search_result) || length(search_result$ids) == 0) next
  if (length(search_result$ids) > 10) next  # too broad

  # Fetch first result
  Sys.sleep(delay)
  xml_text <- tryCatch(
    rentrez::entrez_fetch(db = "pubmed", id = search_result$ids[1], rettype = "xml"),
    error = function(e) NA_character_
  )
  if (is.na(xml_text)) next

  doc <- tryCatch(read_xml(xml_text), error = function(e) NULL)
  if (is.null(doc)) next

  # Find the author whose last name matches our first author
  author_nodes <- xml_find_all(doc, "//Author")
  resolved_first <- NA_character_
  for (a in author_nodes) {
    a_last <- xml_text(xml_find_first(a, "LastName"))
    if (!is.na(a_last) && tolower(trimws(a_last)) == tolower(row$first_last)) {
      a_fore <- xml_text(xml_find_first(a, "ForeName"))
      if (!is.na(a_fore) && nchar(a_fore) > 2) {
        resolved_first <- a_fore
        break
      }
    }
  }

  if (!is.na(resolved_first)) {
    results[[length(results) + 1]] <- tibble(
      abstract_id = row$abstract_id,
      aagl_author = row$author_name_first,
      second_author = row$second_raw,
      resolved_first_name = resolved_first,
      pubmed_query = query,
      n_results = length(search_result$ids),
      pmid_used = search_result$ids[1]
    )
  }
}

resolved <- bind_rows(results)
cli_alert_success("Resolved {nrow(resolved)} first names via second author triangulation")

if (nrow(resolved) > 0) {
  #' @describeIn clean_first_name Strip trailing initials and title-case
  #' @keywords internal
  clean_first_name <- function(n) {
    n <- trimws(n)
    n <- sub("\\s+[A-Z]$", "", n)
    n <- sub("\\s+[A-Z]\\.$", "", n)
    parts <- strsplit(n, "\\s+")[[1]]
    n <- parts[1]
    if (is.na(n) || nchar(n) < 2) return(NA_character_)
    paste0(toupper(substr(n, 1, 1)), tolower(substr(n, 2, nchar(n))))
  }

  resolved <- resolved |>
    mutate(name_clean = vapply(resolved_first_name, clean_first_name, character(1)))

  # SSA pass
  ssa <- tryCatch({
    gender::gender(unique(resolved$name_clean[!is.na(resolved$name_clean)]),
                   years = c(1930, 2012), method = "ssa") |>
      select(name_clean = name, tri_gender = gender)
  }, error = function(e) tibble())

  # International lookup
  intl_path <- here("data", "validation", "international_gender_lookup.csv")
  intl <- tibble()
  if (file.exists(intl_path)) {
    intl <- read_csv(intl_path, show_col_types = FALSE) |>
      mutate(name_clean = paste0(toupper(substr(name, 1, 1)),
                                  tolower(substr(name, 2, nchar(name))))) |>
      transmute(name_clean, tri_gender = gender)
    ssa_names <- if (nrow(ssa) > 0) tolower(ssa$name_clean) else character()
    intl <- intl |> filter(!tolower(name_clean) %in% ssa_names)
  }

  gender_lkp <- bind_rows(ssa, intl) |> distinct(name_clean, .keep_all = TRUE)

  resolved <- resolved |>
    left_join(gender_lkp, by = "name_clean")

  n_gendered <- sum(!is.na(resolved$tri_gender))
  cli_alert_info("Gender resolved: {n_gendered} / {nrow(resolved)}")

  # Save sidecar CSV
  write_csv(resolved, here("data", "processed", "second_author_triangulation.csv"))

  # NOTE: Merge into abstracts_with_matches.csv is handled by 10e_merge_demographics.R

  # Show examples
  cli_alert_info("Sample resolved names:")
  for (k in seq_len(min(15, nrow(resolved)))) {
    cat(sprintf("  %-20s + %-20s -> %s (%s)\n",
                resolved$aagl_author[k], resolved$second_author[k],
                resolved$resolved_first_name[k],
                if (is.na(resolved$tri_gender[k])) "?" else resolved$tri_gender[k]))
  }
} else {
  write_csv(tibble(), here("data", "processed", "second_author_triangulation.csv"))
}
