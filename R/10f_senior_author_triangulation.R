# 10f_senior_author_triangulation.R — Resolve first author full names via
# PubMed co-author search with the senior (last listed) author.
#
# For 300 authors missing gender where we have a senior author name:
# Search PubMed for "FirstAuthorLastName[AU] AND SeniorAuthorLastName[AU]"
# Extract the first author's full first name from the PubMed XML result.
# Then run gender inference on that name.
#
# Writes: data/processed/senior_triangulation.csv
# Updates: output/abstracts_with_matches.csv (gender_unified)

suppressPackageStartupMessages({
  library(here); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(rentrez); library(xml2)
})

source(here("R", "utils_text.R"))

cli_h1("Senior Author Triangulation for Name Resolution")

d <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)
abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)

# Target: missing gender with parseable senior author
missing <- d |> filter(is.na(gender_unified))
missing_abs <- abs |>
  filter(abstract_id %in% missing$abstract_id,
         !is.na(authors_raw), str_count(authors_raw, ",") >= 1) |>
  mutate(
    parts = str_split(authors_raw, ",\\s*"),
    senior_raw = sapply(parts, function(p) {
      p <- str_squish(p[!str_detect(p, "\\.\\.\\.")])
      p <- p[nchar(p) > 1]
      if (length(p) >= 2) tail(p, 1) else NA_character_
    }),
    first_norm = vapply(author_name_first, normalize_author, character(1)),
    senior_norm = vapply(senior_raw, normalize_author, character(1)),
    first_last = trimws(str_extract(first_norm, "^[a-z][a-z '-]+")),
    senior_last = trimws(str_extract(senior_norm, "^[a-z][a-z '-]+"))
  ) |>
  filter(!is.na(first_last), !is.na(senior_last),
         nchar(first_last) >= 2, nchar(senior_last) >= 2) |>
  select(abstract_id, author_name_first, senior_raw, first_last, senior_last)

cli_alert_info("Targets: {nrow(missing_abs)} authors with senior author")

has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
delay <- if (has_key) 0.1 else 0.35

results <- list()
for (i in seq_len(nrow(missing_abs))) {
  row <- missing_abs[i, ]
  if (i %% 50 == 0) cli_alert_info("  {i}/{nrow(missing_abs)}")

  # Search PubMed: first author + senior author
  query <- sprintf('"%s"[AU] AND "%s"[AU]', row$first_last, row$senior_last)

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
      senior_author = row$senior_raw,
      resolved_first_name = resolved_first,
      pubmed_query = query,
      n_results = length(search_result$ids),
      pmid_used = search_result$ids[1]
    )
  }
}

resolved <- bind_rows(results)
cli_alert_success("Resolved {nrow(resolved)} first names via senior author triangulation")

if (nrow(resolved) > 0) {
  # Run gender inference on resolved names
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

  # Save
  write_csv(resolved, here("data", "processed", "senior_triangulation.csv"))

  # Update main output
  if (n_gendered > 0) {
    gendered <- resolved |> filter(!is.na(tri_gender)) |> select(abstract_id, tri_gender)

    for (j in seq_len(nrow(gendered))) {
      idx <- which(d$abstract_id == gendered$abstract_id[j])
      if (length(idx) == 1 && is.na(d$gender_unified[idx])) {
        d$gender_unified[idx] <- gendered$tri_gender[j]
        d$gender_source[idx] <- "senior_triangulation"
      }
    }

    write_csv(d, here("output", "abstracts_with_matches.csv"))
    n_total_gender <- sum(!is.na(d$gender_unified))
    cli_alert_success("Updated gender: {n_total_gender} / {nrow(d)} ({round(n_total_gender/nrow(d)*100,1)}%)")
  }

  # Show examples
  cli_alert_info("Sample resolved names:")
  for (k in seq_len(min(15, nrow(resolved)))) {
    cat(sprintf("  %-20s + %-20s -> %s (%s)\n",
                resolved$aagl_author[k], resolved$senior_author[k],
                resolved$resolved_first_name[k],
                if (is.na(resolved$tri_gender[k])) "?" else resolved$tri_gender[k]))
  }
}
