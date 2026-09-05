#!/usr/bin/env Rscript
# build_docs_metadata.R — Regenerate the machine-readable documentation.
#
# Why this exists
# ---------------
# docs/data_inventory.csv, docs/data_dictionary.csv and docs/DATA_DICTIONARY.md
# were committed with NO producer in the repository. That is precisely the
# orphan-artefact pattern recorded as F15 in docs/FAILURE_MODES.md — a file that
# looks authoritative and that nobody can regenerate or check. This script is
# the missing producer.
#
# Each output is a join of two halves:
#
#   HAND-AUTHORED, committed under docs/_meta/
#     data_inventory_meta.csv   — what a file is for, who produces it, what one
#                                 row means, whether it is authoritative
#     data_dictionary_meta.csv  — what a variable means and how it is derived
#
#   COMPUTED, read from the live tree at run time
#     row and column counts, file sizes, modification times, git-tracked status,
#     coverage counts and percentages, observed value ranges and level sets
#
# Splitting them this way means a number can never go stale: prose is edited by
# hand, everything countable is recomputed. The script FAILS if the two halves
# disagree about which files or variables exist, so a new column or a deleted
# file cannot pass unnoticed.
#
# Usage: Rscript scripts/build_docs_metadata.R

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(purrr)
  library(tibble); library(cli)
})

cli_h1("Rebuilding the machine-readable documentation")

META_INV  <- here("docs", "_meta", "data_inventory_meta.csv")
META_DICT <- here("docs", "_meta", "data_dictionary_meta.csv")
FAD       <- here("output", "final_analytical_dataset.csv")

stopifnot(file.exists(META_INV), file.exists(META_DICT))

# ---------------------------------------------------------------- inventory --

cli_h2("data_inventory.csv")

inventory_paths <- function() {
  c(
    list.files(here("data", "processed"), full.names = TRUE),
    list.files(here("data", "validation"), full.names = TRUE),
    here("data", "Published_only.rds"),
    here("data", "cache", "jmig_2017_abstracts.json"),
    list.files(here("data", "cache", "checkpoints"), full.names = TRUE),
    list.files(here("output"), pattern = "\\.(csv|txt)$", full.names = TRUE),
    list.files(here("output", "tables"), full.names = TRUE),
    list.files(here("output", "figures"), pattern = "\\.csv$", full.names = TRUE),
    # Tracked companion to the gitignored deploy bundle. It is the only thing
    # under shiny/ that the inventory covers.
    here("shiny", "adjudication_app", "bundle_manifest.csv")
  )
}

#' Measure one file: shape, size, age, git status
#' @param path Absolute path.
#' @param tracked Character vector of git-tracked repo-relative paths.
#' @return One-row tibble.
#' @keywords internal
measure_file <- function(path, tracked) {
  rel <- sub(paste0("^", here(), "/"), "", path)
  ext <- tolower(tools::file_ext(path))
  n <- NA_integer_; k <- NA_integer_

  if (ext == "csv") {
    d <- tryCatch(
      suppressWarnings(read_csv(path, show_col_types = FALSE, progress = FALSE,
                                col_types = cols(.default = col_character()))),
      error = function(e) NULL)
    if (!is.null(d)) { n <- nrow(d); k <- ncol(d) }
  } else if (ext == "rds") {
    d <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.data.frame(d)) { n <- nrow(d); k <- ncol(d) }
  } else if (ext == "txt") {
    n <- length(readLines(path, warn = FALSE)); k <- 1L
  }

  tibble(path = rel, n_rows = n, n_cols = k,
         file_bytes = file.size(path),
         file_mtime = format(file.mtime(path), "%Y-%m-%d %H:%M"),
         tracked = if (rel %in% tracked) "yes" else "no")
}

tracked <- system("git ls-files", intern = TRUE)
present <- inventory_paths()
present <- present[file.exists(present)]
measured <- map_dfr(present, measure_file, tracked = tracked)

meta_inv <- read_csv(META_INV, show_col_types = FALSE)

undocumented <- setdiff(measured$path, meta_inv$path)
missing_file <- setdiff(meta_inv$path, measured$path)
if (length(undocumented) > 0) {
  cli_alert_danger("On disk but not in docs/_meta/data_inventory_meta.csv:")
  cli_ul(undocumented)
}
if (length(missing_file) > 0) {
  cli_alert_danger("Documented but absent from disk:")
  cli_ul(missing_file)
}
if (length(undocumented) > 0 || length(missing_file) > 0) {
  stop("Inventory metadata is out of step with the tree. Edit ",
       "docs/_meta/data_inventory_meta.csv and re-run.", call. = FALSE)
}

inventory <- meta_inv |>
  left_join(measured, by = "path") |>
  mutate(n_rows = ifelse(is.na(n_rows), "", as.character(n_rows)),
         n_columns = ifelse(is.na(n_cols), "", as.character(n_cols))) |>
  select(path, type, format, producer, consumers, grain, primary_key,
         n_rows, n_columns, tracked, reproducible, external_dependency,
         authoritative, file_bytes, file_mtime, notes)

write_csv(inventory, here("docs", "data_inventory.csv"), na = "")
cli_alert_success("docs/data_inventory.csv — {nrow(inventory)} files")

# --------------------------------------------------------------- dictionary --

cli_h2("data_dictionary.csv and DATA_DICTIONARY.md")

if (!file.exists(FAD)) {
  cli_alert_warning("No final_analytical_dataset.csv — skipping the dictionary")
} else {

fad <- read_csv(FAD, show_col_types = FALSE)
n_rows_fad <- nrow(fad)

profile <- imap_dfr(fad, function(x, nm) {
  nn <- sum(!is.na(x))
  uv <- length(unique(x[!is.na(x)]))
  allowed <- if (is.logical(x)) {
    "TRUE/FALSE"
  } else if (is.numeric(x)) {
    if (nn == 0) "" else sprintf("[%s, %s]",
                                 format(min(x, na.rm = TRUE), digits = 6),
                                 format(max(x, na.rm = TRUE), digits = 6))
  } else if (uv <= 25 && uv > 0) {
    paste(sort(unique(x[!is.na(x)])), collapse = " | ")
  } else {
    sprintf("%d distinct values", uv)
  }
  tibble(variable = nm, type = class(x)[1], n_distinct = uv,
         coverage_n = nn, coverage_pct = round(nn / n_rows_fad * 100, 1),
         allowed_values = allowed)
})

meta_dict <- read_csv(META_DICT, show_col_types = FALSE)

undoc <- setdiff(profile$variable, meta_dict$variable)
vanished <- setdiff(meta_dict$variable, profile$variable)
if (length(undoc) > 0) {
  cli_alert_danger("Columns in the dataset with no dictionary entry:")
  cli_ul(undoc)
}
if (length(vanished) > 0) {
  cli_alert_danger("Dictionary entries with no column:")
  cli_ul(vanished)
}
if (length(undoc) > 0 || length(vanished) > 0) {
  stop("Dictionary metadata is out of step with the dataset. Edit ",
       "docs/_meta/data_dictionary_meta.csv and re-run.", call. = FALSE)
}

dictionary <- profile |>
  left_join(meta_dict, by = "variable") |>
  select(variable, type, meaning, grain, source, derivation, producer,
         allowed_values, missing_meaning, coverage_n, coverage_pct,
         n_distinct, analysis_role, notes) |>
  arrange(match(variable, names(fad)))

write_csv(dictionary, here("docs", "data_dictionary.csv"), na = "")
cli_alert_success("docs/data_dictionary.csv — {nrow(dictionary)} variables")

# Grouped Markdown rendering. Groups are declared here; every variable must
# fall in exactly one, so a new column cannot be quietly left out of the prose.
GROUPS <- list(
  "Identity and congress" = c("abstract_id", "congress_year", "title", "session_type"),
  "AAGL author fields" = c("first_author_normalized", "last_author_normalized", "author_count",
                           "authors_truncated"),
  "Study characteristics derived from abstract text" = c(
    "is_rct", "sample_size", "is_academic", "is_us_based", "study_design",
    "is_multicenter", "has_funding", "stat_sig_reported", "has_numeric_results",
    "is_database_study", "has_industry", "has_trial_registration",
    "has_irb_statement", "abstract_word_count", "research_category",
    "primary_procedure", "result_positivity"),
  "Match result and score components" = c(
    "best_pmid", "best_score", "classification", "has_tie", "n_candidates",
    "title_sim", "title_pts", "abstract_pts", "first_au_pts", "last_au_pts",
    "coauthor_pts", "team_bonus", "journal_pts", "keyword_pts", "date_pts",
    "no_text_penalty"),
  "Matched publication" = c(
    "pub_title", "pub_journal", "pub_year", "pub_doi", "pub_first_author",
    "months_to_pub", "pub_types", "pub_type_canonical", "cited_by_count",
    "journal_impact_proxy"),
  "Author identity and demographics" = c(
    "n_authors", "n_authors_aagl", "n_unique_affiliations", "first_author_last",
    "first_author_first", "first_author_state", "first_author_country",
    "first_author_acog_district", "practice_type", "subspecialty",
    "career_stage", "demographics_from_matched_pub", "gender_unified",
    "gender_source", "gender_n_sources", "gender_conflict", "state_unified",
    "subspecialty_unified"),
  "NPI identity resolution" = c(
    "npi_number", "npi_gender", "npi_state", "npi_subspecialty",
    "npi_match_score", "npi_match_confidence", "npi_match_strategy",
    "npi_full_name", "npi_acog_district"),
  "ORCID enrichment" = c(
    "orcid_id", "orcid_country", "orcid_institution", "orcid_role",
    "orcid_department", "orcid_org", "orcid_n_works", "orcid_career_stage",
    "orcid_subspecialty", "orcid_false_positive"),
  "Adjudication and outcome" = c(
    "manual_decision", "manual_pmid", "final_published", "final_pmid",
    "final_pmid_shared")
)
grouped <- unlist(GROUPS, use.names = FALSE)
if (!setequal(grouped, dictionary$variable) || anyDuplicated(grouped) > 0) {
  stop("The DATA_DICTIONARY.md grouping in this script does not partition the ",
       "dataset's columns. Ungrouped: ",
       paste(setdiff(dictionary$variable, grouped), collapse = ", "),
       call. = FALSE)
}

esc <- function(x) gsub("\\|", "\\\\|", ifelse(is.na(x), "", as.character(x)))

lines <- c(
  "# Data Dictionary", "",
  "Every column of the **current final analytical dataset**,",
  sprintf("`output/final_analytical_dataset.csv` — **%s rows × %d columns**, one row per",
          format(n_rows_fad, big.mark = ","), nrow(dictionary)),
  "eligible AAGL oral presentation, keyed on `abstract_id`.", "",
  "Machine-readable form: [`data_dictionary.csv`](data_dictionary.csv). Both are",
  "regenerated by `scripts/build_docs_metadata.R`, which joins the hand-authored",
  "prose in `docs/_meta/data_dictionary_meta.csv` to counts recomputed from the",
  "dataset, and fails if the two disagree about which columns exist.",
  "[`tests/testthat/test-docs_drift.R`](../tests/testthat/test-docs_drift.R) asserts",
  "the same invariant from the other direction.", "",
  "Reading the coverage column: `coverage_pct` is the share of rows that are",
  "non-`NA`. Several enrichment variables can only exist for abstracts with a",
  "confirmed matched publication, so a low percentage is often the correct value",
  "rather than a data-quality problem — the `missing_meaning` column says which.",
  "", "---", ""
)

for (g in names(GROUPS)) {
  sub <- dictionary |>
    filter(variable %in% GROUPS[[g]]) |>
    arrange(match(variable, GROUPS[[g]]))
  lines <- c(lines, paste0("## ", g), "",
    "| variable | type | meaning | derivation | producer | allowed values | missing means | n | % | role | notes |",
    "|---|---|---|---|---|---|---|---:|---:|---|---|")
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    lines <- c(lines, sprintf(
      "| `%s` | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |",
      r$variable, esc(r$type), esc(r$meaning), esc(r$derivation), esc(r$producer),
      esc(r$allowed_values), esc(r$missing_meaning), r$coverage_n,
      formatC(r$coverage_pct, format = "f", digits = 1),
      esc(r$analysis_role), esc(r$notes)))
  }
  lines <- c(lines, "")
}

warn_path <- here("docs", "_meta", "data_dictionary_warnings.md")
if (file.exists(warn_path)) {
  lines <- c(lines, "---", "", readLines(warn_path, warn = FALSE))
}

writeLines(lines, here("docs", "DATA_DICTIONARY.md"))
cli_alert_success("docs/DATA_DICTIONARY.md — {length(lines)} lines")
}

cli_alert_success("Documentation metadata rebuilt")
