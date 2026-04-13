# 02_clean_abstracts.R — Clean and normalize parsed abstracts

library(here)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(cli)
library(digest)

source(here("R", "utils_text.R"))

cfg <- config::get(file = here("config.yml"))

cli_h2("Cleaning Abstracts")

# Load parsed abstracts
parsed_path <- here("data", "processed", "abstracts_parsed.csv")
if (!file.exists(parsed_path)) {
  stop("No parsed abstracts found. Run 01b_parse_web.R or 01_parse_pdf.R first.")
}
abstracts <- read_csv(parsed_path, show_col_types = FALSE)
cli_alert_info("Loaded {nrow(abstracts)} abstracts")

# --- Normalize titles ---
cli_alert_info("Normalizing titles...")
abstracts <- abstracts |>
  mutate(title_normalized = vapply(title, normalize_title, character(1)))

# --- Parse and normalize author names ---
cli_alert_info("Parsing author names...")
abstracts <- abstracts |>
  mutate(
    # Split raw author string into individual names
    # ScienceDirect format: "FI LastName, FI LastName, ..." (e.g., "J Hayden, M Milla")
    # Also handles "... Name" ellipsis format
    authors_list = map(authors_raw, function(raw) {
      if (is.na(raw) || raw == "") return(character(0))
      raw <- str_remove_all(raw, "\\b(MD|DO|PhD|MPH|MS|FACOG|FACS|MSCR|DrPH|MHS|MBA|RN|BSN)\\b")
      raw <- str_remove_all(raw, "\\d+")  # Remove superscript numbers
      raw <- str_remove(raw, "^\\.{3}\\s*")  # Remove leading "... "
      raw <- str_remove(raw, ",\\s*\\.{3}\\s*$")  # Remove trailing ", ..."
      # Split on commas — handles "FI Last, FI Last" format
      names <- str_split(raw, ",")[[1]]
      names <- str_squish(names)
      # Remove ellipsis entries
      names <- names[!str_detect(names, "^\\.\\.")]
      names <- names[nchar(names) > 1]
      names
    }),
    author_count = map_int(authors_list, length),
    author_name_first = map_chr(authors_list, ~ if (length(.x) > 0) .x[1] else NA_character_),
    author_name_last = map_chr(authors_list, ~ if (length(.x) > 1) .x[length(.x)] else
      if (length(.x) == 1) .x[1] else NA_character_),
    first_author_normalized = vapply(author_name_first, normalize_author, character(1)),
    last_author_normalized = vapply(author_name_last, normalize_author, character(1)),
    all_authors_normalized = map(authors_list, normalize_authors)
  )

# --- Ensure structured section columns exist ---
section_cols <- c("abstract_objective", "abstract_design", "abstract_setting",
                  "abstract_patients_participants", "abstract_intervention",
                  "abstract_measurements", "abstract_conclusion")
for (col in section_cols) {
  if (!col %in% names(abstracts)) abstracts[[col]] <- NA_character_
}
if (!"abstract_full_text" %in% names(abstracts)) abstracts$abstract_full_text <- NA_character_

# --- Build full abstract text for semantic similarity ---
abstracts <- abstracts |>
  mutate(
    abstract_text = pmap_chr(list(
      abstract_objective, abstract_design, abstract_setting,
      abstract_patients_participants, abstract_intervention,
      abstract_measurements, abstract_conclusion, abstract_full_text
    ), function(obj, des, set, pat, int, meas, conc, full) {
      parts <- c(obj, des, set, pat, int, meas, conc)
      parts <- parts[!is.na(parts)]
      if (length(parts) > 0) return(paste(parts, collapse = " "))
      # Fall back to full text if no structured sections
      if (!is.na(full) && nchar(full) > 20) return(full)
      NA_character_
    })
  )

# --- Extract keywords ---
cli_alert_info("Extracting keywords...")
keywords_list <- extract_keywords(abstracts$abstract_text, top_n = 10)
abstracts$keywords <- keywords_list

# --- Ensure optional columns exist ---
if (!"affiliation_raw" %in% names(abstracts)) abstracts$affiliation_raw <- NA_character_
if (!"abstract_design" %in% names(abstracts)) abstracts$abstract_design <- NA_character_
if (!"abstract_patients_participants" %in% names(abstracts)) abstracts$abstract_patients_participants <- NA_character_

# --- Derive predictor variables ---
cli_alert_info("Deriving predictor variables...")

# Use abstract_full_text or title for text-based detection when structured fields are missing
abstracts <- abstracts |>
  mutate(
    search_text = coalesce(abstract_full_text, abstract_text, title, ""),

    # RCT detection from Design field or full text
    is_rct = str_detect(
      tolower(coalesce(abstract_design, search_text, "")),
      "randomi[sz]ed|rct|random\\s+alloc"
    ),

    # Sample size extraction from Patients field or full text
    sample_size = map_dbl(coalesce(abstract_patients_participants, search_text), function(text) {
      if (is.na(text) || text == "") return(NA_real_)
      # Look for numbers preceded by n=, N=, or common patterns
      nums <- str_extract_all(text, "(?:n\\s*=\\s*|N\\s*=\\s*|total of\\s+|included\\s+)(\\d[\\d,]*)")[[1]]
      if (length(nums) > 0) {
        num_str <- str_extract(nums[1], "\\d[\\d,]*")
        return(as.numeric(str_remove_all(num_str, ",")))
      }
      # Fallback: largest number in text
      all_nums <- as.numeric(str_extract_all(text, "\\d+")[[1]])
      all_nums <- all_nums[!is.na(all_nums) & all_nums > 1]
      if (length(all_nums) > 0) return(max(all_nums))
      NA_real_
    }),

    # Academic center detection from affiliation or authors
    is_academic = str_detect(
      tolower(coalesce(affiliation_raw, authors_raw, "")),
      "university|medical school|academic|teaching hospital|school of medicine"
    ),

    # US-based detection
    is_us_based = str_detect(
      tolower(coalesce(affiliation_raw, authors_raw, "")),
      paste0("\\b(", paste(c(
        state.name, state.abb, "united states", "usa",
        "new york", "los angeles", "chicago", "houston",
        "cleveland clinic", "mayo clinic", "johns hopkins"
      ), collapse = "|"), ")\\b")
    )
  )

# --- Abstract hash for dedup ---
abstracts <- abstracts |>
  mutate(
    abstract_hash = map2_chr(title, author_name_first, ~ {
      if (is.na(.x)) return(NA_character_)
      digest::digest(paste0(normalize_title(.x), "|",
                            normalize_author(.y %||% "")), algo = "md5")
    })
  )

# --- Validation ---
cli_alert_info("Validating...")
n_missing_title <- sum(is.na(abstracts$title) | nchar(abstracts$title) < 5)
n_missing_author <- sum(is.na(abstracts$first_author_normalized))

if (n_missing_title > 0) cli_alert_warning("{n_missing_title} abstracts with missing/short titles")
if (n_missing_author > 0) cli_alert_warning("{n_missing_author} abstracts with missing first author")

# Save
out_path <- here("data", "processed", "abstracts_cleaned.csv")
# Convert list columns for CSV
abstracts_save <- abstracts |>
  mutate(
    keywords_str = map_chr(keywords, ~ paste(.x, collapse = "; ")),
    all_authors_str = map_chr(all_authors_normalized, ~ paste(.x[!is.na(.x)], collapse = "; "))
  ) |>
  select(-keywords, -all_authors_normalized, -authors_list,
         -any_of("search_text"))

write_csv(abstracts_save, out_path)
cli_alert_success("Saved {nrow(abstracts_save)} cleaned abstracts to {out_path}")

# Summary stats
cli_h3("Summary")
cli_alert_info("Total abstracts: {nrow(abstracts)}")
cli_alert_info("RCTs: {sum(abstracts$is_rct, na.rm = TRUE)}")
cli_alert_info("Academic: {sum(abstracts$is_academic, na.rm = TRUE)}")
cli_alert_info("US-based: {sum(abstracts$is_us_based, na.rm = TRUE)}")
cli_alert_info("Median sample size: {median(abstracts$sample_size, na.rm = TRUE)}")
