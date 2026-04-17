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

# --- Filter out video presentations ---
# Study focuses on oral presentations only. Video abstracts (2022: 10, 2023: 38)
# are a different presentation format and excluded per study protocol.
# NAs (e.g., 2018 where tagger failed) are assumed Oral since pre-2022
# congresses had no video section.
if ("session_type" %in% names(abstracts)) {
  abstracts <- abstracts |>
    mutate(session_type = if_else(is.na(session_type), "Oral", session_type)) |>
    filter(session_type != "Video")
  cli_alert_info("After excluding video presentations: {nrow(abstracts)} abstracts")
}

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
      # Helper: filter out year-like numbers (1900-2099)
      drop_years <- function(nums) nums[nums < 1900 | nums > 2099]
      # Look for numbers preceded by n=, N=, or common patterns
      nums <- str_extract_all(text, "(?:n\\s*=\\s*|N\\s*=\\s*|total of\\s+|included\\s+)(\\d[\\d,]*)")[[1]]
      if (length(nums) > 0) {
        num_str <- str_extract(nums[1], "\\d[\\d,]*")
        val <- as.numeric(str_remove_all(num_str, ","))
        if (length(drop_years(val)) > 0) return(val)
      }
      # Fallback: largest number in text, excluding years
      all_nums <- as.numeric(str_extract_all(text, "\\d+")[[1]])
      all_nums <- all_nums[!is.na(all_nums) & all_nums > 1]
      all_nums <- drop_years(all_nums)
      if (length(all_nums) > 0) return(max(all_nums))
      NA_real_
    }),

    # Academic center detection from affiliation, authors, or abstract text
    is_academic = str_detect(
      tolower(coalesce(affiliation_raw, "", "")),
      "university|medical school|academic|teaching hospital|school of medicine"
    ) | str_detect(
      tolower(coalesce(search_text, "")),
      "\\buniversity\\b|\\bacademic\\b|teaching hospital|school of medicine|tertiary.*center|residency|fellowship"
    ),

    # US-based detection from affiliation, authors, or abstract text
    is_us_based = str_detect(
      tolower(paste(coalesce(affiliation_raw, ""), coalesce(search_text, ""))),
      paste0("\\b(", paste(c(
        tolower(state.name), tolower(state.abb), "united states", "usa",
        "new york", "los angeles", "chicago", "houston", "boston",
        "philadelphia", "san francisco", "seattle", "pittsburgh",
        "cleveland clinic", "mayo clinic", "johns hopkins",
        "columbia university", "stanford", "harvard", "yale",
        "duke", "emory", "vanderbilt", "cedars-sinai"
      ), collapse = "|"), ")\\b")
    )
  )

# --- Structured variable extraction (Cochrane MR000005) ---
abstracts <- abstracts |>
  mutate(
    search_text_lc = tolower(coalesce(search_text, abstract_full_text, "")),
    study_design = case_when(
      is_rct ~ "rct",
      str_detect(search_text_lc, "systematic review|meta-analysis|scoping review|narrative review|umbrella review") ~ "systematic_review",
      str_detect(search_text_lc, "prospective\\s+(cohort|observational|study|trial|longitudinal|database|registry|analysis)") ~ "prospective_cohort",
      str_detect(search_text_lc, paste0(
        "retrospective\\s+(cohort|review|chart|analysis|study|database|case)",
        "|chart review|database (study|analysis|review)|medical record review",
        "|retrospective analysis|reviewed.*charts|reviewed.*records",
        "|retrospective.*review of"
      )) ~ "retrospective_cohort",
      str_detect(search_text_lc, "case-control|case control") ~ "case_control",
      str_detect(search_text_lc, "case (series|report)|single.?case|video (presentation|case|demonstration)") ~ "case_series",
      str_detect(search_text_lc, "cross-sectional|cross sectional|survey|questionnaire") ~ "cross_sectional",
      str_detect(search_text_lc, "quality improvement|qi project|pdsa cycle") ~ "quality_improvement",
      str_detect(search_text_lc, "cost.?(effectiveness|analysis|benefit|utility)|economic (analysis|evaluation)") ~ "cost_analysis",
      str_detect(search_text_lc, "simulation|cadaver|bench.?top|dry lab|wet lab|ex.?vivo|animal model|porcine") ~ "simulation_lab",
      str_detect(search_text_lc, "validation (study|of)|validate[ds]?\\b|psychometric|reliability|accuracy") ~ "validation",
      str_detect(search_text_lc, "\\b(nsqip|acs-nsqip|acsnsqip|hcup|nis|nrd|seer|ncdb|national.*database|nationwide.*database|sart|puf)\\b") ~ "retrospective_cohort",
      str_detect(search_text_lc, "cohort study|cohort analysis|longitudinal study") ~ "prospective_cohort",
      str_detect(search_text_lc, "descriptive study|descriptive analysis") ~ "cross_sectional",
      TRUE ~ "other"
    ),
    is_multicenter = str_detect(search_text_lc,
      "multi-?center|multi-?site|multi-?institutional|\\d+\\s+(center|site|institution|hospital)s?\\b"),
    has_funding = str_detect(search_text_lc,
      "funded by|grant|supported by|\\bnih\\b|\\bnichd\\b|foundation|sponsor|funding"),
    stat_sig_reported = str_detect(search_text_lc,
      "p\\s*[<=]\\s*0\\.|confidence interval|odds ratio|hazard ratio|relative risk|\\bci\\b.*\\d")
  ) |>
  select(-search_text_lc)

cli_alert_info("Study design: {paste(names(table(abstracts$study_design)), table(abstracts$study_design), sep='=', collapse=', ')}")
cli_alert_info("Multicenter: {sum(abstracts$is_multicenter, na.rm=TRUE)} | Funded: {sum(abstracts$has_funding, na.rm=TRUE)} | Stat sig reported: {sum(abstracts$stat_sig_reported, na.rm=TRUE)}")

# --- Result positivity classification (Cochrane MR000005) ---
# Scan multiple text fields in priority order: conclusion > measurements/results
# > full text. Many AAGL abstracts lack a structured conclusion but have
# results in abstract_measurements or abstract_full_text.
source(here("R", "utils_positivity.R"))
abstracts <- abstracts |>
  mutate(
    .positivity_text = coalesce(
      if_else(nchar(coalesce(abstract_conclusion, "")) >= 20,
              abstract_conclusion, NA_character_),
      if_else(nchar(coalesce(abstract_measurements, "")) >= 20,
              abstract_measurements, NA_character_),
      if_else(nchar(coalesce(abstract_full_text, "")) >= 30,
              abstract_full_text, NA_character_)
    ),
    result_positivity = vapply(
      .positivity_text,
      classify_result_positivity, character(1)
    )
  ) |>
  select(-.positivity_text)
cli_alert_info("Result positivity: {paste(names(table(abstracts$result_positivity)), table(abstracts$result_positivity), sep='=', collapse=', ')}")

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
