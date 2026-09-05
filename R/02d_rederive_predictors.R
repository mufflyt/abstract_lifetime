# 02d_rederive_predictors.R — Recompute the text-derived study characteristics
# AFTER the abstract-text backfills have run.
#
# Why this exists
# ---------------
# R/02_clean_abstracts.R derives every text-based predictor by regex over
#
#   search_text = coalesce(abstract_full_text, abstract_text, title)
#
# but it runs BEFORE R/02b_backfill_abstract_text.R and
# scripts/backfill_sciencedirect_snippets.R recover the text for congress years
# 2012-2018. Those backfills patch `abstract_text` and nothing else, so the
# predictors were computed from the TITLE ALONE for 700 of the 1,106 abstracts
# and were never recomputed. The result was a step change at 2018/2019 in every
# text-derived variable — `has_numeric_results` was exactly 0.0% in each of the
# seven congresses 2012-2018 and 47-89% thereafter — while the outcome varies
# across the same boundary. Congress year is in neither the Cox nor the logistic
# model, so every coefficient was confounded by year through measurement.
# Documented as F3 in docs/FAILURE_MODES.md.
#
# This script re-runs the identical rules over the current, backfilled text.
#
# Scope: COVARIATES ONLY.
# ----------------------
# It deliberately does NOT touch `title`, `title_normalized`, the author
# columns, `keywords_str`, `all_authors_str` or `abstract_text` itself. Those
# feed R/03_search_pubmed.R and R/04_score_matches.R, and changing them would
# invalidate both the scores and the human adjudication recorded against them.
# Everything this script writes is a model covariate or a descriptor.
#
# One rule change, made deliberately
# ----------------------------------
# `has_numeric_results` and `result_positivity` originally read only the
# structured section columns (`abstract_measurements`, `abstract_conclusion`).
# Those columns are NA for every 2012-2018 abstract and the backfills do not
# populate them, so both variables were structurally 0/NA for 63% of the cohort.
# Both now fall back to `abstract_text`. This widens the construct slightly —
# a number may now come from any part of the abstract rather than from the
# results or conclusion specifically — and that is recorded here rather than
# left implicit.
#
# It also clears degenerate abstract_text
# ---------------------------------------
# scripts/backfill_sciencedirect_snippets.R wrote the page footnote
# "*: Corresponding author." into abstract_text for all 95 abstracts of the 2018
# congress, and a co-first-author footnote for one 2017 abstract. That is worse
# than a missing value: it is 24 characters, so it passes the `nchar >= 10` gate
# that 02b and 02c use to decide a row needs no backfill, and those rows are
# skipped forever. This script sets such values to NA so the backfills will
# retry them, and so search_text falls through to the title rather than to a
# footnote.
#
# Clearing them is score-neutral: score_match() gates the abstract-similarity
# component on `nchar(abstract_text) > 20`, which the footnote passes, but the
# resulting cosine similarity is 0 and all 96 affected abstracts already carry
# abstract_pts == 0 in match_scores.csv. Verified before the change.
#
# Safe to re-run. Idempotent given the same input.

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(stringr)
  library(purrr); library(cli)
})

source(here("R", "utils_positivity.R"))

cli_h2("Re-deriving study characteristics from backfilled abstract text")

abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")
abstracts <- read_csv(abstracts_path, show_col_types = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
for (col in c("affiliation_raw", "abstract_full_text", "abstract_design",
              "abstract_patients_participants", "abstract_measurements",
              "abstract_conclusion")) {
  if (!col %in% names(abstracts)) abstracts[[col]] <- NA_character_
}

# --- Clear degenerate abstract_text ------------------------------------------
# A real structured abstract in this corpus is 600+ characters. Anything under
# MIN_USABLE_CHARS, or matching a leading-footnote pattern, is page furniture.
MIN_USABLE_CHARS <- 100L

# Institutional affiliation patterns, applied to the AFFILIATION string, not to
# abstract body text. The previous definitions read the abstract body, where
# words like "residency", "fellowship" and "tertiary center" describe the study
# rather than the author's institution. Validated against real affiliations the
# body-text proxy agreed 54% of the time for academic status and 53% for US
# location, understating academic affiliation by half and overstating US
# location by 24 percentage points. PI decision, 2026-09-05; see appendix A21.
ACADEMIC_AFFILIATION_RE <- paste0(
  "universit|medical school|school of medicine|college of medicine|",
  "teaching hospital|academic medical|\\bmayo clinic\\b|cleveland clinic|",
  "institute of technology|hospital universit|centre hospitalier universitaire|",
  "\\bnhs\\b|karolinska|charit\u00e9"
)

# A US affiliation names a US state or the country. Non-US countries are tested
# first because "Ontario, California" and "Ontario, Canada" both contain a US
# state name, and international affiliations routinely name a city that
# collides with one.
NON_US_COUNTRY_RE <- paste0(
  "\\b(canada|united kingdom|england|scotland|wales|ireland|china|japan|india|",
  "australia|new zealand|brazil|mexico|argentina|chile|colombia|france|germany|",
  "italy|spain|portugal|netherlands|belgium|switzerland|austria|sweden|norway|",
  "denmark|finland|poland|czech|greece|turkey|israel|egypt|nigeria|south africa|",
  "saudi|emirates|qatar|kuwait|lebanon|jordan|iran|pakistan|bangladesh|",
  "singapore|malaysia|thailand|vietnam|indonesia|philippines|korea|taiwan|",
  "hong kong|russia|ukraine|romania|hungary|peru|ecuador|uruguay)\\b"
)
US_AFFILIATION_RE <- paste0(
  "\\b(", paste(c(tolower(state.name), tolower(state.abb),
                 "united states", "usa", "u\\.s\\.a", "u\\.s\\."),
               collapse = "|"), ")\\b"
)

degenerate <- !is.na(abstracts$abstract_text) &
  (nchar(abstracts$abstract_text) < MIN_USABLE_CHARS |
     str_detect(abstracts$abstract_text, "^\\s*\\*\\s*:"))

if (any(degenerate)) {
  cli_alert_warning(
    "Clearing {sum(degenerate)} degenerate abstract_text values \\
     ({paste(sort(unique(abstracts$congress_year[degenerate])), collapse = ', ')})"
  )
  print(table(abstracts$abstract_text[degenerate]))
  abstracts$abstract_text[degenerate] <- NA_character_
}

before <- abstracts |>
  summarise(across(any_of(c("is_rct", "is_academic", "is_us_based",
                            "is_multicenter", "has_numeric_results")),
                   ~ sum(.x, na.rm = TRUE)))

# --- Identical rules to R/02_clean_abstracts.R, over the current text ---------

# Abstract-level affiliations, produced by scripts/backfill_affiliations_from_cache.R.
# One row per abstract, the union of institution strings in the source record.
AFF_PATH <- here("data", "processed", "abstract_affiliations.csv")
if (file.exists(AFF_PATH)) {
  aff_tbl <- read_csv(AFF_PATH, show_col_types = FALSE) |>
    select(abstract_id, .aff_text = affiliations, .n_aff = n_affiliations) |>
    distinct(abstract_id, .keep_all = TRUE)
} else {
  cli_alert_warning("No abstract_affiliations.csv; affiliation covariates will be NA")
  aff_tbl <- tibble(abstract_id = character(0), .aff_text = character(0),
                    .n_aff = integer(0))
}
abstracts <- abstracts |> left_join(aff_tbl, by = "abstract_id")

abstracts <- abstracts |>
  mutate(
    .search_text = coalesce(abstract_full_text, abstract_text, title, ""),

    is_rct = str_detect(
      tolower(coalesce(abstract_design, .search_text, "")),
      "randomi[sz]ed|rct|random\\s+alloc"
    ),

    sample_size = map_dbl(coalesce(abstract_patients_participants, .search_text),
                          function(text) {
      if (is.na(text) || text == "") return(NA_real_)
      drop_years <- function(nums) nums[nums < 1900 | nums > 2099]
      nums <- str_extract_all(
        text, "(?:n\\s*=\\s*|N\\s*=\\s*|total of\\s+|included\\s+)(\\d[\\d,]*)")[[1]]
      if (length(nums) > 0) {
        num_str <- str_extract(nums[1], "\\d[\\d,]*")
        val <- as.numeric(str_remove_all(num_str, ","))
        if (length(drop_years(val)) > 0) return(val)
      }
      raw_strs <- str_extract_all(text, "\\d+")[[1]]
      raw_strs <- raw_strs[nchar(raw_strs) <= 5]
      all_nums <- as.numeric(raw_strs)
      all_nums <- all_nums[!is.na(all_nums) & all_nums > 1]
      all_nums <- drop_years(all_nums)
      if (length(all_nums) > 0) return(max(all_nums))
      NA_real_
    }),

    # Derived from the author affiliation, never from abstract body text.
    # NA where no affiliation is on file: absence of evidence is not evidence
    # of a non-academic, non-US institution, and coding it FALSE filled the
    # comparison group with unknowns.
    is_academic = if_else(
      !is.na(.aff_text) & nzchar(.aff_text),
      str_detect(tolower(.aff_text), ACADEMIC_AFFILIATION_RE),
      NA
    ),

    is_us_based = if_else(
      !is.na(.aff_text) & nzchar(.aff_text),
      str_detect(tolower(.aff_text), US_AFFILIATION_RE) &
        !str_detect(tolower(.aff_text), NON_US_COUNTRY_RE),
      NA
    ),

    # Distinct institutions named in the record. The previous multicenter flag
    # was derived from abstract text; this counts what the source lists.
    n_affiliations = .n_aff
  )

abstracts <- abstracts |>
  mutate(
    .lc = tolower(coalesce(.search_text, abstract_full_text, "")),
    study_design = case_when(
      is_rct ~ "rct",
      str_detect(.lc, "systematic review|meta-analysis|scoping review|narrative review|umbrella review") ~ "systematic_review",
      str_detect(.lc, "prospective\\s+(cohort|observational|study|trial|longitudinal|database|registry|analysis)") ~ "prospective_cohort",
      str_detect(.lc, paste0(
        "retrospective\\s+(cohort|review|chart|analysis|study|database|case)",
        "|chart review|database (study|analysis|review)|medical record review",
        "|retrospective analysis|reviewed.*charts|reviewed.*records",
        "|retrospective.*review of"
      )) ~ "retrospective_cohort",
      str_detect(.lc, "case-control|case control") ~ "case_control",
      str_detect(.lc, "case (series|report)|single.?case|video (presentation|case|demonstration)") ~ "case_series",
      str_detect(.lc, "cross-sectional|cross sectional|survey|questionnaire") ~ "cross_sectional",
      str_detect(.lc, "quality improvement|qi project|pdsa cycle") ~ "quality_improvement",
      str_detect(.lc, "cost.?(effectiveness|analysis|benefit|utility)|economic (analysis|evaluation)") ~ "cost_analysis",
      str_detect(.lc, "simulation|cadaver|bench.?top|dry lab|wet lab|ex.?vivo|animal model|porcine") ~ "simulation_lab",
      str_detect(.lc, "validation (study|of)|validate[ds]?\\b|psychometric|reliability|accuracy") ~ "validation",
      str_detect(.lc, "\\b(nsqip|acs-nsqip|acsnsqip|hcup|nis|nrd|seer|ncdb|national.*database|nationwide.*database|sart|puf)\\b") ~ "retrospective_cohort",
      str_detect(.lc, "cohort study|cohort analysis|longitudinal study") ~ "prospective_cohort",
      str_detect(.lc, "descriptive study|descriptive analysis") ~ "cross_sectional",
      TRUE ~ "other"
    ),
    is_multicenter = str_detect(.lc,
      "multi-?center|multi-?site|multi-?institutional|\\d+\\s+(center|site|institution|hospital)s?\\b"),
    has_funding = str_detect(.lc,
      "funded by|grant|supported by|\\bnih\\b|\\bnichd\\b|foundation|sponsor|funding"),
    stat_sig_reported = str_detect(.lc,
      "p\\s*[<=]\\s*0\\.|confidence interval|odds ratio|hazard ratio|relative risk|\\bci\\b.*\\d"),

    # Rule change: falls back to abstract_text where the structured section
    # columns are absent, which is every 2012-2018 abstract. See the header.
    has_numeric_results = str_detect(
      tolower(coalesce(abstract_measurements, abstract_conclusion,
                       abstract_text, "")),
      "\\d+\\.\\d|p\\s*[<=]|\\bor\\b\\s|\\bhr\\b\\s|\\brr\\b\\s|\\d+%|\\bci\\b"),

    is_database_study = str_detect(.lc,
      "\\bnsqip\\b|\\bacs-nsqip\\b|\\bhcup\\b|\\bnis\\b|\\bnrd\\b|\\bseer\\b|\\bncdb\\b|\\bsart\\b|\\bpuf\\b|\\bnational inpatient\\b|\\bnationwide.*database\\b|\\bnational.*database\\b|\\bamerican college of surgeons.*quality\\b|\\bpremier\\b|\\bmarketscan\\b|\\btrinetx\\b"),
    has_industry = str_detect(.lc,
      "intuitive surgical|medtronic|ethicon|hologic|cooper surgical|myovant|abbvie|allergan|bayer|merck|pfizer|stryker|karl storz|olympus|applied medical|conceptus|novadaq|lumenis|gynesonics|acessa|sonata|minerva|\\bnovure\\b|\\bcoloplast\\b"),
    has_trial_registration = str_detect(.lc,
      "nct\\d{5,}|isrctn\\d+|clinicaltrials\\.gov|trial regist"),
    has_irb_statement = str_detect(.lc,
      "\\birb\\b|institutional review board|ethics committee|ethical approval|ethically approved|exempt.*review|human subjects"),

    abstract_word_count = vapply(
      strsplit(coalesce(abstract_full_text, abstract_text, ""), "\\s+"),
      function(x) sum(nchar(x) > 0), integer(1)),

    research_category = case_when(
      str_detect(.lc, "\\bcell\\b|molecular|protein|gene\\b|expression|pathway|receptor|histolog|tissue|\\bin vitro\\b|\\bin vivo\\b|biomarker") ~ "basic_science",
      str_detect(.lc, "simulation|training|curriculum|learner|education|teaching|\\bvr\\b|virtual reality|warm.?up|\\bosce\\b") & study_design != "rct" ~ "education",
      str_detect(.lc, "quality improvement|\\bqi\\b|compliance|safety culture|\\beras\\b|enhanced recovery|protocol implement|checklist|bundle") ~ "quality_improvement",
      str_detect(.lc, "cost|utilization|disparit|access|insurance|medicaid|medicare|socioeconomic|equity|racial|ethnic|rural|urban|readmission|length of stay") ~ "health_services",
      str_detect(.lc, "robot|davinci|da vinci|\\bai\\b|artificial intelligence|machine learning|deep learning|computer vision|instrument|device|platform") ~ "device_technology",
      str_detect(.lc, "patient|surgery|procedure|operative|clinical|outcome|complication|surgical") ~ "clinical",
      TRUE ~ "other"
    ),

    primary_procedure = case_when(
      str_detect(.lc, "sacrocolpopex|sacrocervicopex") ~ "sacrocolpopexy",
      str_detect(.lc, "myomectom") ~ "myomectomy",
      str_detect(.lc, "hysterectom") ~ "hysterectomy",
      str_detect(.lc, "endometrios") ~ "endometriosis",
      str_detect(.lc, "oophorectom|salpingo|adnex") ~ "adnexal_surgery",
      str_detect(.lc, "sling|incontinence|prolapse|pelvic organ") ~ "pelvic_floor",
      str_detect(.lc, "steriliz|tubal|essure") ~ "sterilization",
      str_detect(.lc, "ectopic|pregnancy") ~ "ectopic_pregnancy",
      str_detect(.lc, "cerclage|cervical insuff") ~ "cerclage",
      str_detect(.lc, "fibroid|leiomyoma|uterine artery") ~ "fibroids",
      str_detect(.lc, "cancer|malignan|oncolog|staging|sentinel") ~ "gynecologic_oncology",
      TRUE ~ NA_character_
    )
  )

# Result positivity — same fallback widening as has_numeric_results.
abstracts <- abstracts |>
  mutate(
    .positivity_text = coalesce(
      if_else(nchar(coalesce(abstract_conclusion, "")) >= 20, abstract_conclusion, NA_character_),
      if_else(nchar(coalesce(abstract_measurements, "")) >= 20, abstract_measurements, NA_character_),
      if_else(nchar(coalesce(abstract_full_text, "")) >= 30, abstract_full_text, NA_character_),
      if_else(nchar(coalesce(abstract_text, "")) >= 30, abstract_text, NA_character_)
    ),
    result_positivity = vapply(.positivity_text, classify_result_positivity, character(1))
  ) |>
  select(-.search_text, -.lc, -.positivity_text,
         -any_of(c(".aff_text", ".n_aff")))

# --- Report the movement, by congress year -----------------------------------

after <- abstracts |>
  summarise(across(any_of(c("is_rct", "is_academic", "is_us_based",
                            "is_multicenter", "has_numeric_results")),
                   ~ sum(.x, na.rm = TRUE)))

cli_h3("Cohort counts before -> after")
for (v in names(before)) {
  cli_alert_info("{v}: {before[[v]]} -> {after[[v]]}")
}

cli_h3("Coverage by congress year")
print(as.data.frame(
  abstracts |>
    group_by(congress_year) |>
    summarise(
      n = n(),
      rct = round(100 * mean(is_rct), 1),
      us = round(100 * mean(is_us_based), 1),
      acad = round(100 * mean(is_academic), 1),
      numres = round(100 * mean(has_numeric_results), 1),
      samp = round(100 * mean(!is.na(sample_size)), 1),
      other_design = round(100 * mean(study_design == "other"), 1),
      .groups = "drop"
    )
))

stopifnot(nrow(abstracts) == 1106L)
write_csv(abstracts, abstracts_path)
cli_alert_success("Updated {abstracts_path}")
cli_alert_warning("Re-run 05_adjudicate.R onward: these are model covariates.")
