# utils_classify.R — Text-based classifiers for abstract-level variables.
# Extracted from 02_clean_abstracts.R inline case_when blocks for testability.

library(stringr)

#' Classify study design from abstract text.
#' @param text Lowercase abstract text (search_text or abstract_full_text).
#' @param is_rct Logical from RCT detection.
#' @return One of 12 study design categories.
classify_study_design <- function(text, is_rct = FALSE) {
  if (is_rct) return("rct")
  if (is.na(text) || nchar(text) < 10) return("other")
  lc <- tolower(text)
  if (str_detect(lc, "systematic review|meta-analysis|scoping review|narrative review|umbrella review")) return("systematic_review")
  if (str_detect(lc, "prospective\\s+(cohort|observational|study|trial|longitudinal|database|registry|analysis)")) return("prospective_cohort")
  if (str_detect(lc, paste0(
    "retrospective\\s+(cohort|review|chart|analysis|study|database|case)",
    "|chart review|database (study|analysis|review)|medical record review",
    "|retrospective analysis|reviewed.*charts|reviewed.*records",
    "|retrospective.*review of"
  ))) return("retrospective_cohort")
  if (str_detect(lc, "case-control|case control")) return("case_control")
  if (str_detect(lc, "case (series|report)|single.?case|video (presentation|case|demonstration)")) return("case_series")
  if (str_detect(lc, "cross-sectional|cross sectional|survey|questionnaire")) return("cross_sectional")
  if (str_detect(lc, "quality improvement|qi project|pdsa cycle")) return("quality_improvement")
  if (str_detect(lc, "cost.?(effectiveness|analysis|benefit|utility)|economic (analysis|evaluation)")) return("cost_analysis")
  if (str_detect(lc, "simulation|cadaver|bench.?top|dry lab|wet lab|ex.?vivo|animal model|porcine")) return("simulation_lab")
  if (str_detect(lc, "validation (study|of)|validate[ds]?\\b|psychometric|reliability|accuracy")) return("validation")
  if (str_detect(lc, "\\b(nsqip|acs-nsqip|hcup|nis|nrd|seer|ncdb|national.*database|nationwide.*database|sart|puf)\\b")) return("retrospective_cohort")
  if (str_detect(lc, "cohort study|cohort analysis|longitudinal study")) return("prospective_cohort")
  if (str_detect(lc, "descriptive study|descriptive analysis")) return("cross_sectional")
  "other"
}

#' Classify research category from abstract text.
#' @param text Lowercase abstract text.
#' @param study_design Output of classify_study_design (for exclusion).
#' @return One of: basic_science, education, quality_improvement,
#'   health_services, device_technology, clinical, other.
classify_research_category <- function(text, study_design = "other") {
  if (is.na(text) || nchar(text) < 10) return("other")
  lc <- tolower(text)
  if (str_detect(lc, "\\bcell\\b|molecular|protein|gene\\b|expression|pathway|receptor|histolog|tissue|\\bin vitro\\b|\\bin vivo\\b|biomarker")) return("basic_science")
  if (str_detect(lc, "simulation|training|curriculum|learner|education|teaching|\\bvr\\b|virtual reality|warm.?up|\\bosce\\b") && study_design != "rct") return("education")
  if (str_detect(lc, "quality improvement|\\bqi\\b|compliance|safety culture|\\beras\\b|enhanced recovery|protocol implement|checklist|bundle")) return("quality_improvement")
  if (str_detect(lc, "cost|utilization|disparit|access|insurance|medicaid|medicare|socioeconomic|equity|racial|ethnic|rural|urban|readmission|length of stay")) return("health_services")
  if (str_detect(lc, "robot|davinci|da vinci|\\bai\\b|artificial intelligence|machine learning|deep learning|computer vision|instrument|device|platform")) return("device_technology")
  if (str_detect(lc, "patient|surgery|procedure|operative|clinical|outcome|complication|surgical")) return("clinical")
  "other"
}

#' Classify primary surgical procedure from abstract text.
#' @param text Lowercase abstract text.
#' @return Procedure name or NA.
classify_primary_procedure <- function(text) {
  if (is.na(text) || nchar(text) < 10) return(NA_character_)
  lc <- tolower(text)
  if (str_detect(lc, "sacrocolpopex|sacrocervicopex")) return("sacrocolpopexy")
  if (str_detect(lc, "myomectom")) return("myomectomy")
  if (str_detect(lc, "hysterectom")) return("hysterectomy")
  if (str_detect(lc, "endometrios")) return("endometriosis")
  if (str_detect(lc, "oophorectom|salpingo|adnex")) return("adnexal_surgery")
  if (str_detect(lc, "sling|incontinence|prolapse|pelvic organ")) return("pelvic_floor")
  if (str_detect(lc, "steriliz|tubal|essure")) return("sterilization")
  if (str_detect(lc, "ectopic|pregnancy")) return("ectopic_pregnancy")
  if (str_detect(lc, "cerclage|cervical insuff")) return("cerclage")
  if (str_detect(lc, "fibroid|leiomyoma|uterine artery")) return("fibroids")
  if (str_detect(lc, "cancer|malignan|oncolog|staging|sentinel")) return("gynecologic_oncology")
  NA_character_
}
