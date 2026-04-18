# utils_classify.R — Text-based classifiers for abstract-level variables.
# Extracted from 02_clean_abstracts.R inline case_when blocks for testability.

library(stringr)

#' @title Classify Study Design from Abstract Text
#'
#' @description
#' Assigns a study-design category to an abstract using ordered regex rules
#' applied to the full abstract text. Returns a single canonical label that is
#' used downstream for stratified publication-rate analysis.
#'
#' @param text Character scalar. Full abstract text (or the pre-processed
#'   \code{search_text} field). Case will be lowered internally; the caller
#'   may pass raw text.
#' @param is_rct Logical scalar. Pre-computed indicator that the abstract
#'   describes a randomized controlled trial. If \code{TRUE} the function
#'   short-circuits and returns \code{"rct"} without further regex evaluation.
#'   Defaults to \code{FALSE}.
#'
#' @return Character scalar. One of \code{"rct"}, \code{"systematic_review"},
#'   \code{"prospective_cohort"}, \code{"retrospective_cohort"},
#'   \code{"case_control"}, \code{"case_series"}, \code{"cross_sectional"},
#'   \code{"quality_improvement"}, \code{"cost_analysis"},
#'   \code{"simulation_lab"}, \code{"validation"}, or \code{"other"}.
#'
#' @details
#' Rules are evaluated in priority order: systematic reviews and meta-analyses
#' first, then trial types, then observational designs. National database
#' keywords (NSQIP, NIS, SEER, SART, etc.) map to
#' \code{"retrospective_cohort"}. Returns \code{"other"} when no pattern
#' matches or when text is missing or shorter than 10 characters.
#'
#' @examples
#' \dontrun{
#' classify_study_design("We performed a retrospective chart review of 200 patients.")
#' # "retrospective_cohort"
#'
#' classify_study_design("This is an NSQIP database analysis.")
#' # "retrospective_cohort"
#'
#' classify_study_design("A systematic review and meta-analysis was conducted.")
#' # "systematic_review"
#' }
#'
#' @seealso \code{\link{classify_research_category}}, \code{\link{classify_primary_procedure}}
#' @export
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

#' @title Classify Research Category from Abstract Text
#'
#' @description
#' Assigns a broad research-domain category to an abstract using keyword
#' detection applied to the full text. Complements \code{classify_study_design}
#' by capturing thematic content independent of methodological design.
#'
#' @param text Character scalar. Full abstract text (raw or pre-processed).
#'   Case is lowered internally.
#' @param study_design Character scalar. Output of \code{classify_study_design()}
#'   for the same abstract. Used to avoid misclassifying simulation RCTs as
#'   \code{"education"} — when \code{study_design == "rct"} the education
#'   branch is skipped. Defaults to \code{"other"}.
#'
#' @return Character scalar. One of \code{"basic_science"}, \code{"education"},
#'   \code{"quality_improvement"}, \code{"health_services"},
#'   \code{"device_technology"}, \code{"clinical"}, or \code{"other"}.
#'
#' @details
#' Rules are evaluated in priority order: basic-science molecular keywords
#' first, then education/simulation, then QI, then health-services/disparities,
#' then device/AI, then general clinical. Returns \code{"other"} for missing or
#' very short text.
#'
#' @examples
#' \dontrun{
#' classify_research_category("We assessed disparities in Medicaid coverage for hysterectomy.")
#' # "health_services"
#'
#' classify_research_category("Robot-assisted surgery using da Vinci platform.")
#' # "device_technology"
#'
#' classify_research_category("A PDSA cycle was implemented to improve OR checklists.")
#' # "quality_improvement"
#' }
#'
#' @seealso \code{\link{classify_study_design}}, \code{\link{classify_primary_procedure}}
#' @export
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

#' @title Classify the Primary Surgical Procedure from Abstract Text
#'
#' @description
#' Identifies the dominant gynecologic surgical procedure described in an
#' abstract using ordered keyword matching. Returns a single canonical
#' procedure label used for stratified publication-rate analysis.
#'
#' @param text Character scalar. Full abstract text (raw or pre-processed).
#'   Case is lowered internally.
#'
#' @return Character scalar. One of \code{"sacrocolpopexy"},
#'   \code{"myomectomy"}, \code{"hysterectomy"}, \code{"endometriosis"},
#'   \code{"adnexal_surgery"}, \code{"pelvic_floor"}, \code{"sterilization"},
#'   \code{"ectopic_pregnancy"}, \code{"cerclage"}, \code{"fibroids"},
#'   \code{"gynecologic_oncology"}, or \code{NA_character_} if no procedure
#'   keyword is detected.
#'
#' @details
#' Matching is ordered from most-specific to broadest: sacrocolpopexy and
#' myomectomy before hysterectomy (all hysterectomies match "hysterectom" but
#' only some are for fibroids or prolapse). \code{NA_character_} is returned
#' for non-procedural abstracts (e.g., pure health-services or education
#' papers) rather than a fallback category.
#'
#' @examples
#' \dontrun{
#' classify_primary_procedure("Laparoscopic sacrocolpopexy for pelvic organ prolapse.")
#' # "sacrocolpopexy"
#'
#' classify_primary_procedure("Outcomes following total laparoscopic hysterectomy.")
#' # "hysterectomy"
#'
#' classify_primary_procedure("Patient satisfaction survey.")
#' # NA_character_
#' }
#'
#' @seealso \code{\link{classify_study_design}}, \code{\link{classify_research_category}}
#' @export
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
