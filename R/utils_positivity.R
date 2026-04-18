# utils_positivity.R — Classify abstract result direction from conclusion text.
# Uses regex-based NLP to categorize as positive/negative/neutral/unclear.
# Based on patterns validated in publication-bias literature (Cochrane MR000005).

library(stringr)

#' @title Classify Result Direction from Abstract Conclusion Text
#'
#' @description
#' Classifies the directional result of an abstract as positive, negative,
#' neutral, or unclear using regex-based NLP pattern matching applied to
#' conclusion or results text. Supports publication-bias analysis aligned with
#' Cochrane Methodology Review MR000005.
#'
#' @param text Character scalar. The conclusion section or last paragraph of
#'   an abstract. Short or missing text (\code{NA} or fewer than 15
#'   non-whitespace characters) returns \code{"unclear"}.
#'
#' @return Character scalar. One of \code{"positive"}, \code{"negative"},
#'   \code{"neutral"}, or \code{"unclear"}.
#'
#' @details
#' Three pattern sets are evaluated independently:
#' \describe{
#'   \item{Negative patterns}{Terms such as "no significant", "did not differ",
#'     "failed to", p-values >= 0.05 (regex-based).}
#'   \item{Positive patterns}{Terms such as "improved", "reduced", "effective",
#'     "superior", statistically significant p < 0.05 (excluding negative-harm
#'     phrasing).}
#'   \item{Neutral patterns}{Terms such as "feasible", "further study needed",
#'     "preliminary", "trend toward".}
#' }
#' Decision rule: the category with the most pattern hits wins. Ties between
#' positive and negative are resolved as \code{"neutral"}. A text with no
#' pattern matches is \code{"unclear"}.
#'
#' @examples
#' \dontrun{
#' classify_result_positivity(
#'   "Laparoscopic hysterectomy significantly reduced blood loss (p < 0.01)."
#' )
#' # "positive"
#'
#' classify_result_positivity(
#'   "There was no significant difference between groups (p = 0.43)."
#' )
#' # "negative"
#'
#' classify_result_positivity(
#'   "This pilot study suggests trends toward improved outcomes."
#' )
#' # "neutral"
#' }
#'
#' @seealso \code{\link{classify_study_design}}, \code{\link{classify_research_category}}
#' @export
classify_result_positivity <- function(text) {
  if (is.na(text) || nchar(str_squish(text)) < 15) return("unclear")
  lc <- tolower(text)

  neg_patterns <- c(
    "\\bnot? significant",
    "\\bno significant",
    "\\bno difference",
    "\\bno association",
    "\\bnot associated",
    "\\bno improvement",
    "\\bno benefit",
    "\\bno effect",
    "\\bdid not differ",
    "\\bdid not improve",
    "\\bdid not reduce",
    "\\bfailed to",
    "\\binsufficient evidence",
    "\\bp\\s*[>=]\\s*0\\.[1-9]",
    "\\bp\\s*=\\s*0\\.0[5-9]",
    "\\bcomparable between",
    "\\bsimilar between groups",
    "\\bno statistically"
  )

  pos_patterns <- c(
    "(?<!not? )\\bsignificant(ly)?\\b(?! (higher|lower|more|less|greater|fewer) (risk|complication|adverse))",
    "\\bimproved\\b",
    "\\breduced\\b",
    "\\bdecreased\\b",
    "\\beffective\\b",
    "\\bsuperior\\b",
    "\\bbeneficial\\b",
    "\\bbetter outcomes?\\b",
    "\\bhigher rate of (success|improvement|resolution)",
    "\\blower rate of (complication|failure|recurrence|adverse)",
    "\\bassociated with (improved|better|reduced|decreased|lower)",
    "\\bp\\s*<\\s*0\\.0[0-5]",
    "\\bp\\s*=\\s*0\\.0[0-4]",
    "\\bp\\s*<\\s*0\\.001",
    "\\bstatistically significant\\b"
  )

  neutral_patterns <- c(
    "\\bfeasible\\b",
    "\\bsafe\\b(?!.{0,20}(effective|superior|improved))",
    "\\bfurther (study|research|investigation)\\b",
    "\\bmore research\\b",
    "\\bpreliminary\\b",
    "\\bpilot\\b",
    "\\bdescriptive\\b",
    "\\btrends? toward\\b",
    "\\bnumerically (higher|lower)\\b"
  )

  n_neg <- sum(vapply(neg_patterns, function(p) str_detect(lc, regex(p)), logical(1)))
  n_pos <- sum(vapply(pos_patterns, function(p) str_detect(lc, regex(p)), logical(1)))
  n_neu <- sum(vapply(neutral_patterns, function(p) str_detect(lc, regex(p)), logical(1)))

  if (n_neg > n_pos && n_neg > n_neu) return("negative")
  if (n_pos > n_neg && n_pos > n_neu) return("positive")
  if (n_neu > 0 && n_neg == 0 && n_pos == 0) return("neutral")
  if (n_pos == n_neg && n_pos > 0) return("neutral")
  "unclear"
}
