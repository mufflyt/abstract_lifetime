# utils_positivity.R — Classify abstract result direction from conclusion text.
# Uses regex-based NLP to categorize as positive/negative/neutral/unclear.
# Based on patterns validated in publication-bias literature (Cochrane MR000005).

library(stringr)

#' Classify result positivity from abstract conclusion or results text.
#' @param text Character string — the abstract's conclusion or last paragraph.
#' @return One of "positive", "negative", "neutral", "unclear"
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
