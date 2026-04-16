# utils_pub_types.R — PubMed publication-type normalization.

#' Collapse a semi-colon list of PublicationType tags into one canonical label
#' Priority: Review > RCT/Trial > Case Report > Editorial/Letter > Journal Article
canonical_pub_type <- function(types_str) {
  if (is.na(types_str) || nchar(types_str) == 0) return(NA_character_)
  toks <- stringr::str_trim(stringr::str_split(types_str, ";\\s*")[[1]])
  lc <- tolower(toks)
  if (any(stringr::str_detect(lc, "meta-analysis|systematic review|scoping review|^review$|narrative review|umbrella review")))
    return("Review")
  if (any(stringr::str_detect(lc, "randomized controlled trial|clinical trial|equivalence trial|pragmatic trial|crossover trial")))
    return("RCT/Trial")
  if (any(lc == "case reports")) return("Case Report")
  if (any(stringr::str_detect(lc, "^editorial$|^letter$|^comment$|^news$")))
    return("Editorial/Letter")
  if (any(lc == "observational study")) return("Observational Study")
  "Journal Article"
}
