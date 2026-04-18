# utils_pub_types.R — PubMed publication-type normalization.

#' @title Collapse PubMed Publication-Type Tags to a Canonical Label
#'
#' @description
#' Takes a semicolon-delimited string of PubMed PublicationType tags (as
#' stored in \code{pub_types} after parsing PubMed XML) and collapses it to
#' a single canonical publication-type label using a priority hierarchy.
#'
#' @param types_str Character scalar. Semicolon-separated PubMed publication
#'   type tags, e.g., \code{"Journal Article; Randomized Controlled Trial"}.
#'   Returns \code{NA_character_} if \code{NA} or empty.
#'
#' @return Character scalar. One of \code{"Review"}, \code{"RCT/Trial"},
#'   \code{"Case Report"}, \code{"Editorial/Letter"},
#'   \code{"Observational Study"}, or \code{"Journal Article"} (catch-all).
#'
#' @details
#' Priority order (highest to lowest):
#' \enumerate{
#'   \item Review / Meta-analysis / Systematic review / Scoping review /
#'         Umbrella review / Narrative review.
#'   \item Randomized Controlled Trial / Clinical Trial / Equivalence Trial /
#'         Pragmatic Trial / Crossover Trial.
#'   \item Case Reports (exact match on lowercased token).
#'   \item Editorial / Letter / Comment / News (exact match).
#'   \item Observational Study (exact match).
#'   \item Journal Article (default catch-all).
#' }
#' Matching is case-insensitive and applied to individual semicolon-split tokens.
#'
#' @examples
#' \dontrun{
#' canonical_pub_type("Journal Article; Randomized Controlled Trial")
#' # "RCT/Trial"
#'
#' canonical_pub_type("Journal Article; Meta-Analysis; Systematic Review")
#' # "Review"
#'
#' canonical_pub_type(NA_character_)
#' # NA_character_
#' }
#'
#' @seealso \code{\link{parse_pubmed_xml}}, \code{\link{classify_study_design}}
#' @export
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
