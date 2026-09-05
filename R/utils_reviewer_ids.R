# utils_reviewer_ids.R — stable pseudonyms for study staff.
#
# The adjudication log records who made each decision. Those are study staff,
# not published authors, and their initials do not belong in a public
# repository. Published AUTHOR names in the abstract and PubMed data are the
# public scientific record and are deliberately NOT touched here: removing them
# would destroy the dataset.
#
# The mapping is deterministic and structure-preserving. Every analysis that
# groups by reviewer (interrater agreement, per-reviewer counts, the human/AUTO
# precedence rule) behaves identically before and after, because only the label
# changes. "AUTO" is NOT pseudonymised: R/06_analyze_results.R,
# R/10_interrater.R and the Shiny app all branch on
# that literal string.

#' Pseudonymise a study-staff reviewer identifier
#'
#' @param reviewer Character vector of reviewer identifiers.
#' @return Character vector with human identifiers replaced by stable
#'   pseudonyms and the algorithmic marker preserved.
#' @details Unrecognised human identifiers are hashed to a stable `R##` rather
#'   than passed through, so a new reviewer joining the study cannot leak an
#'   identity by simply not being on the list.
#' @export
pseudonymise_reviewer <- function(reviewer) {
  out <- as.character(reviewer)
  keep <- is.na(out) | out %in% c("AUTO", "")
  known <- c(GW = "R01", JM = "R02", TMM = "R03")
  hit <- !keep & out %in% names(known)
  out[hit] <- unname(known[out[hit]])
  # Anything else that is not already a pseudonym gets a deterministic one.
  stray <- !keep & !hit & !grepl("^R[0-9]{2}$", out)
  if (any(stray)) {
    n <- vapply(out[stray], function(s) {
      as.integer(sum(utf8ToInt(s)) %% 89L) + 4L  # 04..92, disjoint from the assigned block
    }, integer(1))
    out[stray] <- sprintf("R%02d", n)
  }
  out
}

#' Identifiers that must never appear in a tracked file
#' @export
reviewer_identity_patterns <- function() {
  c("\\bGW\\b", "\\bJM\\b", "\\bTMM\\b",
    "Whitmore", "McQuaid", "Batlle", "Rosenberg", "Mandell", "Fertel", "Archer")
}
