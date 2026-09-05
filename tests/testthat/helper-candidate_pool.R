# Shared resolver for the candidate pool.
#
# data/processed/pubmed_candidates.csv is 130 MB and gitignored, so every
# assertion about the pool skipped in CI, including the F2 invariant that every
# winning PMID resolves in it. output/candidate_pool_index.csv carries the two
# columns those assertions read, at 1.4 MB.
#
# Prefer the full pool when it is present: on a machine that has run the
# pipeline, the assertions should be made against the real artefact rather than
# a derived one. Fall back to the index otherwise, which is what CI does.
#
# Returns a tibble with abstract_id and character pmid, or NULL when neither is
# available, so callers can skip with an accurate reason.
#' @param typed "character" coerces pmid to character, which is what membership
#'   and anti_join comparisons want and what stops a 8-digit PMID being read
#'   back in scientific notation. "infer" leaves readr's inference alone, which
#'   is what app.R:203 does; the join-fidelity test needs that, because forcing
#'   a type in the test would manufacture the very mismatch it exists to detect.
candidate_pool <- function(typed = c("character", "infer")) {
  typed <- match.arg(typed)
  read_one <- function(path, cols) {
    if (identical(typed, "infer")) {
      readr::read_csv(path, show_col_types = FALSE, col_select = dplyr::all_of(cols))
    } else {
      d <- readr::read_csv(path, show_col_types = FALSE,
                           col_select = dplyr::all_of(cols))
      dplyr::mutate(d, pmid = as.character(pmid))
    }
  }
  cols <- c("abstract_id", "pmid")

  full <- here::here("data", "processed", "pubmed_candidates.csv")
  if (file.exists(full)) return(read_one(full, cols))

  idx <- here::here("output", "candidate_pool_index.csv")
  if (file.exists(idx)) return(read_one(idx, cols))

  NULL
}

candidate_pool_source <- function() {
  if (file.exists(here::here("data", "processed", "pubmed_candidates.csv"))) {
    "data/processed/pubmed_candidates.csv"
  } else if (file.exists(here::here("output", "candidate_pool_index.csv"))) {
    "output/candidate_pool_index.csv"
  } else {
    NA_character_
  }
}
