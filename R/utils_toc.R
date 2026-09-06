# utils_toc.R — find where the oral block ends in a congress supplement.
#
# Ten of the twelve congresses are truncated: the ScienceDirect listing caps at
# ~100 items, and for 2012-2021 every captured record carries an "Oral"
# section header, meaning abstraction stopped while still inside the oral block.
# Completing those frames needs one number per congress -- the page at which
# oral presentations end -- and that number is printed in the supplement's own
# table of contents.
#
# The text parsing is separated from the PDF reading so it can be tested on
# character vectors. A parser that can only be exercised by feeding it a 400-page
# PDF is a parser nobody checks.

#' Canonical session label for a table-of-contents heading.
#'
#' The printed headings are not stable across years: 2022 uses "Video Sessions"
#' and 2023 "Video Presentations", and case varies. Only the family matters.
#'
#' @param x Character vector of candidate heading text.
#' @return "Oral", "Video", "Poster" or `NA_character_`.
#' @export
normalize_session_label <- function(x) {
  y <- tolower(trimws(x %||% ""))
  out <- rep(NA_character_, length(y))
  # Order matters: a heading reading "Oral and Video Presentations" is ambiguous
  # and must not be silently resolved, so it is matched by neither branch below.
  amb <- grepl("oral", y) & grepl("video", y)
  out[grepl("oral", y)   & !amb] <- "Oral"
  out[grepl("video", y)  & !amb] <- "Video"
  out[grepl("poster", y) & !amb] <- "Poster"
  out
}

#' Extract the S-page number from a line of table-of-contents text.
#'
#' @param x Character vector.
#' @return Integer vector; `NA` where no S-page appears.
#' @export
toc_page_number <- function(x) {
  # regmatches() returns one element PER MATCH, not per input, so the hits have
  # to be placed back by position rather than assumed parallel to x.
  m <- regexpr("\\bS[[:space:]]?[0-9]+", x, perl = TRUE, ignore.case = TRUE)
  n <- rep(NA_integer_, length(x))
  hit <- m > 0
  if (any(hit)) n[hit] <- as.integer(gsub("[^0-9]", "", regmatches(x, m)))
  n
}

#' Locate session-block boundaries in supplement table-of-contents text.
#'
#' Walks the lines in order. A line that is a session heading opens a block; the
#' first S-page seen after it starts that block, and the last S-page before the
#' next heading ends it.
#'
#' @param lines Character vector, the table of contents in printed order.
#' @param min_items Integer. A block yielding fewer entries than this is
#'   reported but flagged, because a stray heading in running text can otherwise
#'   masquerade as a session.
#' @return A data frame with `section`, `heading`, `first_page`, `last_page`,
#'   `n_entries`. Empty (0 rows) when no heading is recognised -- callers must
#'   treat that as failure, not as "no sections".
#' @examples
#' toc <- c("Oral Presentations", "1. A study ... S1", "2. Another ... S12",
#'          "Video Sessions", "91. A video ... S37")
#' find_session_boundaries(toc)
#' @export
find_session_boundaries <- function(lines, min_items = 3L) {
  lines <- as.character(lines)
  lab <- normalize_session_label(lines)
  pg  <- toc_page_number(lines)

  # A heading is a short line that names a session family and carries no page
  # reference of its own; "see Oral Presentations, page S4" is not a heading.
  is_head <- !is.na(lab) & is.na(pg) & nchar(trimws(lines)) <= 60

  if (!any(is_head)) {
    return(data.frame(section = character(0), heading = character(0),
                      first_page = integer(0), last_page = integer(0),
                      n_entries = integer(0), stringsAsFactors = FALSE))
  }

  idx <- which(is_head)
  ends <- c(idx[-1] - 1L, length(lines))
  out <- lapply(seq_along(idx), function(k) {
    span <- seq.int(idx[k], ends[k])
    p <- pg[span]; p <- p[!is.na(p)]
    data.frame(section = lab[idx[k]], heading = trimws(lines[idx[k]]),
               first_page = if (length(p)) min(p) else NA_integer_,
               last_page  = if (length(p)) max(p) else NA_integer_,
               n_entries  = length(p), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)

  # Merge repeated headings of the same family: supplements often break the
  # oral block across several "Open Communications" headings under one session.
  agg <- lapply(split(out, out$section), function(g) data.frame(
    section = g$section[1], heading = paste(unique(g$heading), collapse = " | "),
    first_page = suppressWarnings(min(g$first_page, na.rm = TRUE)),
    last_page  = suppressWarnings(max(g$last_page,  na.rm = TRUE)),
    n_entries  = sum(g$n_entries), stringsAsFactors = FALSE))
  agg <- do.call(rbind, agg)
  agg <- agg[order(agg$first_page), , drop = FALSE]
  agg$sparse <- agg$n_entries < min_items
  rownames(agg) <- NULL
  agg
}

`%||%` <- function(a, b) if (is.null(a)) b else a
