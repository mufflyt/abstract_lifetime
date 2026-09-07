# utils_acquisition.R — is this file actually the supplement it claims to be?
#
# The boundary recovery turns twelve PDFs into twelve numbers that nobody can
# eyeball. That makes the identity of the inputs part of the evidence: a
# boundary is only as trustworthy as the certainty that it came from the 2015
# supplement rather than from a login page saved with a .pdf extension.
#
# These checks are deliberately separate from parsing. A file can be perfectly
# parseable and still be the wrong file, and that failure is silent.

#' SHA-256 of a file.
#' @param path Character. File path.
#' @return Character hash, or `NA_character_` if unreadable.
#' @export
file_sha256 <- function(path) {
  if (!length(path) || !file.exists(path)) return(NA_character_)
  tryCatch(as.character(digest::digest(file = path, algo = "sha256")),
           error = function(e) NA_character_)
}

#' Does this file begin with a PDF magic number?
#'
#' A publisher paywall or login redirect saved by a browser is HTML with a .pdf
#' name. It has a plausible size and a plausible filename and parses to nothing.
#'
#' @param path Character. File path.
#' @return `TRUE` if the first bytes are `%PDF-`.
#' @export
is_real_pdf <- function(path) {
  if (!length(path) || !file.exists(path)) return(FALSE)
  con <- file(path, "rb"); on.exit(close(con), add = TRUE)
  identical(rawToChar(readBin(con, "raw", 5L)), "%PDF-")
}

#' Congress year implied by a filename.
#' @param path Character vector of paths.
#' @return Integer vector; `NA` where no 19xx/20xx year appears.
#' @export
year_from_filename <- function(path) {
  b <- basename(path)
  m <- regexpr("(19|20)[0-9]{2}", b)
  y <- rep(NA_integer_, length(b))
  hit <- m > 0
  if (any(hit)) y[hit] <- as.integer(regmatches(b, m))
  y
}

#' Check a set of raw supplement files for identity problems.
#'
#' Reports every problem rather than stopping at the first, because the useful
#' output is "these four files are wrong", not "something is wrong".
#'
#' @param paths Character vector of candidate PDF paths.
#' @param expected_years Integer vector of congress years that must be present.
#' @return A data frame with one row per problem: `severity`, `year`, `file`,
#'   `problem`. Zero rows means the set is usable.
#' @export
check_acquisition <- function(paths, expected_years = 2012:2023) {
  p <- function(sev, yr, f, msg) data.frame(severity = sev, year = yr, file = f,
                                            problem = msg, stringsAsFactors = FALSE)
  out <- list()

  if (!length(paths)) {
    return(p("blocker", NA_integer_, NA_character_,
             "no candidate files supplied"))
  }

  yr  <- year_from_filename(paths)
  sz  <- file.info(paths)$size
  sha <- vapply(paths, file_sha256, character(1), USE.NAMES = FALSE)

  for (i in seq_along(paths)) {
    f <- basename(paths[i])
    if (is.na(yr[i]))
      out[[length(out) + 1L]] <- p("blocker", NA_integer_, f,
        "no four-digit year in the filename, so the file cannot be assigned to a congress")
    if (isTRUE(sz[i] == 0))
      out[[length(out) + 1L]] <- p("blocker", yr[i], f, "zero bytes")
    else if (!is_real_pdf(paths[i]))
      out[[length(out) + 1L]] <- p("blocker", yr[i], f,
        "does not begin with %PDF-; a saved login or paywall page is the usual cause")
    if (is.na(sha[i]))
      out[[length(out) + 1L]] <- p("blocker", yr[i], f, "unreadable, could not be hashed")
  }

  # The same download saved twice under two years is the failure that would
  # otherwise produce two identical boundaries and look like agreement.
  dup <- sha[!is.na(sha)]
  for (h in unique(dup[duplicated(dup)])) {
    k <- which(sha == h)
    out[[length(out) + 1L]] <- p("blocker", NA_integer_,
      paste(basename(paths[k]), collapse = ", "),
      "identical file bytes assigned to more than one congress year")
  }

  seen <- sort(unique(yr[!is.na(yr)]))
  missing <- setdiff(expected_years, seen)
  if (length(missing))
    out[[length(out) + 1L]] <- p("blocker", NA_integer_, NA_character_,
      paste("no file for congress year(s):", paste(missing, collapse = ", ")))

  # The controls are what make every other year believable.
  for (ctrl in intersect(c(2022L, 2023L), expected_years))
    if (!ctrl %in% seen)
      out[[length(out) + 1L]] <- p("blocker", ctrl, NA_character_,
        "validation control missing; without it the parser cannot be checked against a known boundary")

  extra <- setdiff(seen, expected_years)
  if (length(extra))
    out[[length(out) + 1L]] <- p("warning", NA_integer_, NA_character_,
      paste("file(s) for unexpected year(s):", paste(extra, collapse = ", ")))

  if (!length(out))
    return(data.frame(severity = character(0), year = integer(0),
                      file = character(0), problem = character(0),
                      stringsAsFactors = FALSE))
  do.call(rbind, out)
}
