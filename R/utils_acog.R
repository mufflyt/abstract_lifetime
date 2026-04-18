# utils_acog.R — US state / territory → ACOG District mapping.
# Source: https://www.acog.org/community/districts
# Districts I-XI cover states + territories + several Canadian provinces;
# Armed Forces Section (AFS) is treated separately.

ACOG_DISTRICT <- c(
  # District I — New England (+ Atlantic Canada)
  CT = "I", ME = "I", MA = "I", NH = "I", RI = "I", VT = "I",
  NB = "I", NL = "I", NS = "I", PE = "I",
  # District II
  NY = "II", ON = "II", QC = "II",
  # District III — Mid-Atlantic
  DE = "III", NJ = "III", PA = "III",
  # District IV
  DC = "IV", MD = "IV", VA = "IV", WV = "IV",
  # District V — Midwest
  IN = "V", KY = "V", MI = "V", OH = "V",
  # District VI — Upper Midwest (+ Prairie Canada)
  IA = "VI", MN = "VI", ND = "VI", SD = "VI", WI = "VI",
  MB = "VI", SK = "VI",
  # District VII — Central
  IL = "VII", KS = "VII", MO = "VII", NE = "VII",
  # District VIII — Mountain West / Pacific NW (+ Western Canada)
  AK = "VIII", AZ = "VIII", CO = "VIII", HI = "VIII", ID = "VIII",
  MT = "VIII", NV = "VIII", NM = "VIII", OR = "VIII", UT = "VIII",
  WA = "VIII", WY = "VIII",
  AB = "VIII", BC = "VIII",
  # District IX — California
  CA = "IX",
  # District X — South Central
  AR = "X", LA = "X", MS = "X", OK = "X", TN = "X", TX = "X",
  # District XI — Southeast (+ PR / USVI)
  AL = "XI", FL = "XI", GA = "XI", NC = "XI", SC = "XI",
  PR = "XI", VI = "XI"
)

#' @title Return the ACOG District for a State or Territory Code
#'
#' @description
#' Maps a 2-letter US state or territory abbreviation to its corresponding
#' ACOG district Roman numeral. Handles all 50 states, DC, Puerto Rico, USVI,
#' select Canadian provinces, and the Armed Forces Section (AFS).
#'
#' @param state Character scalar. A 2-letter state or territory abbreviation
#'   (case-insensitive). Use "AA", "AE", or "AP" for Armed Forces Section codes.
#'
#' @return Character scalar. A Roman numeral string ("I" through "XI") for
#'   recognized US/Canadian codes, "AFS" for Armed Forces Section codes,
#'   or \code{NA_character_} if the code is unrecognized or missing.
#'
#' @details
#' Districts I–XI follow the official ACOG district boundaries as published at
#' \url{https://www.acog.org/community/districts}. Several Canadian provinces
#' are included because ACOG grants membership and district affiliation to
#' Canadian obstetricians. The lookup table \code{ACOG_DISTRICT} is a named
#' character vector defined at the top of this file.
#'
#' @examples
#' \dontrun{
#' acog_district_for_state("TX")   # "X"
#' acog_district_for_state("NY")   # "II"
#' acog_district_for_state("AE")   # "AFS"
#' acog_district_for_state("ZZ")   # NA_character_
#' }
#'
#' @seealso \code{\link{parse_us_state}}, \code{ACOG_DISTRICT}
#' @export
acog_district_for_state <- function(state) {
  if (is.na(state) || nchar(state) == 0) return(NA_character_)
  s <- toupper(state)
  if (s %in% c("AA", "AE", "AP")) return("AFS")
  d <- unname(ACOG_DISTRICT[s])
  if (is.null(d) || is.na(d)) NA_character_ else d
}
