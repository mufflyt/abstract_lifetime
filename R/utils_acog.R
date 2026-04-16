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

#' Return the ACOG district (Roman numeral) for a 2-letter state/territory code.
#' Returns "AFS" for Armed Forces Section codes AA/AE/AP, NA otherwise.
acog_district_for_state <- function(state) {
  if (is.na(state) || nchar(state) == 0) return(NA_character_)
  s <- toupper(state)
  if (s %in% c("AA", "AE", "AP")) return("AFS")
  d <- unname(ACOG_DISTRICT[s])
  if (is.null(d) || is.na(d)) NA_character_ else d
}
