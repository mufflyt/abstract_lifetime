# utils_affiliation.R — Extract demographics from PubMed affiliation text.
# Cross-references against 2754 ACGME-accredited teaching hospitals from the
# isochrones project's validated hospital classifier.

library(stringr)

# Load teaching hospital names for cross-reference (generated from isochrones
# teaching_hospitals.rds + ACGME residency/fellowship program data).
.teaching_names_path <- file.path(here::here(), "data", "validation", "teaching_hospital_names.txt")
TEACHING_HOSPITAL_NAMES <- if (file.exists(.teaching_names_path)) {
  nms <- readLines(.teaching_names_path)
  # Filter out generic names too short to be meaningful (e.g., "hospital")
  nms[nchar(nms) >= 10]
} else character()

# Precompile a single alternation regex at load time so is_teaching_hospital()
# makes one grepl() call instead of 2,754 per affiliation.

#' @title Normalize an Affiliation String for Fixed-String Matching
#' @description Lowercases, strips non-alphanumeric characters, and collapses
#'   whitespace so that affiliation strings and hospital names can be compared
#'   with fixed (non-regex) string search. Used internally by
#'   \code{is_teaching_hospital()}.
#' @param x Character scalar or vector. Raw affiliation text.
#' @return Character scalar or vector. Normalized string(s).
#' @keywords internal
.normalize_aff <- function(x) gsub("\\s+", " ", trimws(gsub("[^a-z0-9 ]", " ", tolower(x))))

# Normalize hospital names at load time; stored as a plain character vector.
.TEACHING_NAMES_NORM <- if (length(TEACHING_HOSPITAL_NAMES) > 0) {
  norms <- unique(.normalize_aff(TEACHING_HOSPITAL_NAMES))
  norms[nchar(norms) > 3]
} else character()

#' @title Check Whether an Affiliation Matches a Known Teaching Hospital
#'
#' @description
#' Returns \code{TRUE} if the affiliation string contains the name of one of
#' 2,754 ACGME-accredited teaching hospitals loaded from
#' \code{data/validation/teaching_hospital_names.txt}. Uses fixed-string
#' vectorized matching to avoid regex alternation length limits.
#'
#' @param aff Character scalar. Raw affiliation text from a PubMed author record.
#'
#' @return Logical scalar. \code{TRUE} if the affiliation matches at least one
#'   known teaching hospital name; \code{FALSE} otherwise (including when the
#'   reference list has not been loaded).
#'
#' @details
#' Both the query affiliation and the reference names are passed through
#' \code{.normalize_aff()} (lowercase, strip non-alphanumeric, collapse spaces)
#' before comparison. Names shorter than 10 characters are excluded from the
#' reference list at load time. If the external file is absent the function
#' always returns \code{FALSE} without error.
#'
#' @examples
#' \dontrun{
#' is_teaching_hospital("Johns Hopkins Hospital, Baltimore, MD")  # TRUE
#' is_teaching_hospital("Dr. Smith Private Practice")             # FALSE
#' }
#'
#' @seealso \code{\link{classify_practice_type}}, \code{.normalize_aff}
#' @export
is_teaching_hospital <- function(aff) {
  if (is.na(aff) || nchar(aff) < 5 || length(.TEACHING_NAMES_NORM) == 0)
    return(FALSE)
  aff_norm <- .normalize_aff(aff)
  any(stringr::str_detect(aff_norm, stringr::fixed(.TEACHING_NAMES_NORM)))
}

#' @title Classify an Affiliation's Practice Type
#'
#' @description
#' Returns a single category label describing the institutional setting of a
#' PubMed affiliation. Categories are: \code{"academic"}, \code{"community"},
#' \code{"private_practice"}, \code{"research_institute"}, \code{"military_va"}.
#'
#' @param aff Character scalar. The primary (first-listed) affiliation string.
#' @param all_aff Character scalar. All affiliations concatenated (pipe- or
#'   semicolon-separated). Used to detect university signals when the primary
#'   affiliation alone looks community. Defaults to \code{NA_character_}.
#' @param country Character scalar. Parsed country name (e.g., from
#'   \code{parse_country()}). International affiliations default to
#'   \code{"academic"} rather than \code{"community"} because community vs.
#'   academic distinctions differ outside the US. Defaults to \code{NA_character_}.
#'
#' @return Character scalar. One of \code{"academic"}, \code{"community"},
#'   \code{"private_practice"}, \code{"research_institute"},
#'   \code{"military_va"}, or \code{NA_character_} if no signal is found.
#'
#' @details
#' Classification priority (highest to lowest):
#' \enumerate{
#'   \item Military / VA keyword match.
#'   \item Research institute without a university co-affiliation.
#'   \item ACGME teaching-hospital cross-reference via \code{is_teaching_hospital()}.
#'   \item Regex match against a curated list of academic signals including
#'         named elite institutions (Harvard, Stanford, etc.) and international
#'         academic hospital patterns.
#'   \item Private-practice / group-practice keywords (LLC, PC, etc.).
#'   \item Remaining hospital/clinic affiliations are \code{"community"} for
#'         US and \code{"academic"} for non-US.
#'   \item Department-level affiliations without institution type default to
#'         \code{"academic"}.
#' }
#'
#' @examples
#' \dontrun{
#' classify_practice_type("Department of OB/GYN, Harvard Medical School, Boston, MA")
#' # "academic"
#'
#' classify_practice_type("Walter Reed National Military Medical Center")
#' # "military_va"
#'
#' classify_practice_type("Women's Health Associates LLC")
#' # "private_practice"
#' }
#'
#' @seealso \code{\link{is_teaching_hospital}}, \code{\link{classify_subspecialty}},
#'   \code{\link{classify_career_stage}}
#' @export
classify_practice_type <- function(aff, all_aff = NA_character_, country = NA_character_) {
  if (is.na(aff) || nchar(aff) < 5) return(NA_character_)
  lc <- tolower(aff)
  lc_all <- tolower(if (!is.na(all_aff) && nchar(all_aff) > 0) all_aff else aff)

  # Military / VA first (specific)
  if (str_detect(lc, "\\bva\\b|veterans affairs|military|army|navy|air force|walter reed|tricare|armed forces|madigan"))
    return("military_va")

  # Research institute (without university)
  if (str_detect(lc, "research (institute|center|centre|foundation)|\\binstitut\\b") &&
      !str_detect(lc, "university|medical school|school of medicine"))
    return("research_institute")

  # ACGME cross-reference: check against 2754 known teaching hospitals
  if (is_teaching_hospital(aff)) return("academic")

  # Academic signals (regex fallback)
  academic_patterns <- paste0(
    "university|medical school|school of medicine|college of medicine|",
    "academic medical|teaching hospital|faculty of medicine|",
    "\\bresidency\\b|\\bfellowship\\b|graduate medical education|",
    "\\bgme\\b|medical college|institut(e|o) of|",
    "children.?s hospital.*university|",  # children's hospitals affiliated with universities
    "\\bharvard\\b|\\bstanford\\b|\\byale\\b|\\bduke\\b|\\bjohns hopkins\\b|",
    "\\bcornell\\b|\\bcolumbia\\b|\\bnyu\\b|\\bucsf\\b|\\bucla\\b|",
    # Major non-university academic/research hospitals (US)
    "cleveland clinic|mayo clinic|cedars.?sinai|mount sinai|",
    "memorial sloan|md anderson|brigham and women|massachusetts general|",
    "new york.?presbyterian|rush university|northwestern memorial|",
    # International academic hospital patterns
    "\\birccs\\b|\\bchu\\b|\\bchru\\b|\\bap.?hp\\b|charit|",
    "centre hospitalier universitaire|hopital universitaire|",
    "h.?pital.*universit|ospedale.*universit|krankenhaus.*universit|",
    "academic hospital|universitair|universitets|universit.ts|",
    "\\binserm\\b|\\bcnrs\\b|\\bnhs\\b|\\bking.?s college hospital\\b|",
    "imperial college|oxford university hospital|cambridge university hospital"
  )
  if (str_detect(lc, academic_patterns)) return("academic")

  # Private practice / group practice
  if (str_detect(lc, "private practice|associates|\\bllc\\b|\\bpc\\b|\\bpllc\\b|group practice|medical group|physicians? group|solo practice"))
    return("private_practice")

  # Community hospital / health system (no university affiliation in ANY affiliation)
  # International hospitals default to academic — non-US systems don't follow the
  # US community-vs-academic distinction and we can't reliably classify them.
  is_us <- is.na(country) || str_detect(tolower(country), "^usa$|united states|^u\\.s\\.")
  if (str_detect(lc, "hospital|medical center|health system|health sciences? cent|clinic") &&
      !str_detect(lc_all, "university|medical school|school of medicine|college of medicine") &&
      !is_teaching_hospital(all_aff)) {
    return(if (is_us) "community" else "academic")
  }

  # If we detect a department but no institution type, assume academic (most PubMed authors)
  if (str_detect(lc, "department of|division of|section of"))
    return("academic")

  NA_character_
}

#' @title Classify OB/GYN Subspecialty from an Affiliation String
#'
#' @description
#' Detects the primary OB/GYN subspecialty of an author based on keywords
#' found in their institutional affiliation or department name.
#'
#' @param aff Character scalar. Raw affiliation text (e.g., from PubMed).
#'
#' @return Character scalar. One of \code{"MIGS"}, \code{"REI"},
#'   \code{"GYN_ONC"}, \code{"FPMRS"}, \code{"MFM"},
#'   \code{"family_planning"}, \code{"general_OBGYN"}, \code{"obstetrics"},
#'   \code{"urology"}, \code{"surgery_other"}, or \code{NA_character_} if no
#'   subspecialty keyword is detected.
#'
#' @details
#' Rules are applied in priority order: MIGS > REI > GYN_ONC > FPMRS > MFM >
#' family_planning > general_OBGYN > obstetrics > general gyn > urology >
#' other surgery. The first match wins. Detection is case-insensitive and
#' based on common abbreviations and phrases (e.g., \code{"\\bfpmrs\\b"},
#' \code{"minimally invasive"}, \code{"gyn.?onc"}).
#'
#' @examples
#' \dontrun{
#' classify_subspecialty("Division of FPMRS, University of Colorado")
#' # "FPMRS"
#'
#' classify_subspecialty("Department of Gynecologic Oncology")
#' # "GYN_ONC"
#'
#' classify_subspecialty("General Surgery")
#' # "surgery_other"
#' }
#'
#' @seealso \code{\link{classify_practice_type}}, \code{\link{classify_career_stage}}
#' @export
classify_subspecialty <- function(aff) {
  if (is.na(aff) || nchar(aff) < 5) return(NA_character_)
  lc <- tolower(aff)

  if (str_detect(lc, "minimally invasive|\\bmis\\b|gynecologic surgery|endoscop"))
    return("MIGS")
  if (str_detect(lc, "reproductive endocrinology|\\brei\\b|infertility|ivf|fertility"))
    return("REI")
  if (str_detect(lc, "gynecologic oncology|gyn.?onc"))
    return("GYN_ONC")
  if (str_detect(lc, "urogyn|female pelvic|pelvic floor|\\bfpmrs\\b|reconstructive pelvic"))
    return("FPMRS")
  if (str_detect(lc, "maternal.?fetal|\\bmfm\\b|perinatol|high.?risk pregnan"))
    return("MFM")
  if (str_detect(lc, "family planning|contraception"))
    return("family_planning")
  if (str_detect(lc, "obstetrics and gyn|ob.?gyn|obgyn|obstetrics,? gyn"))
    return("general_OBGYN")
  if (str_detect(lc, "\\bobstetrics\\b|\\bmaternity\\b"))
    return("obstetrics")
  if (str_detect(lc, "\\bgynecol|\\bgynae"))
    return("general_OBGYN")
  if (str_detect(lc, "\\burology\\b"))
    return("urology")
  if (str_detect(lc, "\\bsurgery\\b|\\bsurgical\\b"))
    return("surgery_other")

  NA_character_
}

#' @title Classify Author Career Stage from Affiliation Text
#'
#' @description
#' Infers whether an author is a medical student, resident, fellow, junior
#' faculty, or senior faculty based on keywords in their affiliation string.
#'
#' @param aff Character scalar. Raw affiliation text from a PubMed author record.
#'
#' @return Character scalar. One of \code{"resident"}, \code{"fellow"},
#'   \code{"student"}, \code{"faculty_junior"}, \code{"faculty_senior"},
#'   or \code{NA_character_} if no career-stage signal is detected.
#'
#' @details
#' Rules are applied in priority order: trainee terms (resident, fellow,
#' student) are checked before faculty titles. Within faculty, junior ranks
#' (assistant professor, associate professor, instructor, lecturer) are checked
#' before senior titles (professor, chief, director, chair, head) to prevent
#' \code{"assistant professor"} from matching the senior \code{"professor"}
#' pattern. Affiliation text alone may not reliably distinguish staff physicians
#' from faculty — \code{NA_character_} is returned rather than guessing.
#'
#' @examples
#' \dontrun{
#' classify_career_stage("PGY-3 Resident, OB/GYN, Mayo Clinic")
#' # "resident"
#'
#' classify_career_stage("Associate Professor of OB/GYN, Stanford")
#' # "faculty_junior"
#'
#' classify_career_stage("Chair, Department of Gynecology")
#' # "faculty_senior"
#' }
#'
#' @seealso \code{\link{classify_practice_type}}, \code{\link{classify_subspecialty}}
#' @export
classify_career_stage <- function(aff) {
  if (is.na(aff) || nchar(aff) < 5) return(NA_character_)
  lc <- tolower(aff)

  if (str_detect(lc, "\\bresident\\b|\\bresidency\\b|\\bpgy\\b|\\bintern\\b"))
    return("resident")
  if (str_detect(lc, "\\bfellow\\b|\\bfellowship\\b"))
    return("fellow")
  if (str_detect(lc, "\\bmedical student\\b|\\bstudent\\b"))
    return("student")
  # Junior faculty checked first — "assistant professor" contains "professor"
  if (str_detect(lc, "\\bassistant professor\\b|\\bassociate professor\\b|\\binstructor\\b|\\blecturer\\b"))
    return("faculty_junior")
  if (str_detect(lc, "\\bprofessor\\b|\\bchief\\b|\\bdirector\\b|\\bchair\\b|\\bhead\\b"))
    return("faculty_senior")

  # Can't determine from affiliation alone
  NA_character_
}
