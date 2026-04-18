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
.normalize_aff <- function(x) gsub("\\s+", " ", trimws(gsub("[^a-z0-9 ]", " ", tolower(x))))

# Normalize hospital names at load time; stored as a plain character vector.
.TEACHING_NAMES_NORM <- if (length(TEACHING_HOSPITAL_NAMES) > 0) {
  norms <- unique(.normalize_aff(TEACHING_HOSPITAL_NAMES))
  norms[nchar(norms) > 3]
} else character()

#' Check if an affiliation matches any known teaching/academic hospital name.
#' Uses vectorized fixed-string search — avoids regex alternation length limits.
is_teaching_hospital <- function(aff) {
  if (is.na(aff) || nchar(aff) < 5 || length(.TEACHING_NAMES_NORM) == 0)
    return(FALSE)
  aff_norm <- .normalize_aff(aff)
  any(stringr::str_detect(aff_norm, stringr::fixed(.TEACHING_NAMES_NORM)))
}

#' Classify affiliation as academic, community, private_practice, research_institute, or military.
#' @param aff Primary affiliation string.
#' @param all_aff Optional: all affiliations (pipe-separated). Checked for
#'   university signals if primary affiliation looks community.
classify_practice_type <- function(aff, all_aff = NA_character_) {
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
    "\\bcornell\\b|\\bcolumbia\\b|\\bnyu\\b|\\bucsf\\b|\\bucla\\b"
  )
  if (str_detect(lc, academic_patterns)) return("academic")

  # Private practice / group practice
  if (str_detect(lc, "private practice|associates|\\bllc\\b|\\bpc\\b|\\bpllc\\b|group practice|medical group|physicians? group|solo practice"))
    return("private_practice")

  # Community hospital / health system (no university affiliation in ANY affiliation)
  if (str_detect(lc, "hospital|medical center|health system|health sciences? cent|clinic") &&
      !str_detect(lc_all, "university|medical school|school of medicine|college of medicine") &&
      !is_teaching_hospital(all_aff))
    return("community")

  # If we detect a department but no institution type, assume academic (most PubMed authors)
  if (str_detect(lc, "department of|division of|section of"))
    return("academic")

  NA_character_
}

#' Extract OB/GYN subspecialty from affiliation or department text.
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

#' Detect whether the author is likely a trainee (resident/fellow) vs faculty.
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
