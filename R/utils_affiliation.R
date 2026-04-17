# utils_affiliation.R — Extract demographics from PubMed affiliation text.

library(stringr)

#' Classify affiliation as academic, community, private_practice, research_institute, or military.
classify_practice_type <- function(aff) {
  if (is.na(aff) || nchar(aff) < 5) return(NA_character_)
  lc <- tolower(aff)

  # Military / VA first (specific)
  if (str_detect(lc, "\\bva\\b|veterans affairs|military|army|navy|air force|walter reed|tricare|armed forces|madigan"))
    return("military_va")

  # Research institute (without university)
  if (str_detect(lc, "research (institute|center|centre|foundation)|\\binstitut\\b") &&
      !str_detect(lc, "university|medical school|school of medicine"))
    return("research_institute")

  # Academic signals
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

  # Community hospital / health system (no university affiliation)
  if (str_detect(lc, "hospital|medical center|health system|health sciences? cent|clinic") &&
      !str_detect(lc, "university|medical school|school of medicine|college of medicine"))
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
  if (str_detect(lc, "\\bprofessor\\b|\\bchief\\b|\\bdirector\\b|\\bchair\\b|\\bhead\\b"))
    return("faculty_senior")
  if (str_detect(lc, "\\bassistant professor\\b|\\bassociate professor\\b|\\binstructor\\b|\\blecturer\\b"))
    return("faculty_junior")

  # Can't determine from affiliation alone
  NA_character_
}
