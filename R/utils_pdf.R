# utils_pdf.R — PDF text extraction utilities for JMIG supplement

library(pdftools)
library(stringr)

#' Extract text from PDF using pdftools
#' Returns a character vector, one element per page
extract_pdf_text <- function(pdf_path) {
  if (!file.exists(pdf_path)) {
    stop("PDF not found: ", pdf_path)
  }
  pdftools::pdf_text(pdf_path)
}

#' Detect if a line is an abstract title
#' Heuristics: ALL CAPS or Title Case, no period at end, not too long
is_title_line <- function(line) {
  line <- str_squish(line)
  if (nchar(line) < 10 || nchar(line) > 300) return(FALSE)
  if (str_detect(line, "\\.$")) return(FALSE)
  # Check for oral presentation number pattern (e.g., "O-001")
  if (str_detect(line, "^O-\\d{3}")) return(TRUE)
  # Mostly uppercase
  upper_ratio <- sum(str_detect(strsplit(line, "")[[1]], "[A-Z]")) / nchar(line)
  upper_ratio > 0.6
}

#' Detect if a line is an author line
#' Heuristics: contains commas, MD/DO/PhD, or institutional affiliations
is_author_line <- function(line) {
  line <- str_squish(line)
  has_degrees <- str_detect(line, "\\b(MD|DO|PhD|MPH|MS|FACOG|FACS)\\b")
  has_affiliation_markers <- str_detect(line, "\\b(University|Hospital|Medical|Center|Institute|Department)\\b")
  has_name_pattern <- str_detect(line, "^[A-Z][a-z]+\\s")
  (has_degrees || has_affiliation_markers) && has_name_pattern
}

#' Detect structured abstract section headers
#' Returns the section name or NA
detect_section_header <- function(line) {
  line <- str_squish(line)
  sections <- c("Objective", "Design", "Setting",
                "Patients/Participants", "Patients", "Participants",
                "Intervention", "Measurements and Main Results",
                "Measurements", "Main Results", "Results",
                "Conclusion", "Conclusions")
  for (sec in sections) {
    pattern <- paste0("^", sec, "[:\\.]?\\s*")
    if (str_detect(line, regex(pattern, ignore_case = TRUE))) {
      return(sec)
    }
  }
  NA_character_
}

#' State machine parser for JMIG two-column PDF
#' States: SEEKING_TITLE -> IN_TITLE -> IN_AUTHORS -> IN_BODY -> SEEKING_TITLE
parse_pdf_abstracts <- function(pages_text) {
  abstracts <- list()
  current <- list(
    title = "", authors = "", affiliation = "",
    body = "", sections = list(), page = NA_integer_
  )
  state <- "SEEKING_TITLE"
  current_section <- "preamble"

  for (page_num in seq_along(pages_text)) {
    lines <- str_split(pages_text[page_num], "\n")[[1]]

    for (line in lines) {
      line_clean <- str_squish(line)
      if (nchar(line_clean) == 0) next

      if (state == "SEEKING_TITLE") {
        if (is_title_line(line_clean)) {
          # Save previous abstract if exists
          if (nchar(current$title) > 0) {
            abstracts <- c(abstracts, list(current))
          }
          current <- list(
            title = line_clean, authors = "", affiliation = "",
            body = "", sections = list(), page = page_num
          )
          state <- "IN_TITLE"
        }

      } else if (state == "IN_TITLE") {
        if (is_author_line(line_clean)) {
          current$authors <- line_clean
          state <- "IN_AUTHORS"
        } else if (is_title_line(line_clean)) {
          # Multi-line title
          current$title <- paste(current$title, line_clean)
        } else {
          # Could be start of body
          sec <- detect_section_header(line_clean)
          if (!is.na(sec)) {
            current_section <- sec
            body_text <- str_remove(line_clean, regex(paste0("^", sec, "[:\\.]?\\s*"), ignore_case = TRUE))
            current$sections[[sec]] <- body_text
            state <- "IN_BODY"
          }
        }

      } else if (state == "IN_AUTHORS") {
        sec <- detect_section_header(line_clean)
        if (!is.na(sec)) {
          current_section <- sec
          body_text <- str_remove(line_clean, regex(paste0("^", sec, "[:\\.]?\\s*"), ignore_case = TRUE))
          current$sections[[sec]] <- body_text
          state <- "IN_BODY"
        } else if (is_author_line(line_clean) ||
                   str_detect(line_clean, "^\\d+\\s*(University|Hospital|Department)")) {
          current$authors <- paste(current$authors, line_clean)
        } else {
          current$affiliation <- paste(current$affiliation, line_clean)
        }

      } else if (state == "IN_BODY") {
        if (is_title_line(line_clean)) {
          # New abstract starts
          if (nchar(current$title) > 0) {
            abstracts <- c(abstracts, list(current))
          }
          current <- list(
            title = line_clean, authors = "", affiliation = "",
            body = "", sections = list(), page = page_num
          )
          current_section <- "preamble"
          state <- "IN_TITLE"
        } else {
          sec <- detect_section_header(line_clean)
          if (!is.na(sec)) {
            current_section <- sec
            body_text <- str_remove(line_clean, regex(paste0("^", sec, "[:\\.]?\\s*"), ignore_case = TRUE))
            current$sections[[sec]] <- body_text
          } else {
            # Append to current section
            if (current_section %in% names(current$sections)) {
              current$sections[[current_section]] <- paste(
                current$sections[[current_section]], line_clean
              )
            } else {
              current$sections[[current_section]] <- line_clean
            }
          }
        }
      }
    }
  }

  # Save last abstract
  if (nchar(current$title) > 0) {
    abstracts <- c(abstracts, list(current))
  }

  abstracts
}

#' Convert parsed abstract list to data frame
abstracts_to_df <- function(abstracts_list) {
  purrr::map(seq_along(abstracts_list), function(i) {
    a <- abstracts_list[[i]]
    tibble::tibble(
      abstract_id = sprintf("AAGL2023_%03d", i),
      title = str_squish(a$title),
      authors_raw = str_squish(a$authors),
      affiliation_raw = str_squish(a$affiliation %||% ""),
      abstract_objective = a$sections[["Objective"]] %||% NA_character_,
      abstract_design = a$sections[["Design"]] %||% NA_character_,
      abstract_setting = a$sections[["Setting"]] %||% NA_character_,
      abstract_patients_participants = a$sections[["Patients/Participants"]] %||%
        a$sections[["Patients"]] %||%
        a$sections[["Participants"]] %||% NA_character_,
      abstract_intervention = a$sections[["Intervention"]] %||% NA_character_,
      abstract_measurements = a$sections[["Measurements and Main Results"]] %||%
        a$sections[["Measurements"]] %||%
        a$sections[["Main Results"]] %||%
        a$sections[["Results"]] %||% NA_character_,
      abstract_conclusion = a$sections[["Conclusion"]] %||%
        a$sections[["Conclusions"]] %||% NA_character_,
      PDF_page = a$page,
      source = "pdf"
    )
  }) |> purrr::list_rbind()
}
