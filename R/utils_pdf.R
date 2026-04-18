# utils_pdf.R — PDF text extraction utilities for JMIG supplement

library(pdftools)
library(stringr)

#' @title Extract Text from a PDF File
#'
#' @description
#' Reads a PDF file and returns its text content as a character vector with
#' one element per page, using the \pkg{pdftools} package.
#'
#' @param pdf_path Character scalar. Absolute path to the PDF file.
#'
#' @return Character vector. Each element contains the rendered text of one
#'   page. Preserves whitespace and line breaks as returned by
#'   \code{pdftools::pdf_text()}.
#'
#' @details
#' Raises an informative error via \code{stop()} if the file does not exist.
#' Multi-column PDFs (such as the JMIG supplement) may require further
#' parsing with \code{\link{parse_pdf_abstracts}} to correctly interleave
#' columns.
#'
#' @examples
#' \dontrun{
#' pages <- extract_pdf_text(here::here("data", "raw", "JMIG_2023_supplement.pdf"))
#' length(pages)  # number of pages
#' }
#'
#' @seealso \code{\link{parse_pdf_abstracts}}, \code{\link{abstracts_to_df}}
#' @export
extract_pdf_text <- function(pdf_path) {
  if (!file.exists(pdf_path)) {
    stop("PDF not found: ", pdf_path)
  }
  pdftools::pdf_text(pdf_path)
}

#' @title Detect Whether a PDF Line Is an Abstract Title
#'
#' @description
#' Applies heuristic rules to decide whether a single PDF line represents an
#' abstract title rather than body text or an author line. Used as the primary
#' state-transition trigger in \code{\link{parse_pdf_abstracts}}.
#'
#' @param line Character scalar. A single line of text extracted from the PDF,
#'   already collapsed with \code{stringr::str_squish()}.
#'
#' @return Logical scalar. \code{TRUE} if the line likely represents a title.
#'
#' @details
#' A line is considered a title if it:
#' \itemize{
#'   \item Has 10–300 characters after whitespace normalization.
#'   \item Does not end with a period (body-text sentences do).
#'   \item Matches the oral-presentation number pattern (\code{"O-NNN"}), OR
#'   \item Has more than 60\% uppercase characters (all-caps or near-caps style).
#' }
#'
#' @examples
#' \dontrun{
#' is_title_line("LAPAROSCOPIC HYSTERECTOMY: OUTCOMES IN 500 PATIENTS")  # TRUE
#' is_title_line("The patient underwent laparoscopic surgery.")            # FALSE
#' is_title_line("O-042")                                                  # TRUE
#' }
#'
#' @seealso \code{\link{parse_pdf_abstracts}}, \code{\link{is_author_line}}
#' @export
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

#' @title Detect Whether a PDF Line Is an Author / Affiliation Line
#'
#' @description
#' Applies heuristic rules to decide whether a single PDF line represents
#' author names or institutional affiliations rather than title or body text.
#'
#' @param line Character scalar. A single line of text from the PDF.
#'
#' @return Logical scalar. \code{TRUE} if the line is likely an author or
#'   affiliation line.
#'
#' @details
#' A line is classified as an author line when it contains at least one
#' professional degree abbreviation (MD, DO, PhD, MPH, MS, FACOG, FACS) or
#' an institutional affiliation keyword (University, Hospital, Medical, Center,
#' Institute, Department) AND starts with an initial-capped word (typical of
#' author last names). Both conditions must be met.
#'
#' @examples
#' \dontrun{
#' is_author_line("Smith JA, MD, Department of OB/GYN")     # TRUE
#' is_author_line("LAPAROSCOPIC MYOMECTOMY BLOOD LOSS")      # FALSE
#' }
#'
#' @seealso \code{\link{is_title_line}}, \code{\link{parse_pdf_abstracts}}
#' @export
is_author_line <- function(line) {
  line <- str_squish(line)
  has_degrees <- str_detect(line, "\\b(MD|DO|PhD|MPH|MS|FACOG|FACS)\\b")
  has_affiliation_markers <- str_detect(line, "\\b(University|Hospital|Medical|Center|Institute|Department)\\b")
  has_name_pattern <- str_detect(line, "^[A-Z][a-z]+\\s")
  (has_degrees || has_affiliation_markers) && has_name_pattern
}

#' @title Detect Structured Abstract Section Headers in a PDF Line
#'
#' @description
#' Checks whether a line begins with a recognized CONSORT/IMRAD section heading
#' (e.g., "Objective:", "Results", "Conclusion:") and returns the canonical
#' section name. Used to segment abstract body text into structured fields.
#'
#' @param line Character scalar. A single whitespace-normalized line of text.
#'
#' @return Character scalar. The matched section name (e.g.,
#'   \code{"Objective"}, \code{"Results"}, \code{"Conclusion"}) or
#'   \code{NA_character_} if the line is not a section header.
#'
#' @details
#' Recognized sections (case-insensitive): Objective, Design, Setting,
#' Patients/Participants, Patients, Participants, Intervention, Measurements
#' and Main Results, Measurements, Main Results, Results, Conclusion,
#' Conclusions. Matching is anchored to the start of the line and allows an
#' optional trailing colon or period.
#'
#' @examples
#' \dontrun{
#' detect_section_header("Objective: To compare...")  # "Objective"
#' detect_section_header("Results")                   # "Results"
#' detect_section_header("The patient had...")        # NA_character_
#' }
#'
#' @seealso \code{\link{parse_pdf_abstracts}}
#' @export
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

#' @title State-Machine Parser for the JMIG Supplement Two-Column PDF
#'
#' @description
#' Iterates over extracted PDF page text and applies a four-state finite
#' automaton to segment individual abstracts from the AAGL annual meeting
#' supplement published in the Journal of Minimally Invasive Gynecology.
#'
#' @param pages_text Character vector. Raw page text as returned by
#'   \code{\link{extract_pdf_text}()}, one element per page.
#'
#' @return A list of named lists, one per detected abstract, each with
#'   elements: \code{title} (character), \code{authors} (character),
#'   \code{affiliation} (character), \code{body} (character),
#'   \code{sections} (named list of section text), \code{page} (integer).
#'   Pass the output to \code{\link{abstracts_to_df}()} to get a tibble.
#'
#' @details
#' State transitions:
#' \describe{
#'   \item{SEEKING_TITLE}{Wait for a line that passes \code{is_title_line()}.}
#'   \item{IN_TITLE}{Accumulate multi-line titles; advance to IN_AUTHORS on
#'     \code{is_author_line()} or to IN_BODY on a section header.}
#'   \item{IN_AUTHORS}{Accumulate author/affiliation lines until a section
#'     header triggers IN_BODY.}
#'   \item{IN_BODY}{Accumulate body text into named sections via
#'     \code{detect_section_header()}; a new title line resets to SEEKING_TITLE
#'     and saves the current abstract.}
#' }
#' The final abstract is saved after the loop ends. Blank lines are skipped.
#'
#' @examples
#' \dontrun{
#' pages <- extract_pdf_text(here::here("data", "raw", "JMIG_2023_supplement.pdf"))
#' abstracts_list <- parse_pdf_abstracts(pages)
#' length(abstracts_list)  # number of abstracts detected
#' }
#'
#' @seealso \code{\link{extract_pdf_text}}, \code{\link{abstracts_to_df}},
#'   \code{\link{is_title_line}}, \code{\link{is_author_line}},
#'   \code{\link{detect_section_header}}
#' @export
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

#' @title Convert a Parsed Abstract List to a Tibble
#'
#' @description
#' Converts the list output of \code{\link{parse_pdf_abstracts}()} into a
#' tidy \code{\link[tibble]{tibble}} suitable for downstream pipeline steps.
#' Each row represents one abstract with structured IMRAD section columns.
#'
#' @param abstracts_list List. Output from \code{\link{parse_pdf_abstracts}()}.
#'   Each element is a named list with \code{title}, \code{authors},
#'   \code{affiliation}, \code{sections}, and \code{page} fields.
#'
#' @return A \code{\link[tibble]{tibble}} with columns: \code{abstract_id}
#'   (character, format \code{"AAGL2023_NNN"}), \code{title},
#'   \code{authors_raw}, \code{affiliation_raw},
#'   \code{abstract_objective}, \code{abstract_design},
#'   \code{abstract_setting}, \code{abstract_patients_participants},
#'   \code{abstract_intervention}, \code{abstract_measurements},
#'   \code{abstract_conclusion}, \code{PDF_page} (integer),
#'   \code{source} (always \code{"pdf"}).
#'
#' @details
#' Section fields are populated from matching keys in \code{a$sections}.
#' Missing sections receive \code{NA_character_}. Section keys fall back
#' in the order defined by \code{\link{detect_section_header}()} (e.g.,
#' \code{"Measurements and Main Results"} before \code{"Measurements"} before
#' \code{"Results"}).
#'
#' @examples
#' \dontrun{
#' pages <- extract_pdf_text(here::here("data", "raw", "JMIG_2023_supplement.pdf"))
#' abstracts_list <- parse_pdf_abstracts(pages)
#' df <- abstracts_to_df(abstracts_list)
#' nrow(df)
#' }
#'
#' @seealso \code{\link{parse_pdf_abstracts}}, \code{\link{extract_pdf_text}}
#' @export
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
