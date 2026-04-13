# utils_crossref.R — CrossRef and Europe PMC search utilities

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(cli)

#' Search CrossRef by title
#' Returns a tibble of candidate matches with DOI and metadata
search_crossref <- function(title, max_results = 5) {
  if (is.na(title) || nchar(title) < 10) return(tibble())

  # Use rcrossref if available, otherwise raw API
  base_url <- "https://api.crossref.org/works"
  email <- Sys.getenv("CROSSREF_EMAIL", "")

  query_params <- list(
    query.bibliographic = title,
    rows = max_results,
    select = "DOI,title,author,published-print,published-online,container-title,abstract,type"
  )
  if (nchar(email) > 0) {
    query_params$mailto <- email
  }

  tryCatch({
    resp <- httr::GET(base_url, query = query_params,
                      httr::timeout(15),
                      httr::user_agent(paste0("abstract_lifetime/1.0 (mailto:", email, ")")))

    if (httr::status_code(resp) != 200) {
      cli_alert_warning("CrossRef returned status {status_code(resp)}")
      return(tibble())
    }

    body <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
    items <- body$message$items
    if (length(items) == 0) return(tibble())

    purrr::map_dfr(items, function(item) {
      doi <- item$DOI %||% NA_character_
      cr_title <- if (length(item$title) > 0) item$title[[1]] else NA_character_

      # Authors
      authors <- if (!is.null(item$author)) {
        purrr::map_chr(item$author, function(a) {
          paste0(a$family %||% "", " ", substr(a$given %||% "", 1, 1))
        })
      } else character(0)
      first_author <- if (length(authors) > 0) authors[1] else NA_character_
      last_author <- if (length(authors) > 1) authors[length(authors)] else first_author

      # Date
      pub_date <- NULL
      if (!is.null(item$`published-print`)) {
        parts <- item$`published-print`$`date-parts`[[1]]
        pub_date <- parts
      } else if (!is.null(item$`published-online`)) {
        parts <- item$`published-online`$`date-parts`[[1]]
        pub_date <- parts
      }
      pub_year <- if (!is.null(pub_date) && length(pub_date) >= 1) as.character(pub_date[[1]]) else NA_character_
      pub_month <- if (!is.null(pub_date) && length(pub_date) >= 2) sprintf("%02d", pub_date[[2]]) else "01"

      # Journal
      journal <- if (length(item$`container-title`) > 0) item$`container-title`[[1]] else NA_character_

      # Abstract
      cr_abstract <- item$abstract %||% NA_character_
      if (!is.na(cr_abstract)) {
        cr_abstract <- str_remove_all(cr_abstract, "<[^>]+>")  # Strip HTML tags
      }

      tibble::tibble(
        doi = doi,
        cr_title = str_squish(cr_title %||% ""),
        cr_first_author = str_squish(first_author %||% ""),
        cr_last_author = str_squish(last_author %||% ""),
        cr_all_authors = paste(authors, collapse = "; "),
        cr_journal = journal,
        cr_abstract = cr_abstract,
        cr_year = pub_year,
        cr_month = pub_month,
        source = "crossref"
      )
    })
  }, error = function(e) {
    cli_alert_warning("CrossRef error: {e$message}")
    tibble()
  })
}

#' Search Europe PMC by title and/or author
#' Broader coverage than PubMed, includes preprints
search_europmc <- function(title, first_author = NULL, max_results = 5) {
  if (is.na(title) || nchar(title) < 10) return(tibble())

  base_url <- "https://www.ebi.ac.uk/europepmc/webservices/rest/search"

  # Build query
  title_clean <- str_replace_all(title, '[\\[\\](){}"+]', " ") |> str_squish()
  query <- paste0('TITLE:"', title_clean, '"')
  if (!is.null(first_author) && !is.na(first_author)) {
    au_parts <- str_split(first_author, "\\s+")[[1]]
    if (length(au_parts) > 0) {
      query <- paste0(query, ' AND AUTH:"', au_parts[1], '"')
    }
  }

  tryCatch({
    resp <- httr::GET(base_url, query = list(
      query = query,
      format = "json",
      pageSize = max_results,
      resultType = "core"
    ), httr::timeout(15))

    if (httr::status_code(resp) != 200) {
      cli_alert_warning("Europe PMC returned status {status_code(resp)}")
      return(tibble())
    }

    body <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
    results <- body$resultList$result
    if (length(results) == 0) return(tibble())

    purrr::map_dfr(results, function(r) {
      tibble::tibble(
        pmid = r$pmid %||% NA_character_,
        doi = r$doi %||% NA_character_,
        epmc_title = str_squish(r$title %||% ""),
        epmc_first_author = str_squish(r$authorString %||% ""),
        epmc_journal = r$journalTitle %||% NA_character_,
        epmc_year = as.character(r$pubYear %||% NA),
        epmc_abstract = str_squish(r$abstractText %||% ""),
        source = "europmc"
      )
    })
  }, error = function(e) {
    cli_alert_warning("Europe PMC error: {e$message}")
    tibble()
  })
}
