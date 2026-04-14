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

#' Run a single Europe PMC query and parse results
#' @return tibble of results
.epmc_query <- function(query, max_results = 5) {
  base_url <- "https://www.ebi.ac.uk/europepmc/webservices/rest/search"

  tryCatch({
    resp <- httr::GET(base_url, query = list(
      query = query,
      format = "json",
      pageSize = max_results,
      resultType = "core"
    ), httr::timeout(15))

    if (httr::status_code(resp) != 200) return(tibble())

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
    tibble()
  })
}

#' Extract distinctive title keywords for search queries
#' Removes stopwords and very short words, returns top N terms
.title_keywords <- function(title, n = 5) {
  stops <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to",
             "for", "of", "with", "by", "from", "is", "was", "were", "are",
             "be", "this", "that", "its", "not", "as", "after", "between",
             "using", "based", "study", "case", "report", "new", "novel",
             "results", "analysis", "review", "patients", "following", "among",
             "during", "versus", "compared")
  words <- title |>
    tolower() |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish() |>
    str_split("\\s+") |>
    unlist()
  words <- words[nchar(words) >= 4 & !words %in% stops]
  head(unique(words), n)
}

#' Extract the last name from a normalized author string like "johannesson U"
.author_lastname <- function(normalized_author) {
  if (is.na(normalized_author) || normalized_author == "") return(NULL)
  parts <- str_split(str_squish(normalized_author), "\\s+")[[1]]
  # Last name is everything except the final initial
  if (length(parts) >= 2) {
    paste(parts[1:(length(parts) - 1)], collapse = " ")
  } else {
    parts[1]
  }
}

#' Search OpenAlex using multi-strategy keyword search
#' Returns a tibble with pmid, doi, title, authors, journal, year
search_openalex <- function(title, first_author = NULL, max_results = 5,
                            date_start = "2023-11-01", date_end = "2026-04-01") {
  if (is.na(title) || nchar(title) < 10) return(tibble())

  base_url <- "https://api.openalex.org/works"
  email <- Sys.getenv("CROSSREF_EMAIL", "")
  date_filter <- sprintf("from_publication_date:%s,to_publication_date:%s", date_start, date_end)

  kw <- .title_keywords(title, n = 6)
  lastname <- if (!is.null(first_author)) .author_lastname(first_author) else NULL

  all_results <- list()

  # Strategy 1: Full keyword search (title + abstract) — most relevant
  if (length(kw) >= 3) {
    search_term <- paste(head(kw, 6), collapse = " ")
    r1 <- .openalex_query(base_url, search_term, date_filter, max_results, email)
    if (nrow(r1) > 0) {
      r1$oa_strategy <- "keyword_search"
      all_results <- c(all_results, list(r1))
    }
  }

  # Strategy 2: Author last name + top keywords in search
  if (!is.null(lastname) && length(kw) >= 2) {
    search_term <- paste(c(lastname, head(kw, 4)), collapse = " ")
    r2 <- .openalex_query(base_url, search_term, date_filter, max_results, email)
    if (nrow(r2) > 0) {
      r2$oa_strategy <- "author_keyword_search"
      all_results <- c(all_results, list(r2))
    }
  }

  if (length(all_results) == 0) return(tibble())

  combined <- dplyr::bind_rows(all_results) |>
    dplyr::distinct(oa_id, .keep_all = TRUE) |>
    dplyr::mutate(source = "openalex")

  # Filter out JMIG supplement (2023)
  combined <- combined |>
    dplyr::filter(!(grepl("minim.*invasive.*gynecol", tolower(oa_journal), perl = TRUE) &
                      oa_year == "2023"))

  combined
}

#' Execute a single OpenAlex API query and parse results
.openalex_query <- function(base_url, search_term, date_filter, max_results, email) {
  tryCatch({
    params <- list(
      search = search_term,
      filter = date_filter,
      per_page = max_results,
      select = "id,doi,ids,display_name,authorships,publication_date,primary_location"
    )
    if (nchar(email) > 0) params$mailto <- email

    resp <- httr::GET(base_url, query = params, httr::timeout(15),
                      httr::user_agent("abstract_lifetime/1.0"))

    if (httr::status_code(resp) != 200) return(tibble())

    body <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
    results <- body$results
    if (length(results) == 0) return(tibble())

    purrr::map_dfr(results, function(r) {
      # Extract PMID from ids object
      pmid <- NA_character_
      if (!is.null(r$ids$pmid)) {
        pmid <- gsub("https://pubmed.ncbi.nlm.nih.gov/", "", r$ids$pmid)
      }

      # DOI
      doi <- NA_character_
      if (!is.null(r$doi)) {
        doi <- gsub("https://doi.org/", "", r$doi)
      }

      # Authors
      authors <- character(0)
      if (!is.null(r$authorships) && length(r$authorships) > 0) {
        authors <- purrr::map_chr(r$authorships, function(a) {
          a$author$display_name %||% ""
        })
      }
      first_author <- if (length(authors) > 0) authors[1] else NA_character_
      last_author <- if (length(authors) > 1) authors[length(authors)] else first_author

      # Journal
      journal <- NA_character_
      if (!is.null(r$primary_location) && !is.null(r$primary_location$source)) {
        journal <- r$primary_location$source$display_name %||% NA_character_
      }

      # Year from publication_date
      pub_date <- r$publication_date %||% NA_character_
      pub_year <- if (!is.na(pub_date)) substr(pub_date, 1, 4) else NA_character_

      # Affiliations (first author)
      affiliation <- NA_character_
      if (length(r$authorships) > 0) {
        raw_affs <- r$authorships[[1]]$raw_affiliation_strings
        if (!is.null(raw_affs) && length(raw_affs) > 0) {
          affiliation <- raw_affs[[1]]
        }
      }

      # Country (first author)
      country <- NA_character_
      if (length(r$authorships) > 0) {
        countries <- r$authorships[[1]]$countries
        if (!is.null(countries) && length(countries) > 0) {
          country <- countries[[1]]
        }
      }

      tibble::tibble(
        oa_id = r$id %||% NA_character_,
        pmid = pmid,
        doi = doi,
        oa_title = str_squish(r$display_name %||% ""),
        oa_first_author = str_squish(first_author %||% ""),
        oa_last_author = str_squish(last_author %||% ""),
        oa_all_authors = paste(authors, collapse = "; "),
        oa_journal = journal,
        oa_year = pub_year,
        oa_pub_date = pub_date,
        oa_affiliation = affiliation,
        oa_country = country
      )
    })
  }, error = function(e) {
    cli_alert_warning("OpenAlex error: {e$message}")
    tibble()
  })
}

#' Search Europe PMC using multiple strategies
#' Returns combined deduplicated results across all strategies
search_europmc <- function(title, first_author = NULL, max_results = 5,
                           year_start = 2023, year_end = 2026) {
  if (is.na(title) || nchar(title) < 10) return(tibble())

  year_filter <- sprintf("PUB_YEAR:[%d TO %d]", year_start, year_end)
  kw <- .title_keywords(title, n = 5)
  lastname <- if (!is.null(first_author)) .author_lastname(first_author) else NULL

  all_results <- list()

  # Strategy 1: Author + title keywords (most precise)
  if (!is.null(lastname) && length(kw) >= 2) {
    kw_part <- paste(sprintf("TITLE:%s", head(kw, 4)), collapse = " AND ")
    q1 <- paste0("AUTH:", lastname, " AND ", kw_part, " AND ", year_filter)
    r1 <- .epmc_query(q1, max_results)
    if (nrow(r1) > 0) {
      r1$epmc_strategy <- "author_title_kw"
      all_results <- c(all_results, list(r1))
    }
  }

  # Strategy 2: Title keywords only (catches author name changes)
  if (length(kw) >= 3) {
    kw_part <- paste(sprintf("TITLE:%s", head(kw, 5)), collapse = " AND ")
    q2 <- paste0(kw_part, " AND ", year_filter)
    r2 <- .epmc_query(q2, max_results)
    if (nrow(r2) > 0) {
      r2$epmc_strategy <- "title_kw_only"
      all_results <- c(all_results, list(r2))
    }
  }

  # Strategy 3: Author + fewer keywords (broader)
  if (!is.null(lastname) && length(kw) >= 1) {
    kw_part <- paste(sprintf("TITLE:%s", head(kw, 2)), collapse = " AND ")
    q3 <- paste0("AUTH:", lastname, " AND ", kw_part, " AND ", year_filter)
    r3 <- .epmc_query(q3, max_results)
    if (nrow(r3) > 0) {
      r3$epmc_strategy <- "author_broad_kw"
      all_results <- c(all_results, list(r3))
    }
  }

  if (length(all_results) == 0) return(tibble())

  # Combine and deduplicate
  combined <- dplyr::bind_rows(all_results) |>
    dplyr::distinct(pmid, doi, .keep_all = TRUE) |>
    dplyr::mutate(source = "europmc")

  # Filter out JMIG supplement (Vol 30, 2023)
  combined <- combined |>
    dplyr::filter(!(tolower(epmc_journal) %in% c("j minim invasive gynecol") &
                      epmc_year == "2023"))

  combined
}
