# utils_crossref.R — CrossRef, Europe PMC, OpenAlex, and Semantic Scholar search utilities

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(cli)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

#' @title Search CrossRef by Title and Return Candidate Matches
#'
#' @description
#' Queries the CrossRef REST API using bibliographic title search and returns
#' a tidy tibble of candidate publications with DOI, authors, journal, year,
#' and abstract text. Used in the multi-source matching pipeline to supplement
#' PubMed search results.
#'
#' @param title Character scalar. The conference abstract title to search for.
#'   Returns an empty tibble if \code{NA} or shorter than 10 characters.
#' @param max_results Integer scalar. Maximum number of CrossRef results to
#'   return per query. Defaults to \code{5}.
#' @param date_start Character scalar. Earliest publication date filter in
#'   \code{"YYYY-MM-DD"} format. Defaults to \code{"2023-11-01"}.
#' @param date_end Character scalar. Latest publication date filter in
#'   \code{"YYYY-MM-DD"} format. Defaults to \code{"2026-04-01"}.
#'
#' @return A \code{\link[tibble]{tibble}} with columns: \code{doi},
#'   \code{cr_title}, \code{cr_first_author}, \code{cr_last_author},
#'   \code{cr_all_authors}, \code{cr_journal}, \code{cr_abstract},
#'   \code{cr_year}, \code{cr_month}, \code{source} (always
#'   \code{"crossref"}). Returns an empty tibble on API error or no results.
#'
#' @details
#' Uses the \code{query.bibliographic} parameter for best title matching.
#' Polite pool access is enabled by setting the \code{mailto} parameter from
#' the \code{CROSSREF_EMAIL} environment variable. HTML tags are stripped from
#' any returned abstract text. On non-200 HTTP responses or network errors the
#' function logs a warning and returns an empty tibble rather than stopping.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(CROSSREF_EMAIL = "researcher@example.com")
#' search_crossref("Laparoscopic hysterectomy operative time", max_results = 3)
#' }
#'
#' @seealso \code{\link{search_europmc}}, \code{\link{search_openalex}},
#'   \code{\link{search_semantic_scholar}}
#' @export
search_crossref <- function(title, max_results = 5,
                            date_start = "2023-11-01", date_end = "2026-04-01") {
  if (is.na(title) || nchar(title) < 10) return(tibble())

  base_url <- "https://api.crossref.org/works"
  email <- Sys.getenv("CROSSREF_EMAIL", "")

  # CrossRef uses filter=from-pub-date:YYYY-MM-DD,until-pub-date:YYYY-MM-DD
  query_params <- list(
    query.bibliographic = title,
    rows = max_results,
    filter = paste0("from-pub-date:", date_start, ",until-pub-date:", date_end),
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

    purrr::map(items, function(item) {
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
    }) |> purrr::list_rbind()
  }, error = function(e) {
    cli_alert_warning("CrossRef error: {e$message}")
    tibble()
  })
}

#' @title Run a Single Europe PMC REST Query and Parse Results
#'
#' @description
#' Internal helper that executes one Europe PMC \code{/search} API call and
#' returns a tidy tibble. Called by \code{search_europmc()} for each of its
#' search strategies.
#'
#' @param query Character scalar. A fully-formed Europe PMC query string (e.g.,
#'   \code{"TITLE:hysterectomy AND PUB_YEAR:[2023 TO 2026]"}).
#' @param max_results Integer scalar. Number of records to request (maps to
#'   \code{pageSize}). Defaults to \code{5}.
#'
#' @return A \code{\link[tibble]{tibble}} with columns: \code{pmid},
#'   \code{doi}, \code{epmc_title}, \code{epmc_first_author},
#'   \code{epmc_journal}, \code{epmc_year}, \code{epmc_abstract},
#'   \code{source} (\code{"europmc"}). Returns an empty tibble on error or
#'   no results.
#'
#' @keywords internal
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

    purrr::map(results, function(r) {
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
    }) |> purrr::list_rbind()
  }, error = function(e) {
    tibble()
  })
}

#' @title Extract Distinctive Keywords from a Title for API Queries
#'
#' @description
#' Tokenizes a title string, removes common stopwords and very short words,
#' and returns the top \code{n} remaining terms. These terms are used to
#' construct multi-source search queries in place of a raw title.
#'
#' @param title Character scalar. The abstract title to process.
#' @param n Integer scalar. Maximum number of keywords to return. Defaults to
#'   \code{5}.
#'
#' @return Character vector of up to \code{n} unique lowercase keywords, each
#'   at least 4 characters long and not in the internal stopword list.
#'
#' @keywords internal
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

#' @title Extract Last Name from a Normalized Author String
#'
#' @description
#' Splits a normalized author string (e.g., \code{"johannesson U"} or
#' \code{"van der berg A"}) and returns everything except the trailing
#' initial(s) as the last name component.
#'
#' @param normalized_author Character scalar. An author name already processed
#'   by \code{normalize_author()}, in \code{"lastname FI"} format.
#'
#' @return Character scalar with the last-name portion, or \code{NULL} if the
#'   input is \code{NA} or empty.
#'
#' @keywords internal
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

#' @title Search OpenAlex Using Multi-Strategy Keyword Search
#'
#' @description
#' Queries the OpenAlex API using up to two complementary search strategies
#' (keywords-only and author-plus-keywords) and returns a deduplicated tibble
#' of candidate publications. JMIG supplement articles are excluded from results.
#'
#' @param title Character scalar. The conference abstract title.
#' @param first_author Character scalar or \code{NULL}. Normalized first-author
#'   name (e.g., \code{"smith J"}). When provided, enables the author-plus-
#'   keyword strategy. Defaults to \code{NULL}.
#' @param max_results Integer scalar. Records requested per strategy call.
#'   Defaults to \code{5}.
#' @param date_start Character scalar. Start of publication date filter
#'   (\code{"YYYY-MM-DD"}). Defaults to \code{"2023-11-01"}.
#' @param date_end Character scalar. End of publication date filter
#'   (\code{"YYYY-MM-DD"}). Defaults to \code{"2026-04-01"}.
#'
#' @return A \code{\link[tibble]{tibble}} with columns: \code{oa_id},
#'   \code{pmid}, \code{doi}, \code{oa_title}, \code{oa_first_author},
#'   \code{oa_last_author}, \code{oa_all_authors}, \code{oa_journal},
#'   \code{oa_year}, \code{oa_pub_date}, \code{oa_affiliation},
#'   \code{oa_country}, \code{oa_strategy}, \code{source} (\code{"openalex"}).
#'   Returns an empty tibble if no results are found.
#'
#' @details
#' Strategy 1 uses the top 6 title keywords. Strategy 2 prepends the author
#' last name to the top 4 keywords. Both strategies call \code{.openalex_query()}
#' and results are combined with deduplication on \code{oa_id}. JMIG supplement
#' issues within the date range are excluded because they contain the conference
#' abstracts themselves, which would produce spurious self-matches.
#'
#' @examples
#' \dontrun{
#' search_openalex(
#'   title = "Robot-assisted sacrocolpopexy outcomes",
#'   first_author = "smith J",
#'   max_results = 5
#' )
#' }
#'
#' @seealso \code{\link{search_crossref}}, \code{\link{search_europmc}},
#'   \code{\link{search_semantic_scholar}}, \code{.openalex_query}
#' @export
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

  # Filter out JMIG supplement across all congress years in range
  excl_years <- as.character(
    seq(as.integer(substr(date_start, 1, 4)), as.integer(substr(date_end, 1, 4)))
  )
  combined <- combined |>
    dplyr::filter(!(grepl("minim.*invasive.*gynecol", tolower(oa_journal), perl = TRUE) &
                      oa_year %in% excl_years))

  combined
}

#' @title Execute a Single OpenAlex API Query and Parse Results
#'
#' @description
#' Internal helper that performs one HTTP GET to the OpenAlex \code{/works}
#' endpoint and coerces the JSON response into a tidy tibble. Extracts PMIDs
#' from the \code{ids.pmid} field and affiliations from the first authorship.
#'
#' @param base_url Character scalar. OpenAlex works endpoint URL.
#' @param search_term Character scalar. Keyword search string.
#' @param date_filter Character scalar. OpenAlex filter expression for
#'   publication date range (e.g.,
#'   \code{"from_publication_date:2023-11-01,to_publication_date:2026-04-01"}).
#' @param max_results Integer scalar. Number of records to request per page.
#' @param email Character scalar. Polite-pool identifier (sent as
#'   \code{mailto} query parameter when non-empty).
#'
#' @return A \code{\link[tibble]{tibble}} with \code{oa_id}, \code{pmid},
#'   \code{doi}, \code{oa_title}, \code{oa_first_author},
#'   \code{oa_last_author}, \code{oa_all_authors}, \code{oa_journal},
#'   \code{oa_year}, \code{oa_pub_date}, \code{oa_affiliation},
#'   \code{oa_country}. Returns an empty tibble on error.
#'
#' @keywords internal
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

    purrr::map(results, function(r) {
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
    }) |> purrr::list_rbind()
  }, error = function(e) {
    cli_alert_warning("OpenAlex error: {e$message}")
    tibble()
  })
}

#' @title Search Europe PMC Using Multiple Query Strategies
#'
#' @description
#' Queries the Europe PMC REST API using up to three complementary strategies
#' and returns a deduplicated tibble of candidate publications. JMIG supplement
#' articles are excluded from results to prevent self-matching.
#'
#' @param title Character scalar. The conference abstract title.
#' @param first_author Character scalar or \code{NULL}. Normalized first-author
#'   name. When provided, enables author-based strategies. Defaults to
#'   \code{NULL}.
#' @param max_results Integer scalar. Maximum records per strategy query.
#'   Defaults to \code{5}.
#' @param year_start Integer scalar. Start year for publication date filter.
#'   Defaults to \code{2023}.
#' @param year_end Integer scalar. End year for publication date filter.
#'   Defaults to \code{2026}.
#'
#' @return A \code{\link[tibble]{tibble}} with columns: \code{pmid},
#'   \code{doi}, \code{epmc_title}, \code{epmc_first_author},
#'   \code{epmc_journal}, \code{epmc_year}, \code{epmc_abstract},
#'   \code{epmc_strategy}, \code{source} (\code{"europmc"}). Returns an empty
#'   tibble if no results are found.
#'
#' @details
#' Three strategies are tried in order:
#' \enumerate{
#'   \item Author last name + top 4 title keywords (most precise).
#'   \item Top 5 title keywords only (handles author name changes).
#'   \item Author last name + top 2 title keywords (broad catch-all).
#' }
#' Results are combined and deduplicated on (\code{pmid}, \code{doi}) before
#' JMIG supplement filtering.
#'
#' @examples
#' \dontrun{
#' search_europmc(
#'   title = "Laparoscopic myomectomy blood loss",
#'   first_author = "jones A",
#'   year_start = 2023, year_end = 2026
#' )
#' }
#'
#' @seealso \code{\link{search_crossref}}, \code{\link{search_openalex}},
#'   \code{\link{search_semantic_scholar}}, \code{.epmc_query}
#' @export
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

  # Filter out JMIG supplement across all congress years in range
  excl_years_epmc <- as.character(seq(year_start, year_end))
  combined <- combined |>
    dplyr::filter(!(tolower(epmc_journal) %in% c("j minim invasive gynecol") &
                      epmc_year %in% excl_years_epmc))

  combined
}

#' @title Search Semantic Scholar Using the Bulk Search Endpoint
#'
#' @description
#' Queries the Semantic Scholar Graph API bulk-search endpoint using keyword
#' strategies and returns a deduplicated tibble of candidate publications with
#' PMIDs extracted from external identifiers. JMIG supplement articles are
#' excluded.
#'
#' @param title Character scalar. The conference abstract title.
#' @param first_author Character scalar or \code{NULL}. Normalized first-author
#'   name. When provided, enables the author-plus-keyword strategy. Defaults
#'   to \code{NULL}.
#' @param max_results Integer scalar. Maximum records per strategy call.
#'   Defaults to \code{5}.
#' @param year_start Integer scalar. Start year for publication year filter.
#'   Defaults to \code{2023}.
#' @param year_end Integer scalar. End year for publication year filter.
#'   Defaults to \code{2026}.
#'
#' @return A \code{\link[tibble]{tibble}} with columns: \code{s2_id},
#'   \code{pmid}, \code{doi}, \code{s2_title}, \code{s2_first_author},
#'   \code{s2_last_author}, \code{s2_all_authors}, \code{s2_journal},
#'   \code{s2_year}, \code{s2_pub_date}, \code{s2_strategy},
#'   \code{source} (\code{"semantic_scholar"}). Returns an empty tibble on
#'   no results.
#'
#' @details
#' Uses the bulk endpoint (\code{/graph/v1/paper/search/bulk}) which offers
#' more generous rate limits than the relevance endpoint. On HTTP 429 (rate
#' limited) the function sleeps 5 seconds and retries once. Two strategies
#' are used: (1) top 6 keywords, (2) author last name + top 4 keywords.
#'
#' @examples
#' \dontrun{
#' search_semantic_scholar(
#'   title = "Endometriosis fertility outcomes",
#'   first_author = "chen M"
#' )
#' }
#'
#' @seealso \code{\link{search_crossref}}, \code{\link{search_europmc}},
#'   \code{\link{search_openalex}}, \code{.s2_query}
#' @export
search_semantic_scholar <- function(title, first_author = NULL, max_results = 5,
                                    year_start = 2023, year_end = 2026) {
  if (is.na(title) || nchar(title) < 10) return(tibble())

  base_url <- "https://api.semanticscholar.org/graph/v1/paper/search/bulk"
  year_filter <- sprintf("%d-%d", year_start, year_end)
  kw <- .title_keywords(title, n = 6)
  lastname <- if (!is.null(first_author)) .author_lastname(first_author) else NULL

  all_results <- list()

  # Strategy 1: Title keywords (most distinctive terms)
  if (length(kw) >= 3) {
    search_term <- paste(head(kw, 6), collapse = " ")
    r1 <- .s2_query(base_url, search_term, year_filter, max_results)
    if (nrow(r1) > 0) {
      r1$s2_strategy <- "keyword_search"
      all_results <- c(all_results, list(r1))
    }
  }

  # Strategy 2: Author + title keywords
  if (!is.null(lastname) && length(kw) >= 2) {
    search_term <- paste(c(lastname, head(kw, 4)), collapse = " ")
    r2 <- .s2_query(base_url, search_term, year_filter, max_results)
    if (nrow(r2) > 0) {
      r2$s2_strategy <- "author_keyword_search"
      all_results <- c(all_results, list(r2))
    }
  }

  if (length(all_results) == 0) return(tibble())

  combined <- dplyr::bind_rows(all_results) |>
    dplyr::distinct(s2_id, .keep_all = TRUE) |>
    dplyr::mutate(source = "semantic_scholar")

  # Filter out JMIG supplement across all congress years in range
  excl_years_s2 <- as.character(seq(year_start, year_end))
  combined <- combined |>
    dplyr::filter(!(grepl("minim.*invasive.*gynecol", tolower(s2_journal), perl = TRUE) &
                      s2_year %in% excl_years_s2))

  combined
}

#' @title Execute a Single Semantic Scholar Bulk Search Query
#'
#' @description
#' Internal helper that performs one HTTP GET to the Semantic Scholar bulk
#' search endpoint and coerces the JSON response into a tidy tibble. Handles
#' HTTP 429 rate-limit responses with one automatic retry.
#'
#' @param base_url Character scalar. Semantic Scholar bulk search endpoint URL.
#' @param search_term Character scalar. Space-separated keyword query.
#' @param year_filter Character scalar. Year range string in
#'   \code{"YYYY-YYYY"} format (e.g., \code{"2023-2026"}).
#' @param max_results Integer scalar. Maximum records to return per query.
#'
#' @return A \code{\link[tibble]{tibble}} with \code{s2_id}, \code{pmid},
#'   \code{doi}, \code{s2_title}, \code{s2_first_author}, \code{s2_last_author},
#'   \code{s2_all_authors}, \code{s2_journal}, \code{s2_year},
#'   \code{s2_pub_date}. Returns an empty tibble on error.
#'
#' @keywords internal
.s2_query <- function(base_url, search_term, year_filter, max_results) {
  tryCatch({
    resp <- httr::GET(base_url, query = list(
      query = search_term,
      year = year_filter,
      limit = max_results,
      fields = "externalIds,title,authors,journal,publicationDate"
    ), httr::timeout(15),
    httr::user_agent("abstract_lifetime/1.0"))

    sc <- httr::status_code(resp)
    if (sc == 429) {
      # Rate limited — back off and retry once
      Sys.sleep(5)
      resp <- httr::GET(base_url, query = list(
        query = search_term,
        year = year_filter,
        limit = max_results,
        fields = "externalIds,title,authors,journal,publicationDate"
      ), httr::timeout(15),
      httr::user_agent("abstract_lifetime/1.0"))
      sc <- httr::status_code(resp)
    }
    if (sc != 200) return(tibble())

    body <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
    results <- body$data
    if (length(results) == 0) return(tibble())

    purrr::map(results, function(r) {
      # Extract PMID from externalIds
      pmid <- NA_character_
      doi <- NA_character_
      if (!is.null(r$externalIds)) {
        if (!is.null(r$externalIds$PubMed)) pmid <- as.character(r$externalIds$PubMed)
        if (!is.null(r$externalIds$DOI)) doi <- r$externalIds$DOI
      }

      # Authors
      authors <- character(0)
      if (!is.null(r$authors) && length(r$authors) > 0) {
        authors <- purrr::map_chr(r$authors, function(a) a$name %||% "")
      }
      first_author <- if (length(authors) > 0) authors[1] else NA_character_
      last_author <- if (length(authors) > 1) authors[length(authors)] else first_author

      # Journal
      journal <- NA_character_
      if (!is.null(r$journal) && !is.null(r$journal$name)) {
        journal <- r$journal$name
      }

      # Year
      pub_date <- r$publicationDate %||% NA_character_
      pub_year <- if (!is.na(pub_date)) substr(pub_date, 1, 4) else NA_character_

      tibble::tibble(
        s2_id = r$paperId %||% NA_character_,
        pmid = pmid,
        doi = doi,
        s2_title = str_squish(r$title %||% ""),
        s2_first_author = str_squish(first_author %||% ""),
        s2_last_author = str_squish(last_author %||% ""),
        s2_all_authors = paste(authors, collapse = "; "),
        s2_journal = journal,
        s2_year = pub_year,
        s2_pub_date = pub_date
      )
    }) |> purrr::list_rbind()
  }, error = function(e) {
    cli_alert_warning("Semantic Scholar error: {e$message}")
    tibble()
  })
}
