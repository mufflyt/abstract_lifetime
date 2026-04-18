# utils_pubmed.R — PubMed search utilities with multi-strategy support

library(rentrez)
library(httr)
library(xml2)
library(dplyr)
library(stringr)
library(cli)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# Rate limiting state
.pubmed_env <- new.env(parent = emptyenv())
.pubmed_env$last_request <- Sys.time() - 1

#' @title Execute a Rate-Limited PubMed Search Query
#'
#' @description
#' Submits a PubMed search query via \pkg{rentrez} while respecting NCBI's
#' API rate limits (3 requests/second without an API key, up to 10/second with
#' one). Uses a shared environment to track the timestamp of the last request
#' and sleeps as needed before each call.
#'
#' @param query Character scalar. A valid PubMed Entrez query string, including
#'   field tags (e.g., \code{'[TI]'}, \code{'[AU]'}, \code{'[PDAT]'}).
#' @param retmax Integer scalar. Maximum number of PMIDs to return. Defaults
#'   to \code{20}.
#' @param cfg List or \code{NULL}. Parsed config object. When \code{NULL} the
#'   config is read from \code{config.yml} in the project root. Used to read
#'   \code{pubmed$rate_limit_per_sec} and \code{pubmed$rate_limit_with_key}.
#'
#' @return An \code{entrez_search} result object (from \pkg{rentrez}) with
#'   \code{ids} and \code{count} fields, or \code{NULL} on error (a warning
#'   is emitted and the function sleeps 1 second before returning).
#'
#' @details
#' Rate-limit state is stored in the package-level environment
#' \code{.pubmed_env$last_request}. Whether an API key is present is
#' determined by checking the \code{ENTREZ_KEY} environment variable.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(ENTREZ_KEY = "your_ncbi_key")
#' result <- rate_limited_search('"hysterectomy"[TI] AND 2023:2026[PDAT]')
#' result$ids
#' }
#'
#' @seealso \code{\link{search_abstract}}, \code{\link{fetch_pubmed_details}}
#' @export
rate_limited_search <- function(query, retmax = 20, cfg = NULL) {
  if (is.null(cfg)) cfg <- config::get(file = here::here("config.yml"))

  has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
  delay <- if (has_key) 1 / cfg$pubmed$rate_limit_with_key else 1 / cfg$pubmed$rate_limit_per_sec

  elapsed <- as.numeric(difftime(Sys.time(), .pubmed_env$last_request, units = "secs"))
  if (elapsed < delay) Sys.sleep(delay - elapsed)

  .pubmed_env$last_request <- Sys.time()

  tryCatch({
    result <- rentrez::entrez_search(
      db = "pubmed",
      term = query,
      retmax = retmax,
      use_history = FALSE
    )
    result
  }, error = function(e) {
    cli_alert_warning("PubMed search error: {e$message}")
    Sys.sleep(1)
    NULL
  })
}

#' @title Fetch PubMed Article Details for a Vector of PMIDs
#'
#' @description
#' Retrieves full PubMed article metadata (title, authors, journal, date, DOI,
#' keywords, publication types) for a vector of PMIDs. Requests are batched in
#' groups of 100 to comply with NCBI limits and rate-limited between batches.
#'
#' @param pmids Character or integer vector. One or more PubMed IDs. An empty
#'   vector returns an empty tibble immediately.
#' @param cfg List or \code{NULL}. Parsed config object; auto-loaded when
#'   \code{NULL}.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per PMID, combining all
#'   batch results. Column structure matches \code{\link{parse_pubmed_xml}()}.
#'   Batches that fail are skipped with a warning.
#'
#' @details
#' Uses \code{rentrez::entrez_fetch(rettype = "xml")} and delegates XML parsing
#' to \code{\link{parse_pubmed_xml}()}. Batching at 100 PMIDs is the maximum
#' safe batch size for the Entrez E-utilities fetch endpoint.
#'
#' @examples
#' \dontrun{
#' details <- fetch_pubmed_details(c("37000001", "37000002"))
#' dplyr::glimpse(details)
#' }
#'
#' @seealso \code{\link{rate_limited_search}}, \code{\link{parse_pubmed_xml}},
#'   \code{\link{fetch_pubmed_xml}}
#' @export
fetch_pubmed_details <- function(pmids, cfg = NULL) {
  if (length(pmids) == 0) return(tibble())
  if (is.null(cfg)) cfg <- config::get(file = here::here("config.yml"))

  has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
  delay <- if (has_key) 1 / cfg$pubmed$rate_limit_with_key else 1 / cfg$pubmed$rate_limit_per_sec

  # Batch in groups of 100

  all_details <- list()
  batches <- split(pmids, ceiling(seq_along(pmids) / 100))

  for (batch in batches) {
    elapsed <- as.numeric(difftime(Sys.time(), .pubmed_env$last_request, units = "secs"))
    if (elapsed < delay) Sys.sleep(delay - elapsed)
    .pubmed_env$last_request <- Sys.time()

    tryCatch({
      raw <- rentrez::entrez_fetch(
        db = "pubmed",
        id = batch,
        rettype = "xml"
      )
      parsed <- parse_pubmed_xml(raw)
      all_details <- c(all_details, list(parsed))
    }, error = function(e) {
      cli_alert_warning("PubMed fetch error: {e$message}")
    })
  }

  bind_rows(all_details)
}

#' @title Parse a PubMed XML Response into a Tidy Tibble
#'
#' @description
#' Parses raw PubMed XML (as returned by \code{rentrez::entrez_fetch()}) and
#' coerces each \code{PubmedArticle} node into a one-row tibble, then combines
#' all rows into a single tibble.
#'
#' @param xml_text Character scalar. Raw XML string containing one or more
#'   \code{<PubmedArticle>} elements.
#'
#' @return A \code{\link[tibble]{tibble}} with columns: \code{pmid},
#'   \code{pub_title}, \code{pub_abstract}, \code{pub_first_author},
#'   \code{pub_last_author}, \code{pub_all_authors}, \code{pub_journal},
#'   \code{pub_journal_abbrev}, \code{pub_volume}, \code{pub_year},
#'   \code{pub_month}, \code{pub_day}, \code{pub_doi}, \code{pub_keywords},
#'   \code{pub_types}. Missing XML fields receive \code{NA_character_}.
#'
#' @details
#' Abstract text is assembled by concatenating all \code{<AbstractText>}
#' nodes (handles structured abstracts with labeled sections). Authors are
#' formatted as \code{"LastName Initials"}; the semicolon-separated full
#' author string is stored in \code{pub_all_authors}. Publication types from
#' \code{<PublicationTypeList>} are collapsed into a single semicolon-separated
#' string for downstream use by \code{\link{canonical_pub_type}()}.
#'
#' @examples
#' \dontrun{
#' raw_xml <- rentrez::entrez_fetch(db = "pubmed", id = "37000001", rettype = "xml")
#' df <- parse_pubmed_xml(raw_xml)
#' df$pub_title
#' }
#'
#' @seealso \code{\link{fetch_pubmed_details}}, \code{\link{fetch_pubmed_xml}},
#'   \code{\link{canonical_pub_type}}
#' @export
parse_pubmed_xml <- function(xml_text) {
  doc <- read_xml(xml_text)
  articles <- xml_find_all(doc, "//PubmedArticle")

  purrr::map(articles, function(art) {
    pmid <- xml_text(xml_find_first(art, ".//PMID"))
    title <- xml_text(xml_find_first(art, ".//ArticleTitle")) %||% NA_character_
    abstract_text <- {
      parts <- xml_text(xml_find_all(art, ".//AbstractText"))
      if (length(parts) > 0) paste(parts, collapse = " ") else NA_character_
    }

    # Authors
    author_nodes <- xml_find_all(art, ".//Author")
    authors <- purrr::map_chr(author_nodes, function(a) {
      last <- xml_text(xml_find_first(a, "LastName")) %||% ""
      fore <- xml_text(xml_find_first(a, "ForeName")) %||% ""
      initials <- xml_text(xml_find_first(a, "Initials")) %||% ""
      paste0(last, " ", initials)
    })
    first_author <- if (length(authors) > 0) authors[1] else NA_character_
    last_author <- if (length(authors) > 1) authors[length(authors)] else first_author
    all_authors <- if (length(authors) > 0) paste(authors, collapse = "; ") else NA_character_

    # Journal
    journal <- xml_text(xml_find_first(art, ".//Journal/Title")) %||% NA_character_
    journal_abbrev <- xml_text(xml_find_first(art, ".//Journal/ISOAbbreviation")) %||% NA_character_
    volume <- xml_text(xml_find_first(art, ".//JournalIssue/Volume")) %||% NA_character_

    # Date
    year <- xml_text(xml_find_first(art, ".//JournalIssue/PubDate/Year")) %||% NA_character_
    month <- xml_text(xml_find_first(art, ".//JournalIssue/PubDate/Month")) %||% "01"
    day <- xml_text(xml_find_first(art, ".//JournalIssue/PubDate/Day")) %||% "01"

    # DOI
    doi_node <- xml_find_first(art, ".//ArticleId[@IdType='doi']")
    doi <- if (!is.na(doi_node)) xml_text(doi_node) else NA_character_

    # Keywords / MeSH
    keywords <- {
      kw <- xml_text(xml_find_all(art, ".//Keyword"))
      if (length(kw) > 0) paste(kw, collapse = "; ") else NA_character_
    }

    # Publication types — concatenate all. Used downstream to stratify by
    # journal-article / review / case-report / trial.
    pub_types <- {
      pt <- xml_text(xml_find_all(art, ".//PublicationTypeList/PublicationType"))
      if (length(pt) > 0) paste(pt, collapse = "; ") else NA_character_
    }

    tibble::tibble(
      pmid = pmid,
      pub_title = str_squish(title),
      pub_abstract = str_squish(abstract_text),
      pub_first_author = str_squish(first_author),
      pub_last_author = str_squish(last_author),
      pub_all_authors = str_squish(all_authors),
      pub_journal = journal,
      pub_journal_abbrev = journal_abbrev,
      pub_volume = volume,
      pub_year = year,
      pub_month = month,
      pub_day = day,
      pub_doi = doi,
      pub_keywords = keywords,
      pub_types = pub_types
    )
  }) |> purrr::list_rbind()
}

#' @title Fetch Raw PubMed XML for a Single PMID with Disk Caching
#'
#' @description
#' Retrieves the PubMed XML record for a single PMID, using a local disk
#' cache to avoid redundant API calls. Cached files are read directly if they
#' exist and are larger than 100 bytes; otherwise a live fetch is performed
#' and the result is written to cache.
#'
#' @param pmid Character scalar. A single PubMed ID.
#' @param cache_dir Character scalar. Path to the directory where
#'   \code{<pmid>.xml} files are stored (created externally; not created
#'   automatically by this function).
#' @param delay Numeric scalar. Seconds to sleep before a live API fetch to
#'   respect NCBI rate limits. Defaults to \code{0.34} (approximately 3
#'   requests/second). Set lower when an ENTREZ_KEY is in use.
#'
#' @return Character scalar. Raw XML string on success, or
#'   \code{NA_character_} if the fetch fails. A warning is emitted on failure.
#'
#' @details
#' Cache files smaller than 100 bytes are treated as invalid (e.g., empty
#' error responses) and will trigger a fresh live fetch. Successful responses
#' larger than 100 characters are written to disk with
#' \code{readr::write_file()}.
#'
#' @examples
#' \dontrun{
#' cache <- here::here("data", "cache", "pubmed_xml")
#' dir.create(cache, recursive = TRUE, showWarnings = FALSE)
#' xml <- fetch_pubmed_xml("37000001", cache_dir = cache)
#' }
#'
#' @seealso \code{\link{parse_pubmed_xml}}, \code{\link{fetch_pubmed_details}}
#' @export
fetch_pubmed_xml <- function(pmid, cache_dir, delay = 0.34) {
  path <- file.path(cache_dir, paste0(pmid, ".xml"))
  if (file.exists(path) && file.info(path)$size > 100) {
    return(readr::read_file(path))
  }
  Sys.sleep(delay)
  raw <- tryCatch(
    rentrez::entrez_fetch(db = "pubmed", id = pmid, rettype = "xml"),
    error = function(e) {
      cli::cli_alert_warning("PubMed XML fetch failed for PMID {pmid}: {e$message}")
      NA_character_
    }
  )
  if (!is.na(raw) && nchar(raw) > 100) readr::write_file(raw, path)
  raw
}

#' @title Build a PubMed Publication Date Range Filter String
#'
#' @description
#' Constructs the \code{[PDAT]} Entrez date filter expression from config
#' values, ready for use in PubMed query strings.
#'
#' @param cfg List or \code{NULL}. Parsed config object; auto-loaded when
#'   \code{NULL}. Must contain \code{pubmed$date_start} and
#'   \code{pubmed$date_end} keys (format \code{"YYYY/MM/DD"}).
#'
#' @return Character scalar. A PubMed date filter expression of the form
#'   \code{"YYYY/MM/DD:YYYY/MM/DD[PDAT]"}.
#'
#' @examples
#' \dontrun{
#' cfg <- config::get(file = here::here("config.yml"))
#' build_date_filter(cfg)
#' # "2023/11/01:2026/04/01[PDAT]"
#' }
#'
#' @seealso \code{\link{build_search_strategies}}, \code{\link{rate_limited_search}}
#' @export
build_date_filter <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- config::get(file = here::here("config.yml"))
  sprintf("%s:%s[PDAT]", cfg$pubmed$date_start, cfg$pubmed$date_end)
}

#' @title Detect Whether a PubMed Result Row Is a JMIG Supplement Article
#'
#' @description
#' Identifies PubMed articles that belong to the AAGL conference supplement
#' issue of the Journal of Minimally Invasive Gynecology (JMIG) so they can
#' be excluded from publication matches. Conference abstracts published in the
#' supplement are not full publications and must not be counted as matches.
#'
#' @param details_row A single-row data frame or named list. Must contain
#'   \code{pub_journal_abbrev}, \code{pub_volume}, and \code{pub_year} fields
#'   as returned by \code{\link{parse_pubmed_xml}()}.
#' @param cfg List or \code{NULL}. Parsed config object; auto-loaded when
#'   \code{NULL}. Used to read \code{pubmed$exclude_supplement_vol} and
#'   \code{pubmed$exclude_supplement_year}.
#'
#' @return Logical scalar. \code{TRUE} if the article is from the JMIG
#'   supplement issue defined in config; \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' cfg <- config::get(file = here::here("config.yml"))
#' row <- tibble::tibble(pub_journal_abbrev = "J Minim Invasive Gynecol",
#'                       pub_volume = "30", pub_year = "2023")
#' is_supplement_article(row, cfg)  # TRUE or FALSE depending on config
#' }
#'
#' @seealso \code{\link{parse_pubmed_xml}}, \code{\link{search_abstract}}
#' @export
is_supplement_article <- function(details_row, cfg = NULL) {
  if (is.null(cfg)) cfg <- config::get(file = here::here("config.yml"))
  if (is.na(details_row$pub_journal_abbrev)) return(FALSE)

  is_jmig <- str_detect(tolower(details_row$pub_journal_abbrev), "j minim invasive gynecol")
  is_supplement_vol <- !is.na(details_row$pub_volume) &&
    details_row$pub_volume %in% as.character(cfg$pubmed$exclude_supplement_vol)
  is_supplement_year <- !is.na(details_row$pub_year) &&
    details_row$pub_year %in% as.character(cfg$pubmed$exclude_supplement_year)

  is_jmig && is_supplement_vol && is_supplement_year
}

#' @title Generate All PubMed Search Strategies for One Abstract
#'
#' @description
#' Builds up to six complementary PubMed Entrez query strings for a single
#' abstract row, covering different combinations of title words, author names,
#' keywords, and distinctive phrase fragments.
#'
#' @param abstract_row A single-row data frame or named list. Must contain at
#'   minimum: \code{title_normalized}, \code{first_author_normalized},
#'   \code{last_author_normalized}, \code{keywords} (character vector or
#'   \code{NULL}), and \code{title} (raw). \code{congress_year} is used when
#'   present for multi-cohort date filtering.
#' @param cfg List or \code{NULL}. Parsed config object; auto-loaded when
#'   \code{NULL}.
#'
#' @return Named character list with up to six elements:
#' \describe{
#'   \item{title}{First 8 meaningful title words in \code{[TI]}.}
#'   \item{first_author}{First author in \code{[1AU]} with date filter.}
#'   \item{last_author}{Last author in \code{[LASTAU]} with date filter.}
#'   \item{author_keywords}{First author in \code{[AU]} combined with top 4
#'     keywords in \code{[TIAB]}.}
#'   \item{title_fragment}{Distinctive 4-word phrase from
#'     \code{\link{distinctive_phrase}()} in \code{[TIAB]}.}
#'   \item{author_broad}{First author in \code{[AU]} without keyword
#'     constraint (broadest catch-all).}
#' }
#' Strategies whose required fields are missing or too short are omitted.
#'
#' @examples
#' \dontrun{
#' row <- tibble::tibble(
#'   abstract_id = "AAGL2023_001",
#'   title = "Robotic Sacrocolpopexy Outcomes at a Single Center",
#'   title_normalized = "robotic sacrocolpopexy outcomes at a single center",
#'   first_author_normalized = "smith J",
#'   last_author_normalized = "jones A",
#'   keywords = c("sacrocolpopexy", "robotic", "prolapse"),
#'   congress_year = 2023L
#' )
#' cfg <- config::get(file = here::here("config.yml"))
#' strats <- build_search_strategies(row, cfg)
#' names(strats)
#' }
#'
#' @seealso \code{\link{search_abstract}}, \code{\link{build_date_filter}},
#'   \code{\link{distinctive_phrase}}
#' @export
build_search_strategies <- function(abstract_row, cfg = NULL) {
  if (is.null(cfg)) cfg <- config::get(file = here::here("config.yml"))

  date_filter <- build_date_filter(cfg)
  title_norm <- abstract_row$title_normalized
  first_au <- abstract_row$first_author_normalized
  last_au <- abstract_row$last_author_normalized
  keywords <- if (!is.null(abstract_row$keywords)) abstract_row$keywords else character(0)
  keywords <- keywords[!is.na(keywords) & nchar(keywords) > 0]

  strategies <- list()

  # Strategy 1: Title search
  # Use key words from title (first 6 meaningful words)
  title_words <- str_split(title_norm, "\\s+")[[1]]
  title_words <- title_words[nchar(title_words) >= 3]
  title_query_words <- head(title_words, 8)
  if (length(title_query_words) > 0) {
    strategies[["title"]] <- sprintf(
      '"%s"[TI] AND %s',
      paste(title_query_words, collapse = " "),
      date_filter
    )
  }

  # Strategy 2: First author + date

  if (!is.na(first_au) && nchar(first_au) > 1) {
    strategies[["first_author"]] <- sprintf(
      '"%s"[1AU] AND %s',
      first_au, date_filter
    )
  }

  # Strategy 3: Last author + date
  if (!is.na(last_au) && nchar(last_au) > 1) {
    strategies[["last_author"]] <- sprintf(
      '"%s"[LASTAU] AND %s',
      last_au, date_filter
    )
  }

  # Strategy 4: Author + keywords
  if (!is.na(first_au) && length(keywords) >= 2) {
    kw_query <- paste0("(", paste(head(keywords, 4), collapse = " OR "), ")[TIAB]")
    strategies[["author_keywords"]] <- sprintf(
      '"%s"[AU] AND %s AND %s',
      first_au, kw_query, date_filter
    )
  }

  # Strategy 5: Distinctive title fragment
  source(here::here("R", "utils_text.R"), local = TRUE)
  phrase <- distinctive_phrase(abstract_row$title)
  if (nchar(phrase) > 5) {
    strategies[["title_fragment"]] <- sprintf(
      '"%s"[TIAB] AND %s',
      phrase, date_filter
    )
  }

  # Strategy 6: Author + broad (catch-all)
  if (!is.na(first_au) && nchar(first_au) > 1) {
    strategies[["author_broad"]] <- sprintf(
      '"%s"[AU] AND %s',
      first_au, date_filter
    )
  }

  strategies
}

#' @title Execute All PubMed Search Strategies for One Abstract
#'
#' @description
#' Runs each query returned by \code{\link{build_search_strategies}()} against
#' PubMed and aggregates the resulting PMIDs into a deduplicated tibble that
#' records which strategies found each candidate.
#'
#' @param abstract_row A single-row data frame or named list compatible with
#'   \code{\link{build_search_strategies}()}.
#' @param cfg List or \code{NULL}. Parsed config object; auto-loaded when
#'   \code{NULL}. Used to set \code{retmax} per query via
#'   \code{pubmed$max_results_per_query}.
#'
#' @return A \code{\link[tibble]{tibble}} with columns: \code{pmid}
#'   (character), \code{strategies} (semicolon-separated strategy names that
#'   returned this PMID), \code{n_strategies} (integer), \code{first_query}
#'   (the query text of the first strategy that found this PMID). Returns an
#'   empty tibble with those columns when no PMIDs are found.
#'
#' @details
#' Each strategy is run via \code{\link{rate_limited_search}()}. The per-PMID
#' strategy provenance is tracked by grouping on PMID and summarising. A
#' PMID found by more strategies is a stronger candidate and will receive a
#' higher score downstream in \code{\link{score_abstract_candidates}()}.
#'
#' @examples
#' \dontrun{
#' cfg <- config::get(file = here::here("config.yml"))
#' row <- abstracts[1, ]
#' candidates <- search_abstract(row, cfg)
#' candidates
#' }
#'
#' @seealso \code{\link{build_search_strategies}}, \code{\link{rate_limited_search}},
#'   \code{\link{fetch_pubmed_details}}, \code{\link{score_abstract_candidates}}
#' @export
search_abstract <- function(abstract_row, cfg = NULL) {
  if (is.null(cfg)) cfg <- config::get(file = here::here("config.yml"))

  strategies <- build_search_strategies(abstract_row, cfg)
  all_results <- list()

  for (strategy_name in names(strategies)) {
    query <- strategies[[strategy_name]]
    result <- rate_limited_search(query, retmax = cfg$pubmed$max_results_per_query, cfg = cfg)

    if (!is.null(result) && length(result$ids) > 0) {
      all_results <- c(all_results, list(tibble::tibble(
        pmid = result$ids,
        strategy = strategy_name,
        query = query,
        total_hits = result$count
      )))
    }
  }

  if (length(all_results) == 0) {
    return(tibble::tibble(
      pmid = character(0), strategy = character(0),
      query = character(0), total_hits = integer(0)
    ))
  }

  results <- bind_rows(all_results)

  # Track which strategies found each PMID
  results |>
    group_by(pmid) |>
    summarise(
      strategies = paste(strategy, collapse = "; "),
      n_strategies = n(),
      first_query = first(query),
      .groups = "drop"
    )
}
