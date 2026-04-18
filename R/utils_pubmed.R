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

#' Rate-limited PubMed query
#' Respects NCBI rate limits (3/sec without key, 10/sec with key)
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

#' Fetch PubMed article details for a vector of PMIDs
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

#' Parse PubMed XML response into a tidy tibble
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

#' Build PubMed date range filter
build_date_filter <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- config::get(file = here::here("config.yml"))
  sprintf("%s:%s[PDAT]", cfg$pubmed$date_start, cfg$pubmed$date_end)
}

#' Filter out the JMIG supplement itself from results
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

#' Generate all 6 search strategies for an abstract
#' Returns a named list of PubMed query strings
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

#' Execute all search strategies for one abstract
#' Returns a tibble of unique candidate PMIDs with which strategy found them
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
