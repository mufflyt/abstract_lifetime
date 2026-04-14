# 01b_parse_web.R — Web scraping from JMIG/ScienceDirect (preferred approach)
# Parses the ScienceDirect supplement listing page which provides:
#   - Title, authors, page range, DOI, article subtype per item
# Then fetches individual article pages for structured abstract sections.

library(here)
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(cli)

cfg <- config::get(file = here("config.yml"))

#' Scrape one page of the supplement listing
#' Returns a list: items (XML nodesets), has_next (logical), next_url (character)
scrape_listing_page <- function(url) {
  cli_alert_info("Fetching: {url}")
  resp <- httr::GET(url, httr::timeout(30),
                    httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"),
                    httr::add_headers(Accept = "text/html,application/xhtml+xml",
                                      `Accept-Language` = "en-US,en;q=0.9"))

  if (httr::status_code(resp) != 200) {
    cli_alert_warning("HTTP {status_code(resp)} from {url}")
    return(list(items = list(), has_next = FALSE, next_url = NULL))
  }

  page <- rvest::read_html(httr::content(resp, "text", encoding = "UTF-8"))
  items <- page |> html_elements("li.js-article-list-item")

  # Check for pagination "next" link
  next_link <- page |> html_element("a.next-link, a[aria-label='Next'], li.pagination-link--next a") |>
    html_attr("href")
  has_next <- !is.na(next_link) && nchar(next_link) > 0
  if (has_next && !str_starts(next_link, "http")) {
    next_link <- paste0("https://www.sciencedirect.com", next_link)
  }

  list(items = items, has_next = has_next, next_url = next_link)
}

#' Parse a single article list item from ScienceDirect
parse_sd_item <- function(element) {
  # Subtype (Conference abstract, Contents list, etc.)
  subtype <- element |>
    html_element("span.js-article-subtype") |>
    html_text(trim = TRUE)

  # Title
  title <- element |>
    html_element("a.article-content-title span.js-article-title") |>
    html_text(trim = TRUE)

  # Authors
  authors_raw <- element |>
    html_element("dd.js-article-author-list div.js-article__item__authors") |>
    html_text(trim = TRUE)
  if (is.na(authors_raw)) {
    authors_raw <- element |>
      html_element("dd.js-article-author-list") |>
      html_text(trim = TRUE)
  }

  # Page range
  page_range <- element |>
    html_element("dd.js-article-page-range") |>
    html_text(trim = TRUE)

  # DOI (hidden div)
  doi <- element |>
    html_element("div[hidden]") |>
    html_text(trim = TRUE)

  # Article URL
  link <- element |>
    html_element("a.article-content-title") |>
    html_attr("href")
  if (!is.na(link) && !str_starts(link, "http")) {
    link <- paste0("https://www.sciencedirect.com", link)
  }

  # PDF URL
  pdf_link <- element |>
    html_element("a.pdf-download") |>
    html_attr("href")

  tibble::tibble(
    subtype = subtype %||% NA_character_,
    title = str_squish(title %||% ""),
    authors_raw = str_squish(authors_raw %||% ""),
    page_range = str_squish(page_range %||% ""),
    doi = str_squish(doi %||% ""),
    article_url = link %||% NA_character_,
    pdf_url = if (!is.na(pdf_link) && !str_starts(pdf_link, "http"))
      paste0("https://www.sciencedirect.com", pdf_link) else pdf_link
  )
}

#' Extract ScienceDirect PII from article URL for cache keying
sd_pii_from_url <- function(article_url) {
  if (is.na(article_url)) return(NA_character_)
  m <- stringr::str_match(article_url, "/pii/([A-Za-z0-9]+)")
  if (is.na(m[1, 2])) return(NA_character_)
  m[1, 2]
}

#' Fetch HTML with disk caching + retry/backoff. Returns HTML text or NA.
fetch_sd_html_cached <- function(article_url, cache_dir, polite_sleep = 2,
                                 max_retries = 3) {
  pii <- sd_pii_from_url(article_url)
  if (is.na(pii)) return(NA_character_)
  cache_file <- file.path(cache_dir, paste0(pii, ".html"))

  # Cache hit — no network call
  if (file.exists(cache_file) && file.info(cache_file)$size > 5000) {
    return(readr::read_file(cache_file))
  }

  ua_pool <- c(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36"
  )

  for (attempt in seq_len(max_retries)) {
    Sys.sleep(polite_sleep + stats::runif(1, 0, 1.5))
    resp <- tryCatch(
      httr::GET(article_url, httr::timeout(30),
                httr::user_agent(sample(ua_pool, 1)),
                httr::add_headers(Accept = "text/html,application/xhtml+xml",
                                  `Accept-Language` = "en-US,en;q=0.9",
                                  Referer = "https://www.sciencedirect.com/")),
      error = function(e) NULL
    )
    if (is.null(resp)) {
      Sys.sleep(2^attempt)
      next
    }
    sc <- httr::status_code(resp)
    if (sc == 200) {
      html_txt <- httr::content(resp, "text", encoding = "UTF-8")
      # Guard against "Verify you are human" challenge pages
      if (nchar(html_txt) < 5000 || stringr::str_detect(tolower(html_txt),
          "verify you are human|just a moment|captcha")) {
        Sys.sleep(2^attempt * 5)
        next
      }
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
      readr::write_file(html_txt, cache_file)
      return(html_txt)
    }
    # Rate-limited / blocked → exponential backoff
    if (sc %in% c(403, 429, 503)) {
      Sys.sleep(2^attempt * 10)
      next
    }
    # Other non-200 — one more try
    Sys.sleep(2^attempt)
  }
  NA_character_
}

#' Fetch structured abstract text from an individual article page
fetch_article_abstract <- function(article_url, cache_dir = here::here("data", "cache", "sd_html")) {
  if (is.na(article_url)) return(list(sections = list(), abstract_full = NA_character_))

  html_txt <- fetch_sd_html_cached(article_url, cache_dir)
  if (is.na(html_txt)) return(list(sections = list(), abstract_full = NA_character_))

  tryCatch({
    page <- rvest::read_html(html_txt)

    sections <- list()

    # Prefer the structured container. Candidates in priority order.
    abstract_el <- NULL
    for (sel in c("div.abstract.author", "section#abstracts", "div#abstracts", "div.abstract")) {
      node <- html_element(page, sel)
      if (!is.na(node) && length(html_elements(node, "h3, h4")) > 0) {
        abstract_el <- node
        break
      }
      if (is.null(abstract_el) && !is.na(node)) abstract_el <- node
    }

    if (!is.null(abstract_el) && !is.na(abstract_el)) {
      headings <- abstract_el |> html_elements("h3, h4")
      if (length(headings) > 0) {
        for (h in headings) {
          heading_text <- html_text(h, trim = TRUE)
          # ScienceDirect wraps body in <div class="u-margin-s-bottom"> after <h3>,
          # but fall back to <p> or any first non-heading sibling for other layouts.
          body_text <- NA_character_
          for (xp in c("following-sibling::div[1]", "following-sibling::p[1]",
                       "following-sibling::*[not(self::h3) and not(self::h4)][1]")) {
            sib <- html_elements(h, xpath = xp)
            if (length(sib) > 0) {
              txt <- str_squish(html_text(sib[[1]], trim = TRUE))
              if (nchar(txt) > 10) { body_text <- txt; break }
            }
          }
          # Last resort: parent contains "Heading<body>" inline — strip heading from full text
          if (is.na(body_text)) {
            parent <- html_element(h, xpath = "..")
            if (!is.na(parent)) {
              parent_txt <- str_squish(html_text(parent, trim = TRUE))
              stripped <- str_replace(parent_txt, fixed(heading_text), "")
              if (nchar(stripped) > 10) body_text <- str_squish(stripped)
            }
          }
          if (!is.na(body_text)) sections[[heading_text]] <- body_text
        }
      }

      # Also try <dt>/<dd> pattern
      if (length(sections) == 0) {
        dts <- abstract_el |> html_elements("dt")
        dds <- abstract_el |> html_elements("dd")
        if (length(dts) > 0 && length(dts) == length(dds)) {
          for (j in seq_along(dts)) {
            heading_text <- html_text(dts[[j]], trim = TRUE)
            body_text <- html_text(dds[[j]], trim = TRUE)
            sections[[heading_text]] <- str_squish(body_text)
          }
        }
      }
    }

    # Full abstract text fallback
    abstract_full <- page |>
      html_element("div.abstract.author, div.abstract, section#abstracts") |>
      html_text(trim = TRUE)

    list(
      sections = sections,
      abstract_full = str_squish(abstract_full %||% NA_character_)
    )
  }, error = function(e) {
    cli_alert_warning("Error fetching {article_url}: {e$message}")
    list(sections = list(), abstract_full = NA_character_)
  })
}

# ============================================================
# Main execution
# ============================================================

cli_h2("Web Scraping: JMIG 2023 Supplement")

# ---- Short-circuit: skip scraping if we already have a complete parsed CSV ----
parsed_path <- here("data", "processed", "abstracts_parsed_web.csv")
min_complete_n <- 80  # AAGL 2023 supplement has ~98 oral abstracts
min_section_coverage <- 0.50  # sections may be sparse from JS rendering
skip_scrape <- FALSE
if (file.exists(parsed_path)) {
  existing <- tryCatch(readr::read_csv(parsed_path, show_col_types = FALSE), error = function(e) NULL)
  if (!is.null(existing) && nrow(existing) >= min_complete_n) {
    cli_alert_success("Existing parsed CSV has {nrow(existing)} rows — skipping scrape")
    readr::write_csv(existing, here("data", "processed", "abstracts_parsed.csv"))
    skip_scrape <- TRUE
  }
}

if (!skip_scrape) {

# Scrape listing page
# Note: ScienceDirect supplement pages may return all items on first page;
# offset parameter is tried but dedup guards against false pagination.
all_items <- list()
base_url <- cfg$sources$sciencedirect_url

result <- scrape_listing_page(base_url)
all_items <- result$items
cli_alert_info("First page: {length(all_items)} items")

# Try offset pagination only if first page returned exactly 100 items
if (length(all_items) == 100) {
  for (offset in seq(100, 600, by = 100)) {
    Sys.sleep(2)
    url <- paste0(base_url, "?offset=", offset)
    result <- scrape_listing_page(url)
    if (length(result$items) == 0) break

    # Check if this is genuinely new content by comparing first item
    new_first_title <- result$items[[1]] |>
      html_element("a.article-content-title") |>
      html_text(trim = TRUE)
    existing_first_title <- all_items[[1]] |>
      html_element("a.article-content-title") |>
      html_text(trim = TRUE)

    if (!is.na(new_first_title) && new_first_title == existing_first_title) {
      cli_alert_info("Offset={offset} returns same content — no true pagination")
      break
    }

    cli_alert_info("Offset={offset}: {length(result$items)} new items")
    all_items <- c(all_items, result$items)
    if (length(result$items) < 100) break
  }
}

cli_alert_success("Total items from listing: {length(all_items)}")

if (length(all_items) == 0) {
  cli_alert_danger("Could not scrape abstracts from ScienceDirect")
  cli_alert_info("Falling back to PDF parsing")
} else {
  # Parse all items
  cli_alert_info("Parsing listing items...")
  all_parsed <- map_dfr(all_items, parse_sd_item)

  cli_alert_info("Item subtypes: {paste(unique(all_parsed$subtype), collapse=', ')}")
  cli_alert_info("Total items: {nrow(all_parsed)}")

  # Filter to conference abstracts only and dedup by DOI/title
  abstracts_listing <- all_parsed |>
    filter(
      str_detect(tolower(subtype), "abstract|conference") | is.na(subtype),
      nchar(title) > 10,
      !str_detect(tolower(title), "^toc$|^cover|^board|^editorial|^international societies")
    ) |>
    distinct(doi, .keep_all = TRUE) |>  # Dedup by DOI
    distinct(title, .keep_all = TRUE) |>  # Fallback dedup by title
    mutate(abstract_id = sprintf("AAGL2023_%03d", row_number()))

  cli_alert_success("{nrow(abstracts_listing)} conference abstracts identified")

  # Fetch full abstract text for each article
  cli_alert_info("Fetching structured abstract text from individual pages...")
  full_details <- map(seq_len(nrow(abstracts_listing)), function(i) {
    if (i %% 20 == 0) cli_alert_info("  Progress: {i}/{nrow(abstracts_listing)}")
    fetch_article_abstract(abstracts_listing$article_url[i])
  })

  # Helper to safely extract a section by trying multiple heading names
  get_section <- function(detail, ...) {
    keys <- c(...)
    for (k in keys) {
      val <- detail$sections[[k]]
      if (!is.null(val) && nchar(val) > 0) return(val)
    }
    NA_character_
  }

  # Build final dataframe
  abstracts_df <- abstracts_listing |>
    mutate(
      abstract_objective = map_chr(full_details, ~ get_section(.x,
        "Objective", "Study Objective", "Objectives", "Study Objectives",
        "Background", "Purpose", "Aim")),
      abstract_design = map_chr(full_details, ~ get_section(.x,
        "Design", "Study Design")),
      abstract_setting = map_chr(full_details, ~ get_section(.x,
        "Setting", "Settings")),
      abstract_patients_participants = map_chr(full_details, ~ get_section(.x,
        "Patients", "Participants", "Patients/Participants",
        "Patients or Participants", "Subjects")),
      abstract_intervention = map_chr(full_details, ~ get_section(.x,
        "Intervention", "Interventions")),
      abstract_measurements = map_chr(full_details, ~ get_section(.x,
        "Measurements and Main Results", "Measurements", "Main Results",
        "Results", "Findings")),
      abstract_conclusion = map_chr(full_details, ~ get_section(.x,
        "Conclusion", "Conclusions")),
      abstract_full_text = map_chr(full_details, ~ .x$abstract_full %||% NA_character_),
      source = "sciencedirect",
      parse_timestamp = Sys.time()
    )

  # Report data quality
  n_with_obj <- sum(!is.na(abstracts_df$abstract_objective))
  n_with_conc <- sum(!is.na(abstracts_df$abstract_conclusion))
  n_with_full <- sum(!is.na(abstracts_df$abstract_full_text) & nchar(abstracts_df$abstract_full_text) > 20)
  n_with_auth <- sum(nchar(abstracts_df$authors_raw) > 2)

  cli_h3("Data Quality")
  cli_alert_info("Abstracts with authors: {n_with_auth}/{nrow(abstracts_df)}")
  cli_alert_info("Abstracts with Objective: {n_with_obj}/{nrow(abstracts_df)}")
  cli_alert_info("Abstracts with Conclusion: {n_with_conc}/{nrow(abstracts_df)}")
  cli_alert_info("Abstracts with full text: {n_with_full}/{nrow(abstracts_df)}")

  # Save
  out_path <- here("data", "processed", "abstracts_parsed_web.csv")
  write_csv(abstracts_df, out_path)
  cli_alert_success("Saved {nrow(abstracts_df)} abstracts to {out_path}")

  # Set as primary
  write_csv(abstracts_df, here("data", "processed", "abstracts_parsed.csv"))
  cli_alert_info("Set as primary parsed file")
}
}  # end if (!skip_scrape)
