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

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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

    # Full abstract text fallback (structured abstract container)
    abstract_full <- page |>
      html_element("div.abstract.author, div.abstract, section#abstracts") |>
      html_text(trim = TRUE)
    abstract_full <- str_squish(abstract_full %||% NA_character_)

    # ── Section snippets fallback (older/paywalled ScienceDirect pages) ──────
    # For 2012-2018 supplement issues the full abstract is paywalled, but
    # ScienceDirect exposes "Section snippets" in the initial HTML inside
    # id="preview-section-snippets". Headings (h2/h3/h4) are followed by
    # sibling text nodes — extract them with a heading-to-next-heading scan.
    if ((is.na(abstract_full) || nchar(abstract_full) < 50) && length(sections) == 0) {

      # Primary: id="preview-section-snippets" container (ScienceDirect 2012-2023)
      snippets_node <- html_element(page, "#preview-section-snippets")
      if (!is.na(snippets_node)) {
        headings <- html_elements(snippets_node, "h2, h3, h4")
        in_snippets <- FALSE
        collected   <- character(0)
        skip_headings <- c("section snippets", "references", "cited by",
                           "recommended articles", "about sciencedirect")
        for (h in headings) {
          htxt <- str_squish(html_text(h, trim = TRUE))
          lc   <- tolower(htxt)
          if (any(str_detect(lc, fixed(skip_headings)))) next
          # Grab all text siblings until the next heading
          body_nodes <- html_elements(h, xpath =
            "following-sibling::*[not(self::h2) and not(self::h3) and not(self::h4)][position()<=3]")
          body <- paste(vapply(body_nodes, function(n)
            str_squish(html_text(n, trim = TRUE)), character(1)), collapse = " ")
          body <- str_squish(body)
          if (nchar(body) > 10) {
            sections[[htxt]] <- body
            collected <- c(collected, paste0(htxt, ": ", body))
          }
        }
        if (length(collected) > 0) abstract_full <- paste(collected, collapse = " ")
      }

      # Secondary: <dt>/<dd> pairs anywhere on the page
      if (is.na(abstract_full) || nchar(abstract_full) < 50) {
        dts <- html_elements(page, "dt")
        dds <- html_elements(page, "dd")
        if (length(dts) > 0 && length(dts) == length(dds)) {
          parts <- mapply(function(dt, dd) {
            h <- str_squish(html_text(dt, trim = TRUE))
            b <- str_squish(html_text(dd, trim = TRUE))
            if (nchar(h) > 0 && nchar(b) > 10) {
              sections[[h]] <<- b
              paste0(h, ": ", b)
            } else NA_character_
          }, dts, dds, USE.NAMES = FALSE)
          parts <- parts[!is.na(parts)]
          if (length(parts) > 0) abstract_full <- paste(parts, collapse = " ")
        }
      }

      # Tertiary: named container elements
      if (is.na(abstract_full) || nchar(abstract_full) < 50) {
        for (sel in c("dl.snippets", "div.Snippets", "section.Snippets",
                      "div[data-testid='section-snippets']")) {
          node <- html_element(page, sel)
          if (!is.na(node)) {
            txt <- str_squish(html_text(node, trim = TRUE))
            if (nchar(txt) > 50) { abstract_full <- txt; break }
          }
        }
      }
    }

    list(
      sections = sections,
      abstract_full = if (is.na(abstract_full) || nchar(abstract_full) == 0)
        NA_character_ else abstract_full
    )
  }, error = function(e) {
    cli_alert_warning("Error fetching {article_url}: {e$message}")
    list(sections = list(), abstract_full = NA_character_)
  })
}

# ============================================================
# Main execution
# ============================================================

cli_h2("Web Scraping: JMIG AAGL Supplements")

# ---- Short-circuit: skip scraping if we already have a complete parsed CSV ----
# Threshold is per-congress × n_congresses so adding a new year re-runs the scrape.
parsed_path <- here("data", "processed", "abstracts_parsed_web.csv")
min_per_congress <- 80
congresses_cfg <- cfg$congresses %||% list(list(
  year = 2023,
  sciencedirect_url = cfg$sources$sciencedirect_url
))
min_complete_n <- min_per_congress * length(congresses_cfg)
skip_scrape <- FALSE
if (file.exists(parsed_path)) {
  existing <- tryCatch(readr::read_csv(parsed_path, show_col_types = FALSE), error = function(e) NULL)
  if (!is.null(existing) && nrow(existing) >= min_complete_n &&
      "congress_year" %in% names(existing) &&
      length(setdiff(as.integer(sapply(congresses_cfg, function(c) c$year)),
                     as.integer(existing$congress_year))) == 0) {
    cli_alert_success("Existing parsed CSV has {nrow(existing)} rows covering all configured congresses — skipping scrape")
    readr::write_csv(existing, here("data", "processed", "abstracts_parsed.csv"))
    skip_scrape <- TRUE
  }
}

if (!skip_scrape) {

# Scrape each congress in turn. Pagination is rarely needed (issues return all
# items on the first page) but offset fallback is kept for safety.
scrape_one_congress <- function(base_url) {
  all_items <- list()
  result <- scrape_listing_page(base_url)
  all_items <- result$items
  cli_alert_info("  First page: {length(all_items)} items")

  if (length(all_items) == 100) {
    for (offset in seq(100, 600, by = 100)) {
      Sys.sleep(2)
      url <- paste0(base_url, "?offset=", offset)
      result <- scrape_listing_page(url)
      if (length(result$items) == 0) break
      new_first <- html_text(html_element(result$items[[1]], "a.article-content-title"), trim = TRUE)
      existing_first <- html_text(html_element(all_items[[1]], "a.article-content-title"), trim = TRUE)
      if (!is.na(new_first) && new_first == existing_first) break
      all_items <- c(all_items, result$items)
      if (length(result$items) < 100) break
    }
  }
  all_items
}

all_congress_parsed <- list()

for (cc in congresses_cfg) {
  cli_h3("Congress {cc$year}")
  items <- scrape_one_congress(cc$sciencedirect_url)
  cli_alert_success("  Total items: {length(items)}")
  if (length(items) == 0) next
  parsed <- purrr::map(items, parse_sd_item) |> purrr::list_rbind()

  listing <- parsed |>
    filter(
      str_detect(tolower(subtype), "abstract|conference") | is.na(subtype),
      nchar(title) > 10,
      !str_detect(tolower(title), "^toc$|^cover|^board|^editorial|^international societies|^officers|^committees")
    ) |>
    distinct(doi, .keep_all = TRUE) |>
    distinct(title, .keep_all = TRUE) |>
    mutate(
      congress_year = cc$year,
      abstract_id = sprintf("AAGL%d_%03d", cc$year, row_number())
    )

  cli_alert_success("  {nrow(listing)} conference abstracts")
  all_congress_parsed[[as.character(cc$year)]] <- listing
}

if (length(all_congress_parsed) == 0) {
  cli_alert_danger("Could not scrape abstracts from ScienceDirect")
  cli_alert_info("Falling back to PDF parsing")
} else {
  abstracts_listing <- bind_rows(all_congress_parsed)
  cli_alert_success("Combined listing: {nrow(abstracts_listing)} abstracts across {length(all_congress_parsed)} congresses")

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
  n_with_auth <- sum(nchar(abstracts_df$authors_raw) > 2, na.rm = TRUE)

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
