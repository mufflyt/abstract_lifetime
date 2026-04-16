#!/usr/bin/env Rscript
# Rescue the 2016 cohort after its listing page hit HTTP 429 during scrape.
# Fetches the listing + per-article pages, tags session type, and appends to
# existing parsed CSVs without overwriting other years.

suppressPackageStartupMessages({
  library(here); library(rvest); library(xml2); library(httr)
  library(dplyr); library(stringr); library(readr); library(purrr); library(cli)
  library(tibble)
})

YEAR       <- 2016
URL        <- "https://www.sciencedirect.com/journal/journal-of-minimally-invasive-gynecology/vol/23/issue/7/suppl/S"
ua         <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
cache_dir  <- here("data", "cache", "sd_html")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# --- Fetch listing with backoff on 429 ---
fetch_listing <- function(url, max_attempts = 6) {
  for (i in seq_len(max_attempts)) {
    Sys.sleep(5 * i)
    r <- tryCatch(httr::GET(url, httr::timeout(30), httr::user_agent(ua)),
                  error = function(e) NULL)
    if (is.null(r)) next
    sc <- status_code(r)
    cli_alert_info("Attempt {i}: HTTP {sc}")
    if (sc == 200) return(content(r, "text", encoding = "UTF-8"))
    if (sc == 429) Sys.sleep(30 * i)
  }
  stop("Failed to fetch listing after ", max_attempts, " attempts")
}

cli_h2("Rescue AAGL 2016 listing")
html <- fetch_listing(URL)
page <- read_html(html)
items <- html_elements(page, "li.js-article-list-item")
cli_alert_success("Listing items: {length(items)}")

# Replicate parse_sd_item inline
parse_sd_item <- function(el) {
  st <- html_text(html_element(el, "span.js-article-subtype"), trim = TRUE)
  ti <- html_text(html_element(el, "a.article-content-title span.js-article-title"), trim = TRUE)
  au <- html_text(html_element(el, "dd.js-article-author-list div.js-article__item__authors"), trim = TRUE)
  if (is.na(au)) au <- html_text(html_element(el, "dd.js-article-author-list"), trim = TRUE)
  pr <- html_text(html_element(el, "dd.js-article-page-range"), trim = TRUE)
  doi <- html_text(html_element(el, "div[hidden]"), trim = TRUE)
  lk <- html_attr(html_element(el, "a.article-content-title"), "href")
  if (!is.na(lk) && !str_starts(lk, "http")) lk <- paste0("https://www.sciencedirect.com", lk)
  pdf_lk <- html_attr(html_element(el, "a.pdf-download"), "href")
  tibble(
    subtype = st, title = str_squish(ti %||% ""),
    authors_raw = str_squish(au %||% ""),
    page_range = str_squish(pr %||% ""),
    doi = str_squish(doi %||% ""),
    article_url = lk %||% NA_character_,
    pdf_url = if (!is.na(pdf_lk) && !str_starts(pdf_lk, "http"))
      paste0("https://www.sciencedirect.com", pdf_lk) else pdf_lk
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

parsed <- map_dfr(items, parse_sd_item) |>
  filter(
    str_detect(tolower(subtype), "abstract|conference") | is.na(subtype),
    nchar(title) > 10,
    !str_detect(tolower(title), "^toc$|^cover|^board|^editorial|^international societies|^officers|^committees")
  ) |>
  distinct(doi, .keep_all = TRUE) |>
  distinct(title, .keep_all = TRUE) |>
  mutate(
    congress_year = YEAR,
    abstract_id = sprintf("AAGL%d_%03d", YEAR, row_number())
  )

cli_alert_success("Kept {nrow(parsed)} abstracts")

# --- Fetch per-article HTML (with cache + polite delay) ---
sd_pii <- function(u) str_match(u, "/pii/([A-Za-z0-9]+)")[, 2]
fetch_cached <- function(u, max_attempts = 3) {
  pii <- sd_pii(u); cache_file <- file.path(cache_dir, paste0(pii, ".html"))
  if (file.exists(cache_file) && file.info(cache_file)$size > 5000)
    return(read_file(cache_file))
  for (i in seq_len(max_attempts)) {
    Sys.sleep(2 + runif(1, 0, 1.5))
    r <- tryCatch(httr::GET(u, httr::timeout(30), httr::user_agent(ua),
      httr::add_headers(Accept = "text/html", Referer = "https://www.sciencedirect.com/")),
      error = function(e) NULL)
    if (!is.null(r) && status_code(r) == 200) {
      txt <- content(r, "text", encoding = "UTF-8")
      if (nchar(txt) > 5000) { write_file(txt, cache_file); return(txt) }
    }
    Sys.sleep(5 * i)
  }
  NA_character_
}

cli_alert_info("Fetching {nrow(parsed)} article pages...")
full_details <- lapply(seq_len(nrow(parsed)), function(i) {
  html_txt <- fetch_cached(parsed$article_url[i])
  if (i %% 10 == 0) cli_alert_info("  {i}/{nrow(parsed)}")
  if (is.na(html_txt)) return(list(sections = list(), abstract_full = NA_character_))
  tryCatch({
    pg <- read_html(html_txt)
    abstract_el <- html_element(pg, "div.abstract.author, section#abstracts, div#abstracts, div.abstract")
    sections <- list()
    if (!is.na(abstract_el)) {
      for (h in html_elements(abstract_el, "h3, h4")) {
        heading <- html_text(h, trim = TRUE)
        body <- NA_character_
        for (xp in c("following-sibling::div[1]", "following-sibling::p[1]")) {
          sib <- html_elements(h, xpath = xp)
          if (length(sib) > 0) {
            txt <- str_squish(html_text(sib[[1]], trim = TRUE))
            if (nchar(txt) > 10) { body <- txt; break }
          }
        }
        if (!is.na(body)) sections[[heading]] <- body
      }
    }
    full <- str_squish(html_text(abstract_el, trim = TRUE) %||% NA_character_)
    list(sections = sections, abstract_full = full)
  }, error = function(e) list(sections = list(), abstract_full = NA_character_))
})

get_section <- function(d, ...) {
  for (k in c(...)) {
    v <- d$sections[[k]]
    if (!is.null(v) && nchar(v) > 0) return(v)
  }
  NA_character_
}

parsed <- parsed |> mutate(
  abstract_objective = map_chr(full_details, ~ get_section(.x,
    "Objective", "Study Objective", "Objectives", "Study Objectives",
    "Background", "Purpose", "Aim")),
  abstract_design = map_chr(full_details, ~ get_section(.x, "Design", "Study Design")),
  abstract_setting = map_chr(full_details, ~ get_section(.x, "Setting", "Settings")),
  abstract_patients_participants = map_chr(full_details, ~ get_section(.x,
    "Patients", "Participants", "Patients/Participants", "Patients or Participants", "Subjects")),
  abstract_intervention = map_chr(full_details, ~ get_section(.x, "Intervention", "Interventions")),
  abstract_measurements = map_chr(full_details, ~ get_section(.x,
    "Measurements and Main Results", "Measurements", "Main Results", "Results", "Findings")),
  abstract_conclusion = map_chr(full_details, ~ get_section(.x, "Conclusion", "Conclusions")),
  abstract_full_text = map_chr(full_details, ~ .x$abstract_full %||% NA_character_),
  source = "sciencedirect",
  parse_timestamp = Sys.time(),
  session_type = "Oral"   # AAGL 2016 supplement is all-Oral
)

# --- Merge into existing parsed_web.csv ---
parsed_path <- here("data", "processed", "abstracts_parsed_web.csv")
existing <- read_csv(parsed_path, show_col_types = FALSE)
# Drop any stale 2016 rows
existing <- existing |> filter(!(congress_year %in% YEAR))
# Align columns
for (col in setdiff(names(existing), names(parsed))) parsed[[col]] <- NA
parsed <- parsed[, names(existing), drop = FALSE]
combined <- bind_rows(existing, parsed) |> arrange(congress_year, abstract_id)
write_csv(combined, parsed_path)
write_csv(combined, here("data", "processed", "abstracts_parsed.csv"))
cli_alert_success("Merged: {nrow(combined)} total abstracts across {length(unique(combined$congress_year))} congresses")
