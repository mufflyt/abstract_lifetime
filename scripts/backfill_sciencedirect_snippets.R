#!/usr/bin/env Rscript
# backfill_sciencedirect_snippets.R
#
# Fetches "Section snippets" from ScienceDirect /abs/pii/ pages for 2012-2018
# abstracts that are missing abstract_text. These snippets are publicly visible
# (no login required) and contain the structured abstract sections.
#
# Pipeline:
#   1. Find abstracts missing text with a DOI
#   2. Resolve DOI → PII via CrossRef
#   3. Fetch https://www.sciencedirect.com/science/article/abs/pii/{PII}
#   4. Parse Section snippets from HTML
#   5. Patch abstracts_cleaned.csv in place

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(stringr)
  library(httr); library(rvest); library(purrr); library(cli)
})

abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")
cache_dir      <- here("data", "cache", "sd_html")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

abstracts <- read_csv(abstracts_path, show_col_types = FALSE)

missing <- abstracts |>
  filter(is.na(abstract_text) | nchar(abstract_text) < 10) |>
  filter(!is.na(doi) & nchar(doi) > 5)

cli_h2("ScienceDirect Section-Snippet Backfill")
cli_alert_info("{nrow(missing)} abstracts need backfill")
if (nrow(missing) == 0) { cli_alert_success("Nothing to do"); quit(save = "no") }

# ── Step 1: Resolve DOI → PII via CrossRef ────────────────────────────────────
get_pii_from_crossref <- function(doi) {
  doi_bare <- str_replace(doi, "^https?://doi\\.org/", "")
  url <- paste0("https://api.crossref.org/works/", URLencode(doi_bare, repeated = TRUE),
                "?mailto=", Sys.getenv("PIPELINE_EMAIL", "abstract.lifetime@example.com"))
  resp <- tryCatch(
    httr::GET(url, httr::timeout(15)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) return(NA_character_)
  body <- tryCatch(httr::content(resp, "parsed", simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(body)) return(NA_character_)
  links <- body$message$link
  if (is.null(links) || !is.data.frame(links)) return(NA_character_)
  # Extract PII from Elsevier TDM link URL
  pii_link <- links$URL[str_detect(links$URL, "PII:")]
  if (length(pii_link) == 0) {
    # Fall back to resource primary URL
    resource_url <- body$message$resource$primary$URL %||% ""
    m <- str_match(resource_url, "/pii/([A-Za-z0-9]+)")
    if (!is.na(m[1, 2])) return(m[1, 2])
    return(NA_character_)
  }
  m <- str_match(pii_link[1], "PII:([A-Za-z0-9]+)")
  if (!is.na(m[1, 2])) m[1, 2] else NA_character_
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# ── Step 2: Fetch & parse Section snippets from ScienceDirect /abs/pii/ ───────
ua_pool <- c(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.3 Safari/605.1.15",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
)

fetch_snippets <- function(pii) {
  if (is.na(pii)) return(NA_character_)
  cache_file <- file.path(cache_dir, paste0(pii, ".html"))

  html_txt <- if (file.exists(cache_file) && file.info(cache_file)$size > 3000) {
    readr::read_file(cache_file)
  } else {
    url <- paste0("https://www.sciencedirect.com/science/article/abs/pii/", pii)
    Sys.sleep(runif(1, 2, 4))
    resp <- tryCatch(
      httr::GET(url, httr::timeout(30),
                httr::user_agent(sample(ua_pool, 1)),
                httr::add_headers(
                  Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                  `Accept-Language` = "en-US,en;q=0.9",
                  Referer = "https://www.sciencedirect.com/"
                )),
      error = function(e) NULL
    )
    if (is.null(resp) || httr::status_code(resp) != 200) return(NA_character_)
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    if (nchar(txt) < 3000 || str_detect(tolower(txt), "just a moment|captcha|verify you are human"))
      return(NA_character_)
    readr::write_file(txt, cache_file)
    txt
  }

  parse_snippets(html_txt)
}

parse_snippets <- function(html_txt) {
  if (is.na(html_txt) || nchar(html_txt) < 500) return(NA_character_)
  page <- tryCatch(rvest::read_html(html_txt), error = function(e) NULL)
  if (is.null(page)) return(NA_character_)

  # Priority 1: full structured abstract container
  for (sel in c("div.abstract.author", "section#abstracts", "div#abstracts", "div.abstract")) {
    node <- html_element(page, sel)
    if (!is.na(node)) {
      txt <- str_squish(html_text(node, trim = TRUE))
      if (nchar(txt) > 50) return(txt)
    }
  }

  # Priority 2: id="preview-section-snippets" — headings + sibling text
  skip_headings <- c("section snippets", "references", "cited by",
                     "recommended articles", "about sciencedirect")
  snippets_node <- html_element(page, "#preview-section-snippets")
  if (!is.na(snippets_node)) {
    headings  <- html_elements(snippets_node, "h2, h3, h4")
    collected <- character(0)
    for (h in headings) {
      htxt <- str_squish(html_text(h, trim = TRUE))
      if (any(str_detect(tolower(htxt), fixed(skip_headings)))) next
      body_nodes <- html_elements(h, xpath =
        "following-sibling::*[not(self::h2) and not(self::h3) and not(self::h4)][position()<=3]")
      body <- str_squish(paste(vapply(body_nodes,
        function(n) str_squish(html_text(n, trim = TRUE)), character(1)), collapse = " "))
      if (nchar(body) > 10) collected <- c(collected, paste0(htxt, ": ", body))
    }
    if (length(collected) > 0) return(paste(collected, collapse = " "))
  }

  # Priority 3: <dt>/<dd> pairs anywhere
  dts <- html_elements(page, "dt")
  dds <- html_elements(page, "dd")
  if (length(dts) > 0 && length(dts) == length(dds)) {
    parts <- mapply(function(dt, dd) {
      h <- str_squish(html_text(dt, trim = TRUE))
      b <- str_squish(html_text(dd, trim = TRUE))
      if (nchar(h) > 0 && nchar(b) > 10) paste0(h, ": ", b) else NA_character_
    }, dts, dds, USE.NAMES = FALSE)
    parts <- parts[!is.na(parts)]
    if (length(parts) > 0) return(paste(parts, collapse = " "))
  }

  NA_character_
}

# ── Main loop ─────────────────────────────────────────────────────────────────
n_total  <- nrow(missing)
n_found  <- 0L
results  <- vector("list", n_total)

for (i in seq_len(n_total)) {
  row <- missing[i, ]

  # Get PII
  pii <- get_pii_from_crossref(row$doi)
  Sys.sleep(0.5)  # polite CrossRef rate limit

  # Fetch & parse snippets
  txt <- fetch_snippets(pii)

  results[[i]] <- tibble(abstract_id = row$abstract_id, abstract_text_new = txt)
  if (!is.na(txt)) n_found <- n_found + 1L

  if (i %% 10 == 0 || !is.na(txt))
    cli_alert_info("[{i}/{n_total}] {row$abstract_id} pii={pii %||% 'NA'} found={!is.na(txt)} (total={n_found})")
}

backfill <- bind_rows(results) |>
  filter(!is.na(abstract_text_new) & nchar(abstract_text_new) > 10)

cli_alert_success("Retrieved text for {nrow(backfill)} / {n_total} abstracts")

if (nrow(backfill) == 0) {
  cli_alert_warning("No text retrieved — ScienceDirect may be blocking; try again later")
  quit(save = "no")
}

# ── Patch abstracts_cleaned.csv ───────────────────────────────────────────────
abstracts_patched <- abstracts |>
  left_join(backfill, by = "abstract_id") |>
  mutate(
    abstract_text = if_else(
      !is.na(abstract_text_new) & (is.na(abstract_text) | nchar(abstract_text) < 10),
      abstract_text_new, abstract_text
    )
  ) |>
  select(-abstract_text_new)

write_csv(abstracts_patched, abstracts_path)
cli_alert_success("Patched abstracts_cleaned.csv ({nrow(backfill)} rows updated)")

coverage <- abstracts_patched |>
  filter(congress_year <= 2018) |>
  group_by(congress_year) |>
  summarise(n = n(),
            has_text = sum(!is.na(abstract_text) & nchar(abstract_text) > 10),
            pct = round(has_text / n * 100, 1), .groups = "drop")
print(coverage)
