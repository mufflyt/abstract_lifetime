# 01d_tag_session_type.R — Assign AAGL session type (Oral vs Video) to each
# abstract by walking the JMIG supplement TOC HTML in DOM order and pairing
# every <li.js-article-list-item> with the most recent preceding section-title
# heading. Merges the result into abstracts_parsed_web.csv, abstracts_cleaned.csv,
# and abstracts_with_matches.csv so downstream analysis can stratify by session.

suppressPackageStartupMessages({
  library(here)
  library(rvest)
  library(xml2)
  library(httr)
  library(dplyr)
  library(stringr)
  library(readr)
  library(cli)
  library(tibble)
  library(purrr)
})

cfg <- config::get(file = here("config.yml"))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

cli_h2("Tagging session type from JMIG supplement TOC(s)")

congresses <- cfg$congresses %||% list(list(
  year = 2023,
  sciencedirect_url = cfg$sources$sciencedirect_url
))

#' Scrape session types (Oral/Video) from a ScienceDirect supplement TOC
#'
#' @param toc_url Character. ScienceDirect supplement URL for one congress.
#' @param year Integer. Congress year.
#' @return Tibble with columns \code{pii}, \code{congress_year}, \code{session_type}.
#' @keywords internal
tag_one_congress <- function(toc_url, year) {
  cli_alert_info("Congress {year} -> {toc_url}")
  resp <- httr::GET(toc_url, httr::timeout(30),
    httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"))
  if (status_code(resp) != 200) {
    cli_alert_danger("HTTP {status_code(resp)} — skipping {year}")
    return(tibble())
  }
  page <- read_html(httr::content(resp, "text", encoding = "UTF-8"))
  nodes <- xml_find_all(page, "//h3[contains(@class, 'section-title')] | //li[contains(@class, 'js-article-list-item')]")

  current_section <- NA_character_
  rows <- list()
  for (n in nodes) {
    tag <- xml_name(n)
    if (tag == "h3") {
      current_section <- str_squish(xml_text(n))
      cli_alert_info("  Section: '{current_section}'")
      next
    }
    href <- xml_attr(xml_find_first(n, ".//a[contains(@class, 'article-content-title')]"), "href")
    if (is.na(href)) next
    pii <- str_match(href, "/pii/([A-Za-z0-9]+)")[, 2]
    if (is.na(pii)) next
    rows[[length(rows) + 1]] <- tibble(
      pii = pii, congress_year = year, session_type = current_section
    )
  }
  bind_rows(rows) |> distinct(pii, .keep_all = TRUE)
}

sessions <- purrr::map(congresses, ~ tag_one_congress(.x$sciencedirect_url, .x$year)) |>
  purrr::list_rbind()

# "Oral Presentations" / "Video Presentations" (2023), "Oral Presentations" /
# "Video Sessions" (2022) — collapse to canonical Oral / Video labels.
sessions <- sessions |>
  mutate(
    session_type = case_when(
      str_detect(tolower(session_type), "oral")   ~ "Oral",
      str_detect(tolower(session_type), "video")  ~ "Video",
      str_detect(tolower(session_type), "poster") ~ "Poster",
      TRUE ~ session_type
    )
  )

cli_alert_success("Tagged {nrow(sessions)} articles across {length(unique(sessions$congress_year))} congresses")
print(table(sessions$session_type, sessions$congress_year, useNA = "ifany"))

# --- Merge into parsed/cleaned/matches CSVs ------------------------------

#' Extract PII from a ScienceDirect article URL
#' @param u Character. ScienceDirect URL.
#' @return Character. PII string.
#' @keywords internal
pii_from_url <- function(u) {
  m <- str_match(u, "/pii/([A-Za-z0-9]+)")
  m[, 2]
}

#' Merge session_type tags into a pipeline CSV by PII or abstract_id
#' @param path Character. Path to the CSV to update.
#' @return Invisible NULL. Writes updated CSV in place.
#' @keywords internal
merge_session <- function(path) {
  if (!file.exists(path)) return(invisible(NULL))
  d <- read_csv(path, show_col_types = FALSE)
  join_key <- NULL
  if ("article_url" %in% names(d)) {
    d <- d |> mutate(.pii = pii_from_url(article_url))
    join_key <- ".pii"
  } else if ("abstract_id" %in% names(d)) {
    # abstracts_with_matches / cleaned don't carry article_url — join via the
    # parsed web CSV as a bridge.
    bridge <- read_csv(here("data", "processed", "abstracts_parsed_web.csv"),
                       show_col_types = FALSE) |>
      mutate(.pii = pii_from_url(article_url)) |>
      select(abstract_id, .pii)
    d <- d |> left_join(bridge, by = "abstract_id")
    join_key <- ".pii"
  } else {
    return(invisible(NULL))
  }

  if ("session_type" %in% names(d)) d$session_type <- NULL
  d <- d |>
    left_join(sessions |> select(pii, session_type), by = setNames("pii", join_key)) |>
    select(-all_of(join_key))

  write_csv(d, path)
  cli_alert_success("Updated {basename(path)} — sessions: {paste(names(table(d$session_type)), table(d$session_type), sep='=', collapse=', ')}")
}

merge_session(here("data", "processed", "abstracts_parsed_web.csv"))
merge_session(here("data", "processed", "abstracts_parsed.csv"))
merge_session(here("data", "processed", "abstracts_cleaned.csv"))
merge_session(here("output", "abstracts_with_matches.csv"))
