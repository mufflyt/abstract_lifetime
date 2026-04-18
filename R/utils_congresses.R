# utils_congresses.R — helpers for multi-congress (year-aware) pipeline steps.

#' @title Build a Congress-Year to Conference-Date Lookup Table
#'
#' @description
#' Converts the \code{congresses} list in a config object into a named
#' \code{Date} vector keyed by congress year (as a character string). Supports
#' multi-cohort pipelines that span several AAGL annual meetings.
#'
#' @param cfg List. A parsed config object as returned by
#'   \code{config::get(file = here::here("config.yml"))}. Must contain either
#'   a \code{congresses} list (each element with \code{year} and \code{date}
#'   fields) or a legacy \code{conference$date} scalar.
#'
#' @return Named \code{Date} vector. Names are congress years as character
#'   strings (e.g., \code{"2022"}, \code{"2023"}); values are the
#'   corresponding conference start dates.
#'
#' @details
#' When \code{cfg$congresses} is present and non-empty, one entry is created
#' per congress. Otherwise the function falls back to the legacy single-date
#' format (\code{cfg$conference$date}), producing a length-1 vector keyed by
#' the year extracted from that date. This backward-compatible fallback lets
#' earlier pipeline steps that pre-date the multi-congress redesign continue
#' to work without modification.
#'
#' @examples
#' \dontrun{
#' cfg <- config::get(file = here::here("config.yml"))
#' lkp <- congress_date_lookup(cfg)
#' lkp["2023"]  # Date: 2023-11-07
#' }
#'
#' @seealso \code{\link{conference_date_for}}
#' @export
congress_date_lookup <- function(cfg) {
  if (!is.null(cfg$congresses) && length(cfg$congresses) > 0) {
    years <- vapply(cfg$congresses, function(c) as.integer(c$year), integer(1))
    dates <- as.Date(vapply(cfg$congresses, function(c) as.character(c$date), character(1)))
    setNames(dates, as.character(years))
  } else {
    setNames(as.Date(cfg$conference$date), as.character(lubridate::year(as.Date(cfg$conference$date))))
  }
}

#' @title Return the Conference Date for Each Row's Congress Year
#'
#' @description
#' Vectorized lookup that maps a vector of congress year values to their
#' corresponding conference \code{Date} objects. Rows with a missing or
#' unrecognized congress year fall back to the legacy
#' \code{cfg$conference$date} value.
#'
#' @param congress_year Integer or character vector. Congress year(s) for each
#'   abstract row (e.g., \code{2022L}, \code{2023L}). May contain \code{NA}.
#' @param cfg List. Parsed config object (see \code{\link{congress_date_lookup}}).
#'
#' @return \code{Date} vector of the same length as \code{congress_year}.
#'   Elements for recognized years receive the exact congress date; elements
#'   for missing/unknown years receive the legacy fallback date
#'   (\code{2023-11-07} if \code{cfg$conference$date} is also absent).
#'
#' @examples
#' \dontrun{
#' cfg <- config::get(file = here::here("config.yml"))
#' conference_date_for(c(2022L, 2023L, NA_integer_), cfg)
#' }
#'
#' @seealso \code{\link{congress_date_lookup}}
#' @export
conference_date_for <- function(congress_year, cfg) {
  lkp <- congress_date_lookup(cfg)
  legacy <- as.Date(cfg$conference$date %||% "2023-11-07")
  out <- lkp[as.character(congress_year)]
  out[is.na(out)] <- legacy
  unname(out)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
