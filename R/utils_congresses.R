# utils_congresses.R — helpers for multi-congress (year-aware) pipeline steps.

#' Build a named lookup of congress_year -> conference Date from config.
#' Falls back to the legacy single `conference$date` if `congresses` missing.
congress_date_lookup <- function(cfg) {
  if (!is.null(cfg$congresses) && length(cfg$congresses) > 0) {
    years <- vapply(cfg$congresses, function(c) as.integer(c$year), integer(1))
    dates <- as.Date(vapply(cfg$congresses, function(c) as.character(c$date), character(1)))
    setNames(dates, as.character(years))
  } else {
    setNames(as.Date(cfg$conference$date), as.character(lubridate::year(as.Date(cfg$conference$date))))
  }
}

#' Return the conference date for each row's congress_year. If a row is missing
#' congress_year, fall back to the legacy single conference$date.
conference_date_for <- function(congress_year, cfg) {
  lkp <- congress_date_lookup(cfg)
  legacy <- as.Date(cfg$conference$date %||% "2023-11-07")
  out <- lkp[as.character(congress_year)]
  out[is.na(out)] <- legacy
  unname(out)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
