#!/usr/bin/env Rscript
# warm_sd_cache.R — One-time (resumable) ScienceDirect HTML cache warmer.
#
# ScienceDirect hosts the AAGL 2023 meeting abstracts (686 items, fixed dataset).
# This script politely fetches each article page to data/cache/sd_html/<pii>.html
# with long backoff on 403/429. Safe to Ctrl-C and re-run — already-cached PIIs
# are skipped. Once all 686 are cached, Step 1 is effectively free forever.
#
# Usage: Rscript scripts/warm_sd_cache.R [--sleep 4] [--retries 5]

suppressMessages({
  library(here); library(readr); library(dplyr); library(stringr); library(cli)
})

source(here("R", "01b_parse_web.R"))  # loads fetch_sd_html_cached + sd_pii_from_url

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  i <- which(args == name)
  if (length(i) && i < length(args)) args[i + 1] else default
}
polite_sleep <- as.numeric(get_arg("--sleep", 4))
max_retries  <- as.integer(get_arg("--retries", 5))

cache_dir <- here("data", "cache", "sd_html")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# Load existing URL list from the parsed CSV (already has every article_url)
parsed <- read_csv(here("data", "processed", "abstracts_parsed_web.csv"),
                   show_col_types = FALSE)
urls <- parsed$article_url
urls <- urls[!is.na(urls)]

cli_h1("ScienceDirect cache warmer")
cli_alert_info("Target URLs: {length(urls)}")
cli_alert_info("Cache dir: {cache_dir}")
cli_alert_info("Polite sleep: {polite_sleep}s (jittered)")

piis <- vapply(urls, sd_pii_from_url, character(1))
cached_before <- sum(file.exists(file.path(cache_dir, paste0(piis, ".html"))))
cli_alert_info("Already cached: {cached_before}/{length(urls)}")

need <- which(!file.exists(file.path(cache_dir, paste0(piis, ".html"))))
if (length(need) == 0) {
  cli_alert_success("All URLs already cached. Nothing to do.")
  quit(save = "no", status = 0)
}

n_ok <- 0L; n_fail <- 0L
t0 <- Sys.time()
for (idx in seq_along(need)) {
  i <- need[idx]
  url <- urls[i]
  html_txt <- fetch_sd_html_cached(url, cache_dir,
                                   polite_sleep = polite_sleep,
                                   max_retries = max_retries)
  if (!is.na(html_txt)) n_ok <- n_ok + 1L else n_fail <- n_fail + 1L

  if (idx %% 10 == 0 || idx == length(need)) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    rate <- idx / max(elapsed, 0.001)
    remaining <- (length(need) - idx) / max(rate, 0.001)
    cli_alert_info("Progress {idx}/{length(need)} (ok={n_ok}, fail={n_fail}) — rate {round(rate, 1)}/min, ETA ~{round(remaining)} min")
  }
}

total_cached <- sum(file.exists(file.path(cache_dir, paste0(piis, ".html"))))
cli_alert_success("Done. Cache now has {total_cached}/{length(urls)} HTMLs.")
if (total_cached < length(urls)) {
  cli_alert_warning("{length(urls) - total_cached} still missing — re-run this script later (ScienceDirect may unblock after a pause).")
}
