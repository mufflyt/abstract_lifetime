#!/usr/bin/env Rscript
# ingest_jmig_2017_json.R
# Reads jmig_2017_abstracts.json (produced by jmig_2017_scraper.js run in Chrome)
# and patches abstracts_cleaned.csv with the recovered abstract text.

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(jsonlite); library(stringr); library(cli)
})

json_path <- here("data", "cache", "jmig_2017_abstracts.json")
if (!file.exists(json_path)) {
  cli_abort("File not found: {json_path}\nRun jmig_2017_scraper.js in Chrome first, then move the downloaded JSON here.")
}

jmig <- fromJSON(json_path) |> as_tibble()
cli_alert_info("JSON has {nrow(jmig)} entries")
cli_alert_info("With text: {sum(!is.na(jmig$abstract_text) & nchar(jmig$abstract_text) > 30)}")

# Map PII back to abstract_id using the cached ScienceDirect PII filenames
# PII format S1553-4650(17)30453-3 → S1553465017304533
pii_to_sd <- function(jmig_pii) {
  x <- str_remove_all(jmig_pii, "[-()]")   # remove dashes and parens
  x  # already matches ScienceDirect format
}

# Load abstracts and match via DOI → PII → abstract_id
abstracts_path <- here("data", "processed", "abstracts_cleaned.csv")
abstracts <- read_csv(abstracts_path, show_col_types = FALSE)

# Load the 2017 abstracts with their DOIs
missing_2017 <- abstracts |>
  filter(congress_year == 2017, is.na(abstract_text) | nchar(abstract_text) < 10)

cli_alert_info("{nrow(missing_2017)} 2017 abstracts currently missing text")

# Build lookup: SD_PII → abstract_id using cache filenames
# Filenames are SD PIIs; they map sequentially to AAGL2017_001..097
sd_piis_sorted <- sort(list.files(here("data", "cache", "sd_html"),
                                   pattern = "S155346501730.*\\.html") |>
                         str_remove("\\.html$"))

# Map each jmig PII to SD PII then to abstract_id
jmig_matched <- jmig |>
  mutate(sd_pii = pii_to_sd(pii)) |>
  left_join(
    tibble(sd_pii = sd_piis_sorted,
           abstract_id = sprintf("AAGL2017_%03d", seq_along(sd_piis_sorted))),
    by = "sd_pii"
  ) |>
  filter(!is.na(abstract_id), !is.na(abstract_text), nchar(abstract_text) > 30)

cli_alert_info("Matched {nrow(jmig_matched)} abstracts with text to abstract IDs")

if (nrow(jmig_matched) == 0) {
  cli_alert_warning("No matches found — check PII format in the JSON")
  quit(save = "no")
}

# Patch abstracts_cleaned.csv
abstracts_patched <- abstracts |>
  left_join(jmig_matched |> select(abstract_id, abstract_text_new = abstract_text),
            by = "abstract_id") |>
  mutate(
    abstract_text = if_else(
      !is.na(abstract_text_new) & (is.na(abstract_text) | nchar(abstract_text) < 10),
      abstract_text_new, abstract_text
    )
  ) |>
  select(-abstract_text_new)

write_csv(abstracts_patched, abstracts_path)
cli_alert_success("Patched {nrow(jmig_matched)} 2017 abstracts into abstracts_cleaned.csv")

# Coverage report
abstracts_patched |>
  filter(congress_year <= 2018) |>
  group_by(congress_year) |>
  summarise(n = n(), has_text = sum(!is.na(abstract_text) & nchar(abstract_text) > 10),
            pct = round(has_text / n * 100, 1), .groups = "drop") |>
  print()
