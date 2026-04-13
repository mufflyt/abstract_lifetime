# 01_parse_pdf.R — PDF text extraction (fallback approach)
# Uses state machine parser from utils_pdf.R

library(here)
library(readr)
library(dplyr)
library(cli)
library(config)

source(here("R", "utils_pdf.R"))

cfg <- config::get(file = here("config.yml"))
pdf_path <- here("data", "raw", cfg$sources$pdf_filename)

if (!file.exists(pdf_path)) {
  cli_alert_danger("PDF not found at: {pdf_path}")
  cli_alert_info("Place the AAGL 2023 abstracts PDF in data/raw/")
  cli_alert_info("Expected filename: {cfg$sources$pdf_filename}")
  stop("PDF file missing. Cannot proceed with PDF parsing.")
}

cli_alert_info("Extracting text from PDF: {basename(pdf_path)}")
pages <- extract_pdf_text(pdf_path)
cli_alert_success("Extracted {length(pages)} pages")

cli_alert_info("Parsing abstracts with state machine parser...")
abstracts_list <- parse_pdf_abstracts(pages)
cli_alert_success("Found {length(abstracts_list)} potential abstracts")

cli_alert_info("Converting to data frame...")
abstracts_df <- abstracts_to_df(abstracts_list)

# Add metadata
abstracts_df <- abstracts_df |>
  mutate(
    PDF_name = basename(pdf_path),
    parse_timestamp = Sys.time()
  )

# Save
out_path <- here("data", "processed", "abstracts_parsed_pdf.csv")
write_csv(abstracts_df, out_path)
cli_alert_success("Saved {nrow(abstracts_df)} abstracts to {out_path}")

# If no web-parsed file exists, this becomes the primary
if (!file.exists(here("data", "processed", "abstracts_parsed.csv"))) {
  write_csv(abstracts_df, here("data", "processed", "abstracts_parsed.csv"))
  cli_alert_info("Set as primary parsed file (no web source available)")
}
