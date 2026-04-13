# 01c_compare_sources.R — Compare PDF vs web parsing for QA

library(here)
library(readr)
library(dplyr)
library(stringr)
library(cli)

source(here("R", "utils_text.R"))

cli_h2("Source Comparison: Web vs PDF")

web_path <- here("data", "processed", "abstracts_parsed_web.csv")
pdf_path <- here("data", "processed", "abstracts_parsed_pdf.csv")

if (!file.exists(web_path)) {
  cli_alert_warning("Web-parsed file not found, skipping comparison")
  quit(save = "no")
}
if (!file.exists(pdf_path)) {
  cli_alert_warning("PDF-parsed file not found, skipping comparison")
  quit(save = "no")
}

web <- read_csv(web_path, show_col_types = FALSE)
pdf <- read_csv(pdf_path, show_col_types = FALSE)

cli_alert_info("Web abstracts: {nrow(web)}")
cli_alert_info("PDF abstracts: {nrow(pdf)}")

# Compare counts
count_diff <- abs(nrow(web) - nrow(pdf))
if (count_diff > 5) {
  cli_alert_warning("Large count discrepancy: {count_diff} abstracts differ")
} else {
  cli_alert_success("Abstract counts within tolerance (diff = {count_diff})")
}

# Title matching between sources
web_titles_norm <- vapply(web$title, normalize_title, character(1))
pdf_titles_norm <- vapply(pdf$title, normalize_title, character(1))

# Find best matches
matches <- tibble::tibble(
  web_idx = integer(0), pdf_idx = integer(0),
  web_title = character(0), pdf_title = character(0),
  similarity = numeric(0)
)

for (i in seq_along(web_titles_norm)) {
  sims <- vapply(pdf_titles_norm, function(pt) {
    jaccard_similarity(web_titles_norm[i], pt)
  }, numeric(1))
  best_j <- which.max(sims)
  matches <- bind_rows(matches, tibble::tibble(
    web_idx = i, pdf_idx = best_j,
    web_title = web$title[i], pdf_title = pdf$title[best_j],
    similarity = sims[best_j]
  ))
}

# Report
high_match <- sum(matches$similarity >= 0.8)
med_match <- sum(matches$similarity >= 0.5 & matches$similarity < 0.8)
low_match <- sum(matches$similarity < 0.5)

cli_alert_success("High confidence matches (>=0.8): {high_match}")
cli_alert_info("Medium confidence matches (0.5-0.8): {med_match}")
cli_alert_warning("Low/no match (<0.5): {low_match}")

# Save comparison report
comparison_df <- matches |>
  mutate(match_quality = case_when(
    similarity >= 0.8 ~ "high",
    similarity >= 0.5 ~ "medium",
    TRUE ~ "low"
  ))

write_csv(comparison_df, here("data", "processed", "source_comparison.csv"))
cli_alert_success("Comparison report saved to data/processed/source_comparison.csv")

# Recommend which source to use
if (nrow(web) >= nrow(pdf) && high_match / nrow(web) > 0.7) {
  cli_alert_success("Recommendation: Use WEB source (more structured, good coverage)")
} else if (nrow(pdf) > nrow(web) * 1.1) {
  cli_alert_info("Recommendation: Use PDF source (more abstracts found)")
} else {
  cli_alert_info("Recommendation: Manual review needed — sources differ substantially")
}
