#!/usr/bin/env Rscript
# pre_congress_exclusions.R — the abstracts the pre-congress rule removes.
#
# PI decision, 2026-09-05: a publication that appeared before the congress
# cannot be a conference-to-publication conversion, and neither a `definite`
# classification nor a reviewer's `match` overrides that.
#
# output/excluded_pre_congress_publications.csv previously had no producer at
# all. It was built by hand from ArticleDate, the electronic publication date,
# while the analysis measured intervals from JournalIssue/PubDate, the print
# issue. The two bases disagreed by up to 4.9 months and identified different
# sets, so the file could not be used to audit the rule it described.
#
# This regenerates it from the analytical dataset, on the decided basis, so the
# evidence and the rule are the same thing measured once.
#
# Usage: Rscript scripts/pre_congress_exclusions.R
# Writes: output/excluded_pre_congress_publications.csv

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(cli)
})

fad <- here("output", "final_analytical_dataset.csv")
if (!file.exists(fad)) stop("run R/06_analyze_results.R first")
f <- read_csv(fad, show_col_types = FALSE)

if (!"months_to_pub" %in% names(f)) stop("no months_to_pub in the analytical dataset")

ex <- f |>
  filter(!is.na(months_to_pub), months_to_pub < 0) |>
  transmute(
    congress_year,
    abstract_id,
    abstract_title  = .data[[intersect(c("title", "abstract_title"), names(f))[1]]],
    pmid            = final_pmid,
    paper_title     = if ("pub_title" %in% names(f)) pub_title else NA_character_,
    journal         = if ("pub_journal" %in% names(f)) pub_journal else NA_character_,
    months_before_congress = round(abs(months_to_pub), 1),
    classification,
    final_published
  ) |>
  arrange(months_before_congress)

write_csv(ex, here("output", "excluded_pre_congress_publications.csv"))

still_counted <- sum(ex$final_published %in% TRUE)
cli_alert_success("{nrow(ex)} abstract{?s} have a credited publication predating their congress")
if (still_counted > 0) {
  cli_alert_danger("{still_counted} of them are still counted as published; the rule is not being applied")
} else {
  cli_alert_success("None is counted as published")
}
print(as.data.frame(ex |> select(abstract_id, congress_year, months_before_congress,
                                 classification, final_published)))
