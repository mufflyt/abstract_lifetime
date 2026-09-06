# cohort_coverage_by_year.R — how much of each congress programme we hold.
#
# The truncation figure in the manuscript and README (1,154 of 7,711) was only
# derivable from data/cache/crossref_supplements/, which is gitignored. A number
# in a limitations section that a reviewer cannot recompute is not a limitation,
# it is an assertion, so this writes the per-year counts to a tracked artifact
# the manuscript reads at knit time.
#
# "Supplement item" means a Crossref record whose page begins with S. The
# November publication-date window that retrieves them also returns regular
# journal content published that month -- 526 records across the twelve years --
# and counting those gives 8,237 rather than 7,711.
#
# Usage: Rscript scripts/cohort_coverage_by_year.R
# Needs the Crossref cache; run scripts/audit_cohort_completeness.R first.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

cache_dir <- here("data", "cache", "crossref_supplements")
out_path  <- here("output", "cohort_coverage_by_year.csv")

if (!dir.exists(cache_dir) || !length(list.files(cache_dir, pattern = "\\.json$"))) {
  message("Crossref cache absent; run scripts/audit_cohort_completeness.R first. ",
          "Leaving ", basename(out_path), " unchanged.")
} else {
  supp <- lapply(list.files(cache_dir, pattern = "\\.json$", full.names = TRUE), function(f) {
    items <- jsonlite::fromJSON(f, simplifyVector = FALSE)$message$items
    pages <- vapply(items, function(i) as.character(i$page %||% ""), character(1))
    tibble(congress_year = as.integer(substr(basename(f), 1, 4)),
           deposited_all = length(items),
           supplement_items = sum(grepl("^\\s*S", pages, ignore.case = TRUE)))
  }) |> bind_rows()

  captured <- read_csv(here("data", "processed", "abstracts_parsed.csv"),
                       show_col_types = FALSE, progress = FALSE) |>
    count(congress_year, name = "captured")

  cohort <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                     show_col_types = FALSE, progress = FALSE) |>
    count(congress_year, name = "oral_cohort")

  cov <- supp |>
    left_join(captured, by = "congress_year") |>
    left_join(cohort, by = "congress_year") |>
    mutate(non_supplement = deposited_all - supplement_items,
           coverage_pct = round(100 * captured / supplement_items, 1)) |>
    arrange(congress_year)

  write_csv(cov, out_path)
  message("wrote ", out_path, ": ", nrow(cov), " congresses, ",
          sum(cov$captured), " of ", sum(cov$supplement_items), " supplement items (",
          round(100 * sum(cov$captured) / sum(cov$supplement_items), 1), "%)")
}
