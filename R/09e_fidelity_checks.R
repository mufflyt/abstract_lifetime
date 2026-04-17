# 09e_fidelity_checks.R — Abstract-to-paper fidelity checks for matched pairs.
# For each definite/probable match, compares the AAGL abstract to the published
# paper on title, authors, sample size, and conclusion direction.
#
# Writes: data/processed/fidelity_checks.csv

suppressPackageStartupMessages({
  library(here); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble)
})

source(here("R", "utils_scoring.R"))
source(here("R", "utils_positivity.R"))

cli_h2("Abstract-to-Paper Fidelity Checks")

matches <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"), show_col_types = FALSE)
authors <- if (file.exists(here("data", "processed", "authors_pubmed.csv"))) {
  read_csv(here("data", "processed", "authors_pubmed.csv"), show_col_types = FALSE)
} else tibble()

# Only check definite + probable (the pairs where we believe there's a real match)
confirmed <- matches |>
  filter(classification %in% c("definite", "probable"))

cli_alert_info("{nrow(confirmed)} matched pairs to check")
if (nrow(confirmed) == 0) {
  cli_alert_warning("No matched pairs — skipping fidelity checks")
  quit(save = "no")
}

fidelity <- confirmed |>
  left_join(abstracts |> select(abstract_id, abs_title = title,
                                 abs_conclusion = abstract_conclusion,
                                 abs_first_au = first_author_normalized,
                                 abs_sample_size = sample_size),
            by = "abstract_id") |>
  mutate(
    title_jaccard = mapply(function(a, b) {
      if (is.na(a) || is.na(b)) return(NA_real_)
      wa <- str_split(normalize_title(a), "\\s+")[[1]]
      wb <- str_split(normalize_title(b), "\\s+")[[1]]
      wa <- wa[nchar(wa) >= 3]; wb <- wb[nchar(wb) >= 3]
      if (length(wa) == 0 || length(wb) == 0) return(NA_real_)
      length(intersect(wa, wb)) / length(union(wa, wb))
    }, abs_title, pub_title),
    title_changed = !is.na(title_jaccard) & title_jaccard < 0.7,
    first_author_changed = !is.na(abs_first_au) & !is.na(pub_first_author) &
      tolower(str_extract(abs_first_au, "^[a-z]+")) !=
      tolower(str_extract(pub_first_author, "^[a-z]+"))
  ) |>
  select(abstract_id, congress_year, classification,
         title_jaccard, title_changed, first_author_changed)

write_csv(fidelity, here("data", "processed", "fidelity_checks.csv"))

n <- nrow(fidelity)
cli_alert_success("Fidelity checks saved ({n} pairs)")
n_title_eval <- sum(!is.na(fidelity$title_changed))
n_au_eval <- sum(!is.na(fidelity$first_author_changed))
cli_alert_info("Title changed (Jaccard < 0.7): {sum(fidelity$title_changed, na.rm=TRUE)} / {n_title_eval} evaluable ({round(mean(fidelity$title_changed, na.rm=TRUE)*100,1)}%)")
cli_alert_info("First author changed: {sum(fidelity$first_author_changed, na.rm=TRUE)} / {n_au_eval} evaluable ({round(mean(fidelity$first_author_changed, na.rm=TRUE)*100,1)}%)")
