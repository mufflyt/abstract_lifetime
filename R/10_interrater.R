# 10_interrater.R — Compute interrater agreement for abstracts reviewed by
# 2+ reviewers. Reports Cohen's kappa and percentage agreement.
#
# Reads from Google Sheets (or local CSV fallback).
# Writes: output/interrater_agreement.csv

suppressPackageStartupMessages({
  library(here); library(config); library(cli); library(dplyr); library(readr)
  library(tibble)
})

cli_h2("Interrater Agreement")

decisions_path <- here("output", "manual_review_decisions.csv")
if (!file.exists(decisions_path)) {
  cli_alert_warning("No manual_review_decisions.csv found — skipping interrater")
  quit(save = "no")
}

decisions <- read_csv(decisions_path, show_col_types = FALSE)

if (!"reviewer" %in% names(decisions) || !"manual_decision" %in% names(decisions)) {
  cli_alert_warning("Decisions file missing reviewer or manual_decision columns")
  quit(save = "no")
}

# Deduplicate: latest decision per (abstract_id, reviewer)
deduped <- decisions |>
  filter(!is.na(reviewer), reviewer != "AUTO") |>
  group_by(abstract_id, reviewer) |>
  arrange(desc(review_timestamp)) |>
  slice(1) |>
  ungroup()

# Find abstracts with 2+ distinct human reviewers
multi <- deduped |>
  group_by(abstract_id) |>
  filter(n_distinct(reviewer) >= 2) |>
  ungroup()

if (nrow(multi) == 0) {
  cli_alert_warning("No abstracts with 2+ human reviewers — cannot compute kappa")
  write_csv(tibble(metric = "n_multi_reviewed", value = 0),
            here("output", "interrater_agreement.csv"))
  quit(save = "no")
}

n_abstracts <- length(unique(multi$abstract_id))
cli_alert_info("{n_abstracts} abstracts reviewed by 2+ reviewers")

# For each pair, check agreement
pairs <- multi |>
  group_by(abstract_id) |>
  summarise(
    reviewers = paste(unique(reviewer), collapse = "+"),
    decisions = paste(sort(unique(manual_decision)), collapse = "+"),
    agree = n_distinct(manual_decision) == 1,
    .groups = "drop"
  )

pct_agree <- round(mean(pairs$agree) * 100, 1)
cli_alert_info("Raw agreement: {pct_agree}%")

# Cohen's kappa (requires irr package)
kappa_val <- NA_real_
if (requireNamespace("irr", quietly = TRUE) && n_abstracts >= 5) {
  # Build a rating matrix: columns = reviewers, rows = abstracts
  # Take the first two reviewers per abstract for a 2-rater kappa
  reviewer_pairs <- multi |>
    group_by(abstract_id) |>
    arrange(reviewer) |>
    mutate(rater_num = row_number()) |>
    filter(rater_num <= 2) |>
    ungroup()

  wide <- reviewer_pairs |>
    select(abstract_id, rater_num, manual_decision) |>
    tidyr::pivot_wider(names_from = rater_num, values_from = manual_decision,
                       names_prefix = "rater_")

  if (nrow(wide) >= 5 && all(c("rater_1", "rater_2") %in% names(wide))) {
    k <- tryCatch(
      irr::kappa2(wide |> select(rater_1, rater_2)),
      error = function(e) NULL
    )
    if (!is.null(k)) {
      kappa_val <- round(k$value, 3)
      cli_alert_success("Cohen's kappa: {kappa_val} (p = {round(k$p.value, 4)})")
    }
  }
}

result <- tibble(
  metric = c("n_multi_reviewed", "pct_agreement", "cohens_kappa"),
  value = c(n_abstracts, pct_agree, kappa_val)
)
write_csv(result, here("output", "interrater_agreement.csv"))
cli_alert_success("Interrater agreement saved")
print(result)
