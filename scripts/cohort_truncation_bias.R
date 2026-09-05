#!/usr/bin/env Rscript
# cohort_truncation_bias.R — how much does the ingestion ceiling matter?
#
# scripts/audit_cohort_completeness.R measures the SIZE of the truncation: the
# pipeline holds 1,154 of 7,711 supplement items because R/01b_parse_web.R
# fetches one listing page per congress and ScienceDirect returns that same page
# for every offset, capping each congress at roughly 100 items.
#
# Size is not the same as consequence. Whether a 15% sample biases the
# publication rate depends on whether the captured items differ from the rest,
# and the captured set is not a random sample: it is the FIRST N pages of each
# supplement in printed order.
#
# This script tests the one thing the available data can answer. Within the
# window that WAS captured, does publication vary with position in the
# supplement? If the rate is flat across the captured pages, extrapolating to
# later pages is less hazardous. If it slopes, the truncation is a bias with a
# direction, and the direction is measurable even though the magnitude beyond
# the window is not.
#
# What this script cannot do, and why, is recorded in appendix A23: the session
# type of an uncaptured item is unknowable from any source currently reachable.
#
# Usage:  Rscript scripts/cohort_truncation_bias.R
# Output: output/cohort_truncation_bias.csv

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(stringr); library(cli)
})

fad <- read_csv(here("output", "final_analytical_dataset.csv"), show_col_types = FALSE)
parsed <- read_csv(here("data", "processed", "abstracts_parsed.csv"),
                   show_col_types = FALSE) |>
  mutate(page_num = as.integer(str_extract(page_range, "(?<=S)[0-9]+"))) |>
  select(abstract_id, page_num)

d <- fad |>
  left_join(parsed, by = "abstract_id") |>
  filter(!is.na(final_published), !is.na(page_num)) |>
  group_by(congress_year) |>
  mutate(position = (page_num - min(page_num)) /
           pmax(1, max(page_num) - min(page_num))) |>
  ungroup()

if (nrow(d) < 50) stop("too few abstracts carry a page number to test position")

quint <- d |>
  mutate(quintile = ntile(position, 5)) |>
  group_by(quintile) |>
  summarise(n = n(),
            published = sum(final_published),
            rate = round(100 * mean(final_published), 1),
            pct_rct = round(100 * mean(is_rct, na.rm = TRUE), 1),
            .groups = "drop")

# Adjusted for congress year, because both position and rate vary by year.
m <- glm(final_published ~ position + factor(congress_year),
         data = d, family = binomial)
co <- summary(m)$coefficients

out <- bind_rows(
  quint |> transmute(metric = paste0("rate_quintile_", quintile), value = rate),
  quint |> transmute(metric = paste0("pct_rct_quintile_", quintile), value = pct_rct),
  tibble(metric = "position_odds_ratio",
         value = round(unname(exp(coef(m)["position"])), 3)),
  tibble(metric = "position_p_value",
         value = round(unname(co["position", "Pr(>|z|)"]), 4)),
  tibble(metric = "n_analysed", value = nrow(d))
)
write_csv(out, here("output", "cohort_truncation_bias.csv"))

cli_h2("Position within the captured window")
print(as.data.frame(quint))
cli_alert_info(
  "Position odds ratio {round(exp(coef(m)['position']), 3)}, \\
   p = {round(co['position','Pr(>|z|)'], 3)}, adjusted for congress year")

if (co["position", "Pr(>|z|)"] < 0.05) {
  cli_alert_danger(
    "Publication varies with supplement position. The truncation is a bias \\
     with a direction, not merely a smaller sample.")
} else {
  cli_alert_warning(
    "No significant position effect within the captured window. That is weak \\
     reassurance, not evidence of none: the test has {nrow(d)} abstracts over \\
     the first ~15% of each supplement and cannot see beyond it.")
}
