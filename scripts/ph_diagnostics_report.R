#!/usr/bin/env Rscript
# ph_diagnostics_report.R — the supporting figures behind the PH remediation.
#
# docs/STATISTICAL_ANALYSIS.md and appendix A16 quote several numbers that
# justify how the proportional-hazards violation was handled: the model frame
# size, what the global Schoenfeld test becomes once the violating term is
# removed, the AIC of the time-varying fit against the proportional one, the
# correlation of the violator's Schoenfeld residuals with time, and the share of
# the frame sitting at the author-count ceiling.
#
# Until now those were computed ad hoc and pasted into prose, which is the
# failure mode this repository keeps rediscovering: a number with no producer
# cannot be re-derived, so nothing notices when it stops being true. This script
# is the producer. Its output is registered in docs/manuscript_claims.csv, so
# the claims test recomputes them like every other reported figure.
#
# Usage: Rscript scripts/ph_diagnostics_report.R
# Writes: output/cox_ph_support.csv

suppressPackageStartupMessages({
  library(survival); library(dplyr); library(readr); library(tidyr)
  library(here); library(cli)
})

source(here("R", "utils_congresses.R"))
cfg <- config::get(file = here("config.yml"))

results <- read_csv(here("output", "final_analytical_dataset.csv"), show_col_types = FALSE)

km_data <- results |>
  filter(!is.na(final_published)) |>
  mutate(
    censor_time = as.numeric(difftime(
      as.Date(cfg$pubmed$date_end, "%Y/%m/%d"),
      conference_date_for(congress_year, cfg), units = "days")) / 30.44,
    time = case_when(
      final_published & !is.na(months_to_pub) ~ months_to_pub,
      !final_published ~ censor_time,
      TRUE ~ NA_real_),
    event = as.integer(final_published)) |>
  filter(!is.na(time), time > 0)

# Read the specification back from the screen rather than restating it, so this
# cannot drift from the model that was actually fitted.
screen_path <- here("output", "model_variable_screen.csv")
terms <- if (file.exists(screen_path)) {
  s <- read_csv(screen_path, show_col_types = FALSE)
  s$variable[s$model == "cox" & s$kept]
} else {
  c("is_rct", "is_academic", "is_us_based", "n_authors",
    "gender_unified", "is_multicenter")
}
terms <- intersect(terms, names(km_data))

cox_data <- km_data |> drop_na(all_of(terms))
f  <- as.formula(paste("Surv(time, event) ~", paste(terms, collapse = " + ")))
m  <- coxph(f, data = cox_data)
z  <- cox.zph(m)

tab <- as.data.frame(z$table); tab$term <- rownames(tab)
violators <- tab$term[tab$term != "GLOBAL" & tab$p < 0.05]
violators <- intersect(violators, terms)

rows <- list(
  tibble(metric = "cox_n",      value = as.numeric(m$n)),
  tibble(metric = "cox_events", value = as.numeric(m$nevent)),
  tibble(metric = "ph_global_p", value = round(unname(tab$p[tab$term == "GLOBAL"]), 4))
)

if (length(violators) > 0) {
  v <- violators[1]

  # What the global test becomes with the violator removed: the evidence that
  # the violation is confined to that one term.
  rest <- setdiff(terms, v)
  if (length(rest) >= 2) {
    m_wo <- coxph(as.formula(paste("Surv(time, event) ~", paste(rest, collapse = " + "))),
                  data = cox_data)
    rows <- c(rows, list(tibble(
      metric = "ph_global_p_without_violator",
      value  = round(unname(cox.zph(m_wo)$table["GLOBAL", "p"]), 4))))
  }

  if (is.numeric(cox_data[[v]])) {
    tv <- tryCatch(
      coxph(as.formula(paste("Surv(time, event) ~", paste(terms, collapse = " + "),
                             sprintf("+ tt(%s)", v))),
            data = cox_data, tt = function(x, t, ...) x * log(t)),
      error = function(e) NULL)
    if (!is.null(tv)) {
      rows <- c(rows, list(
        tibble(metric = "aic_proportional", value = round(AIC(m), 1)),
        tibble(metric = "aic_time_varying", value = round(AIC(tv), 1))))
    }

    i <- match(v, colnames(z$y))
    if (!is.na(i)) {
      rows <- c(rows, list(tibble(
        metric = "schoenfeld_rho_violator",
        value  = round(suppressWarnings(cor(z$x, z$y[, i], method = "spearman")), 3))))
    }

    # The ceiling the violator is estimated over. A per-unit effect measured on
    # a censored covariate is a lower bound, and the share at the cap says how
    # much of one.
    cap <- max(cox_data[[v]], na.rm = TRUE)
    rows <- c(rows, list(
      tibble(metric = "violator_ceiling", value = as.numeric(cap)),
      tibble(metric = "violator_n_at_ceiling",
             value = as.numeric(sum(cox_data[[v]] >= cap, na.rm = TRUE))),
      tibble(metric = "violator_pct_at_ceiling",
             value = round(100 * mean(cox_data[[v]] >= cap, na.rm = TRUE), 1))))
  }
}

out <- bind_rows(rows)
write_csv(out, here("output", "cox_ph_support.csv"))
cli_alert_success("output/cox_ph_support.csv — {nrow(out)} metrics")
print(as.data.frame(out))
