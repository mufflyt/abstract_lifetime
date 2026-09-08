# regen_ph_diagnostics.R — rebuild the PH diagnostics artifacts from the
# COMMITTED analytic dataset, without re-running the whole of 06.
#
# Why this exists rather than "just run 06": re-running R/06_analyze_results.R
# from committed inputs currently produces a DIFFERENT analytic dataset from
# the committed one (172 published against 170, 16.4% against 16.2%). The
# committed outputs are stale relative to their own inputs. That is a
# reproducibility defect in its own right and a change in a reported number, so
# it is not something a diagnostics refactor may absorb silently.
#
# This script therefore reads the committed dataset and writes only the two new
# PH artifacts, leaving every existing number untouched. When the staleness is
# resolved by whoever owns that decision, 06 produces these artifacts itself and
# this script becomes unnecessary.
#
# Usage: Rscript scripts/regen_ph_diagnostics.R

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(survival)
  library(broom); library(config); library(cli)
})
source(here("R", "utils_ph.R"))
source(here("R", "utils_congresses.R"))
cfg <- config::get(file = here("config.yml"))
PH_ALPHA <- 0.05

results <- read_csv(here("output", "final_analytical_dataset.csv"),
                    show_col_types = FALSE, progress = FALSE)

km_data <- results |>
  filter(!is.na(final_published)) |>
  mutate(
    censor_time = as.numeric(difftime(as.Date(cfg$pubmed$date_end, "%Y/%m/%d"),
                                      conference_date_for(congress_year, cfg),
                                      units = "days")) / 30.44,
    time = case_when(
      final_published & !is.na(months_to_pub) ~ months_to_pub,
      !final_published ~ censor_time,
      TRUE ~ NA_real_),
    event = as.integer(final_published)) |>
  filter(!is.na(time), time > 0)

parts <- read_csv(here("output", "cox_ph_terms.csv"), show_col_types = FALSE,
                  progress = FALSE)$term
parts <- setdiff(parts, "GLOBAL")
cox_data <- km_data |> tidyr::drop_na(all_of(parts))

fit <- coxph(as.formula(paste("Surv(time, event) ~", paste(parts, collapse = " + "))),
             data = cox_data)
ident <- ph_identify_violators(cox.zph(fit), parts, PH_ALPHA)
plan <- ph_plan_remediation(ident$violators, cox_data)

fits <- list(original = fit)
prod_fit <- NULL
if (nrow(plan) > 0) {
  prod_fit <- coxph(ph_production_formula(parts, plan), data = cox_data)
  fits$production <- prod_fit
  drop_terms <- setdiff(parts, ident$violators)
  if (length(drop_terms) >= 1)
    fits$violators_dropped <- coxph(
      as.formula(paste("Surv(time, event) ~", paste(drop_terms, collapse = " + "))),
      data = cox_data)
  tvf <- ph_timevarying_formula(parts, plan)
  if (!is.null(tvf))
    fits$time_varying <- tryCatch(
      coxph(tvf, data = cox_data, tt = function(x, t, ...) x * log(t)),
      error = function(e) NULL)
}

write_csv(
  plan |>
    left_join(ident$table |> select(variable = term, ph_chisq = chisq,
                                    ph_df = df, ph_p = p), by = "variable") |>
    mutate(across(where(is.numeric), ~ round(.x, 4))),
  here("output", "cox_ph_remediation.csv"))
write_csv(ph_model_comparison(fits), here("output", "cox_ph_model_comparison.csv"))

# The full diagnostic row: original, violators-dropped, and production, so the
# three are legible side by side rather than in three separate files.
drop_p <- if (!is.null(fits$violators_dropped))
  ph_retest(fits$violators_dropped)$global_p else NA_real_
retest <- ph_retest(prod_fit)
write_csv(
  tibble::tibble(
    test = "cox_zph_global",
    p_value = round(ident$global_p, 3),
    violating_terms = if (length(ident$violators)) paste(ident$violators, collapse = "|") else NA_character_,
    remediation = if (nrow(plan) == 0) "none_needed" else
      paste(sprintf("%s:%s", plan$remediation, plan$variable), collapse = ";"),
    remediated_global_p = round(retest$global_p, 3),
    remediated_global_p_status = retest$status,
    violators_dropped_global_p = round(drop_p, 3),
    n_violators = length(ident$violators),
    production_model = "cox_model_production.rds"),
  here("output", "cox_ph_assumption.csv"))
if (!is.null(prod_fit))
  saveRDS(prod_fit, here("data", "processed", "cox_model_production.rds"))

cli_alert_success(
  "PH diagnostics regenerated from the committed dataset: {nrow(plan)} violator(s), \\
   original global p = {round(ident$global_p, 4)}, production global p = \\
   {round(ph_retest(prod_fit)$global_p, 4)}")
