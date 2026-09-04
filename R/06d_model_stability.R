# 06d_model_stability.R — How much do the regression findings depend on the
# particular sample and the particular congresses?
#
# Why this exists
# ---------------
# docs/STATISTICAL_ANALYSIS.md records that neither model has any influence,
# stability or leave-one-out diagnostic, and separately flags `is_academic` as
# provisional: it moved from HR 0.87 (p = 0.62) to 0.62 (p = 0.007) when
# R/02d_rederive_predictors.R corrected the covariate it rests on. "Provisional"
# was a judgement. This puts numbers on it.
#
# Two analyses, both on the logistic model's specification:
#
#   1. Bootstrap predictor stability. Refit on `n_boot` resamples and count how
#      often each term is retained at p < 0.05. A term retained in 99% of
#      resamples is a finding; one retained in 20% is noise that happened to
#      clear the threshold once.
#
#   2. Leave-one-congress-out. Refit twelve times, dropping each congress in
#      turn, and report the term's estimate each time. This matters here more
#      than in most studies because two congresses (2017, 2018) have no
#      recoverable abstract text, so their covariates are near-constant - a
#      finding that depends on either of them would be an artefact of that.
#
# Both come from the mysterycall package, pinned in docs/REPRODUCIBILITY.md.
# The script is a no-op with a clear message if it is absent.
#
# Reproducible: seeded from config.yml (pipeline$seed).
#
# Outputs:
#   output/model_predictor_stability.csv
#   output/model_leave_one_congress_out.csv

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(tidyr)
  library(cli); library(tibble); library(config)
})

cli_h2("Model stability")

cfg <- config::get(file = here("config.yml"))
fad_path <- here("output", "final_analytical_dataset.csv")
screen_path <- here("output", "model_variable_screen.csv")

if (!file.exists(fad_path)) {
  cli_alert_danger("No final_analytical_dataset.csv - run 06_analyze_results.R first")
} else if (!requireNamespace("mysterycall", quietly = TRUE)) {
  cli_alert_warning("mysterycall not installed - skipping stability analysis")
  cli_alert_info("Install with: remotes::install_github('mufflyt/mysterycall@42d66d92')")
} else {

results <- read_csv(fad_path, show_col_types = FALSE) |>
  filter(!is.na(final_published)) |>
  mutate(published_int = as.integer(final_published),
         log_sample_size = log1p(coalesce(sample_size, 0)))

# Use exactly the specification 06 shipped, read from the screen it recorded,
# so this cannot drift from the model it is meant to describe.
core <- c("is_rct", "log_sample_size", "is_academic", "is_us_based")
extras <- if (file.exists(screen_path)) {
  scr <- read_csv(screen_path, show_col_types = FALSE)
  scr$variable[scr$model == "logistic" & scr$kept]
} else {
  cli_alert_warning("No model_variable_screen.csv - falling back to the core terms")
  character(0)
}
predictors <- intersect(c(core, extras), names(results))
cli_alert_info("Specification: published_int ~ {paste(predictors, collapse = ' + ')}")

model_data <- results |> drop_na(all_of(setdiff(predictors, "log_sample_size")))
cli_alert_info("Complete cases: {nrow(model_data)} of {nrow(results)} evaluated")

# ---- 1. Bootstrap predictor stability ---------------------------------------

set.seed(cfg$pipeline$seed)
N_BOOT <- 500L

stab <- tryCatch(
  mysterycall::mysterycall_bootstrap_predictor_stability(
    model_data, outcome = "published_int", predictors = predictors,
    n_boot = N_BOOT, p_threshold = 0.05, family = "binomial"
  ),
  error = function(e) { cli_alert_danger("stability failed: {conditionMessage(e)}"); NULL }
)

if (!is.null(stab)) {
  stab <- as_tibble(stab) |>
    rename_with(tolower) |>
    mutate(n_boot = N_BOOT,
           interpretation = case_when(
             retention_frequency >= 90 ~ "robust",
             retention_frequency >= 70 ~ "moderate - report with caution",
             TRUE                      ~ "unstable - not a reliable finding"
           )) |>
    arrange(desc(retention_frequency))
  write_csv(stab, here("output", "model_predictor_stability.csv"))
  cli_alert_success("Predictor stability over {N_BOOT} resamples:")
  print(as.data.frame(stab))

  weak <- stab |> filter(retention_frequency < 70)
  if (nrow(weak) > 0) {
    cli_alert_warning(
      "{nrow(weak)} term{?s} retained in under 70% of resamples: \\
       {paste(weak$predictor, collapse = ', ')}"
    )
  }
}

# ---- 2. Leave-one-congress-out ----------------------------------------------

fit <- glm(
  as.formula(paste("published_int ~", paste(predictors, collapse = " + "))),
  data = model_data, family = binomial()
)

# Every non-intercept term, so no single result is cherry-picked.
terms_to_test <- setdiff(names(coef(fit)), "(Intercept)")

loo_rows <- lapply(terms_to_test, function(tm) {
  res <- tryCatch(
    mysterycall::mysterycall_leave_one_out(
      fit, model_data, group = "congress_year", term = tm, exponentiate = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)
  tab <- tryCatch(as_tibble(res), error = function(e) NULL)
  if (is.null(tab) || nrow(tab) == 0) return(NULL)
  tab |> mutate(term = tm, .before = 1)
})

loo <- bind_rows(loo_rows)

if (nrow(loo) > 0) {
  write_csv(loo, here("output", "model_leave_one_congress_out.csv"))

  summ <- loo |>
    group_by(term) |>
    summarise(
      n_refits = n(),
      all_converged = all(converged),
      min_ratio = round(min(ratio, na.rm = TRUE), 3),
      max_ratio = round(max(ratio, na.rm = TRUE), 3),
      n_sig = sum(p_value < 0.05, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(sign_stable = (min_ratio > 1 & max_ratio > 1) |
                          (min_ratio < 1 & max_ratio < 1))
  cli_alert_success("Leave-one-congress-out, {nrow(loo)} refits:")
  print(as.data.frame(summ))

  flipped <- summ |> filter(!sign_stable)
  if (nrow(flipped) > 0) {
    cli_alert_warning(
      "{nrow(flipped)} term{?s} change direction when a single congress is \\
       dropped: {paste(flipped$term, collapse = ', ')}"
    )
  }
  fragile <- summ |> filter(n_sig < n_refits, n_sig > 0)
  if (nrow(fragile) > 0) {
    cli_alert_info(
      "{nrow(fragile)} term{?s} lose significance on at least one refit: \\
       {paste(fragile$term, collapse = ', ')}"
    )
  }
} else {
  cli_alert_warning("Leave-one-congress-out produced no rows")
}

}
