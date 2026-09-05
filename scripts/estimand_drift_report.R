#!/usr/bin/env Rscript
# estimand_drift_report.R — detect silent changes to what is being estimated.
#
# The tests in this repository check that VALUES are right. Nothing checks that
# the question stayed the same. Those are different failures, and the second is
# worse: a publication rate of 16.9% is not wrong in the way a miscalculated
# number is wrong if the denominator quietly stopped excluding unresolved
# abstracts, or the censoring horizon moved, or a match tier started counting as
# published. The value still reconciles, every test still passes, and the
# estimand is no longer the one the protocol described.
#
# This records the parameters that DEFINE the target quantity -- not its value --
# as a fingerprint, and compares them against docs/estimand_baseline.yml. Drift
# is not treated as an error to be fixed automatically: it is surfaced, and the
# baseline has to be updated deliberately, which is the point.
#
# Usage: Rscript scripts/estimand_drift_report.R [--update-baseline]
# Writes: output/estimand_drift.csv, output/estimand_current.yml
# Exit:   0 no drift, 1 drift detected.

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(yaml); library(cli)
})

args <- commandArgs(trailingOnly = TRUE)
update_baseline <- "--update-baseline" %in% args

cfg <- config::get(file = here("config.yml"))

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

# ---- the estimand fingerprint ------------------------------------------------
# Each element answers "what question is this analysis asking?" A change to any
# of them changes the estimand, whatever happens to the numbers.

congress_years <- sort(as.integer(names(cfg$congresses) %||% character(0)))
if (!length(congress_years)) {
  congress_years <- sort(unique(suppressWarnings(
    readr::read_csv(here("output", "final_analytical_dataset.csv"),
                    show_col_types = FALSE)$congress_year)))
}

d <- read_csv(here("output", "final_analytical_dataset.csv"), show_col_types = FALSE)

fingerprint <- list(
  # Population
  population = list(
    congress_years      = as.integer(congress_years),
    n_congress_years    = length(congress_years),
    cohort_size         = nrow(d),
    unit_of_analysis    = "one conference abstract"
  ),

  # Denominator rule: which abstracts are eligible to have an outcome at all.
  denominator = list(
    rule                = "abstracts whose match status was resolved",
    excludes_unresolved = TRUE,
    n_unresolved        = sum(is.na(d$final_published)),
    n_evaluated         = sum(!is.na(d$final_published))
  ),

  # Outcome: what counts as the event.
  outcome = list(
    definition          = "matched to a full publication that appeared after the congress",
    counted_as_published = paste(
      "classification == 'definite', or a reviewer decision of match;",
      "in either case only if the credited publication's print issue date is",
      "on or after the congress date (PI decision, 2026-09-05)"),
    pre_congress_excluded = "always; no classification or reviewer verdict overrides it",
    n_events            = sum(d$final_published %in% TRUE)
  ),

  # Time: origin and administrative censoring.
  time = list(
    origin              = "congress date",
    censoring           = "administrative at the end of the search window",
    search_window_start = cfg$pubmed$date_start,
    search_window_end   = cfg$pubmed$date_end,
    interval_units      = "months"
  ),

  # Estimator settings that change the target rather than its precision.
  estimation = list(
    survival_model      = "Kaplan-Meier and Cox proportional hazards",
    pre_congress_handling = "publications dated before their congress are excluded from the interval"
  )
)

flatten_fp <- function(x, prefix = "") {
  out <- list()
  for (nm in names(x)) {
    v <- x[[nm]]
    key <- if (nzchar(prefix)) paste(prefix, nm, sep = ".") else nm
    if (is.list(v)) {
      out <- c(out, flatten_fp(v, key))
    } else {
      out[[key]] <- paste(as.character(v), collapse = ", ")
    }
  }
  out
}

current_flat <- flatten_fp(fingerprint)
write_yaml(fingerprint, here("output", "estimand_current.yml"))

baseline_path <- here("docs", "estimand_baseline.yml")

if (update_baseline || !file.exists(baseline_path)) {
  write_yaml(fingerprint, baseline_path)
  cli_alert_success("Estimand baseline written to {.path {baseline_path}}")
  write_csv(tibble(component = character(), baseline = character(),
                   current = character(), status = character()),
            here("output", "estimand_drift.csv"))
  quit(save = "no", status = 0)
}

baseline_flat <- flatten_fp(read_yaml(baseline_path))

keys <- union(names(baseline_flat), names(current_flat))
rows <- lapply(keys, function(k) {
  b <- baseline_flat[[k]] %||% NA_character_
  c_ <- current_flat[[k]] %||% NA_character_
  status <- if (is.na(b)) "added" else if (is.na(c_)) "removed" else
    if (identical(b, c_)) "unchanged" else "CHANGED"
  tibble(component = k, baseline = b, current = c_, status = status)
})
drift <- bind_rows(rows)
write_csv(drift, here("output", "estimand_drift.csv"))

changed <- drift |> filter(status != "unchanged")

if (nrow(changed) == 0) {
  cli_alert_success("Estimand unchanged against the baseline ({nrow(drift)} components)")
  quit(save = "no", status = 0)
}

cli_alert_danger("Estimand drift: {nrow(changed)} component(s) differ from the baseline")
print(as.data.frame(changed))
cli_alert_info(paste("If the change is intended, re-run with --update-baseline and say why",
                     "in the commit message. If it is not, the analysis is answering a",
                     "different question than the protocol describes."))
quit(save = "no", status = 1)
