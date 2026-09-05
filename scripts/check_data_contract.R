#!/usr/bin/env Rscript
# check_data_contract.R — run the row-level data contract and write the report.
#
# Usage: Rscript scripts/check_data_contract.R
# Writes: output/data_contract_violations.csv (always, even when empty, so the
#         absence of violations is a recorded fact rather than a missing file)
# Exit:   0 clean, 1 violations found.

suppressPackageStartupMessages({library(here); library(readr); library(cli); library(yaml)})
source(here("R", "utils_data_contract.R"))

v <- validate_data_contract(here("config", "data_contract.yml"),
                            report_dir = here("output", "data_contract_reports"))
out <- here("output", "data_contract_violations.csv")
write_csv(v, out)

if (nrow(v) == 0) {
  cli_alert_success("Row-level data contract holds; report written to {.path {out}}")
  quit(save = "no", status = 0)
}

cli_alert_danger("{nrow(v)} row-level contract violation(s)")
print(utils::head(as.data.frame(v), 20))
cli_alert_info("Full report: {.path {out}}")
quit(save = "no", status = 1)
