# The estimand must not move without someone saying so.
#
# Everything else in this suite checks that VALUES are right. Nothing checked
# that the question stayed the same, and those are different failures. A
# publication rate of 16.9% is not wrong in the way an arithmetic slip is wrong
# if the denominator quietly stopped excluding unresolved abstracts, or the
# censoring horizon moved, or another match tier started counting as published.
# The value still reconciles, the suite still passes, and the analysis is
# answering a different question than the protocol describes.
#
# docs/estimand_baseline.yml records the parameters that define the target
# quantity. This fails when the live fingerprint differs from it, so a change
# to the estimand has to be an explicit act -- re-running with
# --update-baseline and saying why -- rather than a side effect.

library(testthat)
suppressPackageStartupMessages({library(readr); library(yaml)})

repo_root     <- here::here()
baseline_path <- file.path(repo_root, "docs", "estimand_baseline.yml")
report_script <- file.path(repo_root, "scripts", "estimand_drift_report.R")

test_that("the estimand baseline exists and describes a complete estimand", {
  expect_true(file.exists(baseline_path))
  b <- read_yaml(baseline_path)

  # PICO-ish: population, denominator rule, outcome, time. Dropping any one of
  # these makes the recorded estimand ambiguous.
  for (section in c("population", "denominator", "outcome", "time")) {
    expect_true(!is.null(b[[section]]),
                label = sprintf("the baseline has no '%s' section", section))
  }

  expect_true(!is.null(b$denominator$excludes_unresolved),
              label = "the baseline does not say whether unresolved abstracts leave the denominator")
  expect_true(nzchar(b$time$censoring %||% ""),
              label = "the baseline does not state the censoring rule")
  expect_true(nzchar(b$time$search_window_end %||% ""),
              label = "the baseline does not state the censoring horizon")
})

test_that("the live estimand still matches the baseline", {
  skip_if_not(file.exists(report_script))
  status <- system2("Rscript", c(shQuote(report_script)),
                    stdout = TRUE, stderr = TRUE)
  code <- attr(status, "status") %||% 0

  drift_csv <- file.path(repo_root, "output", "estimand_drift.csv")
  detail <- ""
  if (file.exists(drift_csv)) {
    dr <- suppressWarnings(read_csv(drift_csv, show_col_types = FALSE))
    if (nrow(dr) && "status" %in% names(dr)) {
      ch <- dr[dr$status != "unchanged", , drop = FALSE]
      if (nrow(ch)) {
        detail <- paste0("\n  ", paste(sprintf("%s: baseline '%s' -> current '%s' (%s)",
                                               ch$component, ch$baseline, ch$current, ch$status),
                                       collapse = "\n  "))
      }
    }
  }

  expect_equal(
    code, 0,
    label = paste0(
      "the estimand has drifted from docs/estimand_baseline.yml. This is not a ",
      "numerical regression: it means the analysis is targeting a different ",
      "quantity. If intended, re-run `Rscript scripts/estimand_drift_report.R ",
      "--update-baseline` and say why in the commit message.", detail))
})

test_that("the drift report actually detects a changed estimand", {
  # A drift checker that never fires is indistinguishable from a stable
  # estimand, so the baseline is perturbed in a temp copy and the report must
  # notice. Without this the test above would pass on a broken checker forever.
  skip_if_not(file.exists(baseline_path))
  b <- read_yaml(baseline_path)
  expect_true(!is.null(b$denominator$excludes_unresolved))

  perturbed <- b
  perturbed$denominator$excludes_unresolved <- !isTRUE(b$denominator$excludes_unresolved)

  flatten_fp <- function(x, prefix = "") {
    out <- list()
    for (nm in names(x)) {
      v <- x[[nm]]
      key <- if (nzchar(prefix)) paste(prefix, nm, sep = ".") else nm
      if (is.list(v)) out <- c(out, flatten_fp(v, key))
      else out[[key]] <- paste(as.character(v), collapse = ", ")
    }
    out
  }

  a <- flatten_fp(b)
  p <- flatten_fp(perturbed)
  differing <- names(a)[vapply(names(a), function(k) !identical(a[[k]], p[[k]]), logical(1))]

  expect_true("denominator.excludes_unresolved" %in% differing,
              label = "flattening the fingerprint does not surface a changed denominator rule")
})
