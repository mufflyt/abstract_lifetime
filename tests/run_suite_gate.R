#!/usr/bin/env Rscript
# run_suite_gate.R — Run the test suite and decide whether it passes.
#
# This repository preserves failing tests on purpose. Each marks an open
# scientific or methodological decision that belongs to the author, and
# weakening the assertion would hide the question. See tests/loop/LEDGER.md and
# docs/VALIDATION.md.
#
# Failing the build on all of them meant CI had never once been green, so it
# could not signal anything: a genuine regression looked exactly like the
# standing set. This gate fails when
#
#   * any test fails that is NOT in tests/expected_failures.yaml, or
#   * any test IN that manifest passes.
#
# The second rule stops the manifest rotting. When a decision is finally taken
# and the assertion starts holding, CI goes red until the entry is removed.
#
# Both .github/workflows/tests.yaml and .github/workflows/R-CMD-check.yaml call
# this, so the two cannot drift apart on what "green" means.
#
# Usage: Rscript tests/run_suite_gate.R
# Exit:  0 green, 1 red.

suppressPackageStartupMessages({
  library(testthat); library(yaml)
})

res <- test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
df  <- as.data.frame(res)

cat("\n--- suite summary ---\n")
cat("files:", length(unique(df$file)),
    " passed:", sum(df$passed),
    " failed:", sum(df$failed),
    " errors:", sum(df$error),
    " skipped:", sum(df$skipped), "\n")

manifest_path <- "tests/expected_failures.yaml"
manifest <- if (file.exists(manifest_path)) {
  yaml::read_yaml(manifest_path)$expected_failures
} else {
  list()
}

key <- function(file, test) paste(file, test, sep = " :: ")
expected_keys <- if (length(manifest) > 0) {
  key(vapply(manifest, `[[`, character(1), "file"),
      vapply(manifest, `[[`, character(1), "test"))
} else character(0)

failed <- df[df$failed > 0 | df$error > 0, , drop = FALSE]
failed_keys <- key(failed$file, failed$test)

passing <- df[df$failed == 0 & df$error == 0, , drop = FALSE]
stale <- expected_keys[expected_keys %in% key(passing$file, passing$test)]

if (nrow(failed) > 0) {
  cat("\n--- failures ---\n")
  for (i in seq_len(nrow(failed))) {
    tag <- if (failed_keys[i] %in% expected_keys) "EXPECTED  " else "UNEXPECTED"
    cat(sprintf("  [%s] %s :: %s\n", tag, failed$file[i], failed$test[i]))
  }
}

problems <- character()

unexpected <- failed_keys[!failed_keys %in% expected_keys]
if (length(unexpected) > 0) {
  problems <- c(problems, sprintf(
    "%d unexpected failure(s). Fix them, or add each to %s with a reason and the decision it is waiting on.",
    length(unexpected), manifest_path))
}

if (length(stale) > 0) {
  cat("\n--- stale manifest entries (these now PASS) ---\n")
  cat(paste0("  ", stale, collapse = "\n"), "\n")
  problems <- c(problems, sprintf(
    "%d expected-failure entr%s now pass. Remove them from %s.",
    length(stale), if (length(stale) == 1) "y" else "ies", manifest_path))
}

if (length(problems) > 0) {
  cat("\n")
  stop(paste(problems, collapse = "\n"), call. = FALSE)
}

cat(sprintf("\nSuite green: %d failure(s), all on the expected-failure manifest.\n",
            nrow(failed)))
