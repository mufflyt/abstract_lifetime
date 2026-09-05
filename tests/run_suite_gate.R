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
#   * any test fails that is NOT in tests/expected_failures.yaml,
#   * any test IN that manifest passes, or
#   * any entry IN that manifest names a test the suite never ran.
#
# The last two rules stop the manifest rotting. When a decision is finally
# taken and the assertion starts holding, CI goes red until the entry is
# removed. And when a test is renamed or deleted, its entry would otherwise sit
# there forever describing a decision that no longer has an assertion behind
# it, which is how a manifest quietly turns into a list of excuses.
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

# The rules below are declared in config/ci_contract.yml so that this script,
# tests.yaml and R-CMD-check.yaml cannot disagree about what green means. The
# fallbacks keep the gate working if the contract is ever absent, but
# tests/testthat/test-ci_contract.R fails in that case.
`%||%` <- function(a, b) if (is.null(a)) b else a
contract_path <- "config/ci_contract.yml"
contract <- if (file.exists(contract_path)) yaml::read_yaml(contract_path) else list()
mrules <- contract$manifest %||% list()

manifest_path <- mrules$path %||% "tests/expected_failures.yaml"
rule_unexpected <- isTRUE(mrules$fail_on_unexpected_failure %||% TRUE)
rule_stale      <- isTRUE(mrules$fail_on_stale_entry %||% TRUE)
rule_orphaned   <- isTRUE(mrules$fail_on_orphaned_entry %||% TRUE)
max_entries     <- mrules$max_entries %||% Inf

manifest <- if (file.exists(manifest_path)) {
  yaml::read_yaml(manifest_path)$expected_failures
} else {
  list()
}

# The approved-skip manifest. A skipped test asserts nothing, so an unapproved
# skip is a test that stopped covering something without anyone saying so.
srules <- contract$skips %||% list()
skip_manifest_path <- srules$path %||% "tests/expected_skips.yaml"
rule_unapproved_skip <- isTRUE(srules$fail_on_unapproved_skip %||% TRUE)
skip_manifest <- if (file.exists(skip_manifest_path)) {
  yaml::read_yaml(skip_manifest_path)$expected_skips
} else {
  list()
}

key <- function(file, test) paste(file, test, sep = " :: ")
expected_keys <- if (length(manifest) > 0) {
  key(vapply(manifest, `[[`, character(1), "file"),
      vapply(manifest, `[[`, character(1), "test"))
} else character(0)

approved_skip_keys <- if (length(skip_manifest) > 0) {
  key(vapply(skip_manifest, `[[`, character(1), "file"),
      vapply(skip_manifest, `[[`, character(1), "test"))
} else character(0)

# Both workflows invoke this from the repository root; the fallback keeps it
# working if that ever stops being true.
gate_rules <- if (file.exists("tests/gate_rules.R")) "tests/gate_rules.R" else
  file.path(here::here(), "tests", "gate_rules.R")
source(gate_rules)

failed <- df[df$failed > 0 | df$error > 0, , drop = FALSE]
cls <- gate_classify(df, expected_keys)
scls <- skip_classify(df, approved_skip_keys)
failed_keys     <- cls$failed_keys
stale           <- cls$stale
orphaned        <- cls$orphaned
skipped_entries <- cls$skipped_entries

if (nrow(failed) > 0) {
  cat("\n--- failures ---\n")
  for (i in seq_len(nrow(failed))) {
    tag <- if (failed_keys[i] %in% expected_keys) "EXPECTED  " else "UNEXPECTED"
    cat(sprintf("  [%s] %s :: %s\n", tag, failed$file[i], failed$test[i]))
  }
}

problems <- character()

unexpected <- cls$unexpected
if (rule_unexpected && length(unexpected) > 0) {
  problems <- c(problems, sprintf(
    "%d unexpected failure(s). Fix them, or add each to %s with a reason and the decision it is waiting on.",
    length(unexpected), manifest_path))
}

if (rule_stale && length(stale) > 0) {
  cat("\n--- stale manifest entries (these now PASS) ---\n")
  cat(paste0("  ", stale, collapse = "\n"), "\n")
  problems <- c(problems, sprintf(
    "%d expected-failure entr%s now pass. Remove them from %s.",
    length(stale), if (length(stale) == 1) "y" else "ies", manifest_path))
}

if (rule_orphaned && length(orphaned) > 0) {
  cat("\n--- orphaned manifest entries (no such test ran) ---\n")
  cat(paste0("  ", orphaned, collapse = "\n"), "\n")
  problems <- c(problems, sprintf(
    "%d manifest entr%s name a test that never ran. Re-point it at the renamed test, or remove it from %s.",
    length(orphaned), if (length(orphaned) == 1) "y does" else "ies", manifest_path))
}

# Every skip is printed with the reason recorded for it, so an intentional
# skip is visible as a gap rather than blending into the pass count.
if (length(scls$skipped_keys) > 0) {
  reasons <- setNames(
    vapply(skip_manifest, function(e) e$reason %||% "", character(1)),
    key(vapply(skip_manifest, `[[`, character(1), "file"),
        vapply(skip_manifest, `[[`, character(1), "test")))
  cat("\n--- skipped: these assert nothing in this environment ---\n")
  for (k in sort(scls$skipped_keys)) {
    r <- reasons[[k]]
    cat(sprintf("  %s%s\n", k,
                if (is.null(r) || !nzchar(r)) "  [NOT APPROVED]" else
                  paste0("\n      reason: ", trimws(gsub("\\s+", " ", r)))))
  }
}

if (rule_unapproved_skip && length(scls$unapproved) > 0) {
  problems <- c(problems, sprintf(
    paste("%d test(s) skipped without an entry in %s. A skipped test asserts",
          "nothing; add each with a reason and how it would be made to run, or",
          "make it run."),
    length(scls$unapproved), skip_manifest_path))
}

# Not enforced. The skip set is environment-dependent by design: the Shiny
# bundle and the PubMed cache exist on a developer machine and not in a clean
# checkout, so an entry that legitimately skips in CI runs locally. Reported so
# the list can still be pruned deliberately.
if (length(scls$did_not_skip) > 0) {
  cat("\n--- approved skips that RAN here (informational) ---\n")
  cat(paste0("  ", sort(scls$did_not_skip), collapse = "\n"), "\n")
}

if (length(manifest) > max_entries) {
  problems <- c(problems, sprintf(
    paste("the expected-failure manifest holds %d entries, above the %s allowed by",
          "%s. It is meant to shrink: close a decision, or raise max_entries",
          "deliberately and say why."),
    length(manifest), format(max_entries), contract_path))
}

if (length(problems) > 0) {
  cat("\n")
  stop(paste(problems, collapse = "\n"), call. = FALSE)
}

if (length(skipped_entries) > 0) {
  cat("\n--- manifest entries whose test skipped (not judged either way) ---\n")
  cat(paste0("  ", skipped_entries, collapse = "\n"), "\n")
}

cat(sprintf("\nSuite green: %d failure(s), all on the expected-failure manifest.\n",
            nrow(failed)))
