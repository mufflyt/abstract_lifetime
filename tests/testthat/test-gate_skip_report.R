# The unapproved-skip report is the gate's only way to name a test that stopped
# asserting. It was written so that the one case it exists for -- a skip with no
# manifest entry -- threw "subscript out of bounds" before reaching the
# [NOT APPROVED] branch. The gate still exited non-zero, so nothing unapproved
# ever slipped through, but the operator was handed an R error instead of the
# name of the offending test. These tests pin the reporting behaviour itself.

source(testthat::test_path("..", "gate_rules.R"))

entry <- function(file, test, reason) list(file = file, test = test, reason = reason)

MANIFEST <- list(
  entry("test-shiny_e2e.R", "the app boots", "shinytest2 absent in CI"),
  entry("test-pubmed.R", "cache is fresh", "PubMed cache is not committed")
)

test_that("an unapproved skip is named and marked, not thrown", {
  out <- gate_skip_report("test-new.R :: a brand new skip", MANIFEST)
  expect_length(out, 1)
  expect_match(out, "test-new.R :: a brand new skip", fixed = TRUE)
  expect_match(out, "[NOT APPROVED]", fixed = TRUE)
})

test_that("an approved skip is reported with its recorded reason", {
  out <- gate_skip_report("test-pubmed.R :: cache is fresh", MANIFEST)
  expect_match(out, "reason: PubMed cache is not committed", fixed = TRUE)
  expect_false(grepl("NOT APPROVED", out, fixed = TRUE))
})

test_that("approved and unapproved skips are reported together", {
  # The mixed case is the one that matters: an unapproved skip appearing
  # alongside approved ones must not suppress the whole report.
  out <- gate_skip_report(
    c("test-pubmed.R :: cache is fresh", "test-new.R :: a brand new skip"),
    MANIFEST)
  expect_length(out, 2)
  expect_equal(sum(grepl("NOT APPROVED", out, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("reason:", out, fixed = TRUE)), 1L)
})

test_that("an empty manifest marks every skip unapproved rather than erroring", {
  # setNames(character(0), character(0))[["k"]] is also subscript out of bounds,
  # so a missing tests/expected_skips.yaml crashed the gate the same way.
  out <- gate_skip_report(c("test-a.R :: x", "test-b.R :: y"), list())
  expect_length(out, 2)
  expect_true(all(grepl("NOT APPROVED", out, fixed = TRUE)))
})

test_that("an entry carrying no reason counts as unapproved", {
  out <- gate_skip_report("test-x.R :: y", list(entry("test-x.R", "y", "")))
  expect_match(out, "[NOT APPROVED]", fixed = TRUE)
})

test_that("no skips produces no report lines", {
  expect_length(gate_skip_report(character(0), MANIFEST), 0L)
})

test_that("the report is sorted so its output is stable across runs", {
  out <- gate_skip_report(c("test-z.R :: z", "test-a.R :: a"), list())
  expect_match(out[1], "test-a.R", fixed = TRUE)
})

test_that("run_suite_gate.R does not index the reason lookup with [[", {
  # A guard on the shape of the fix, not just its behaviour: restoring
  # reasons[[k]] reintroduces the crash even if the helper stays correct.
  src <- readLines(testthat::test_path("..", "run_suite_gate.R"), warn = FALSE)
  expect_false(any(grepl("reasons\\[\\[", src)),
               info = "run_suite_gate.R must report skips via gate_skip_report()")
})
