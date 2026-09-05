# The gate decides what "green" means, and it was wrong.
#
# Its classification lived inside a script that runs the entire suite, so the
# only way to exercise it was a full CI run. A bug in it -- a skipped test
# counted as a passing one -- took main red with no correct fix available: the
# gate demanded the removal of a manifest entry whose test had never run,
# because the PubMed XML cache is gitignored and the test skips with "no cache"
# on a fresh checkout. Removing the entry would have been wrong; it is accurate
# locally, where the cache exists.
#
# The rules are now in tests/gate_rules.R and tested against synthetic results.

library(testthat)
source(here::here("tests", "gate_rules.R"))

mk <- function(...) {
  rows <- list(...)
  do.call(rbind, lapply(rows, function(r) data.frame(
    file = r$file, test = r$test,
    failed = r$failed %||% 0, error = r$error %||% 0, skipped = r$skipped %||% 0,
    stringsAsFactors = FALSE)))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

K <- gate_key

test_that("a skipped test is not treated as passing", {
  df <- mk(list(file = "test-a.R", test = "skips in CI", skipped = 1))
  cls <- gate_classify(df, expected = K("test-a.R", "skips in CI"))

  # This is the regression. It used to appear in `stale`, which demanded that a
  # correct manifest entry be deleted.
  expect_length(cls$stale, 0)
  expect_length(cls$orphaned, 0)   # it ran, so it is not orphaned either
  expect_equal(cls$skipped_entries, K("test-a.R", "skips in CI"))
})

test_that("a genuinely passing expected-failure is still flagged stale", {
  df <- mk(list(file = "test-a.R", test = "now passes"))
  cls <- gate_classify(df, expected = K("test-a.R", "now passes"))
  expect_equal(cls$stale, K("test-a.R", "now passes"))
  expect_length(cls$skipped_entries, 0)
})

test_that("an unexpected failure is reported and an expected one is not", {
  df <- mk(list(file = "test-a.R", test = "known", failed = 1),
           list(file = "test-b.R", test = "regression", failed = 1))
  cls <- gate_classify(df, expected = K("test-a.R", "known"))
  expect_equal(cls$unexpected, K("test-b.R", "regression"))
  expect_setequal(cls$failed_keys, c(K("test-a.R", "known"), K("test-b.R", "regression")))
})

test_that("an entry naming a test that never ran is orphaned", {
  df <- mk(list(file = "test-a.R", test = "present"))
  cls <- gate_classify(df, expected = K("test-a.R", "renamed away"))
  expect_equal(cls$orphaned, K("test-a.R", "renamed away"))
  expect_length(cls$stale, 0)
})

test_that("an error counts as a failure, not a pass", {
  df <- mk(list(file = "test-a.R", test = "errors", error = 1))
  cls <- gate_classify(df, expected = character(0))
  expect_equal(cls$unexpected, K("test-a.R", "errors"))
  expect_length(cls$stale, 0)
})

test_that("a test that both failed and skipped is a failure", {
  # testthat can record both when a test_that block skips after an assertion
  # has already failed. Failure has to win, or a regression could hide behind a
  # later skip.
  df <- mk(list(file = "test-a.R", test = "mixed", failed = 1, skipped = 1))
  cls <- gate_classify(df, expected = character(0))
  expect_equal(cls$unexpected, K("test-a.R", "mixed"))
  expect_length(cls$stale, 0)
})

test_that("an empty run orphans every entry rather than declaring them stale", {
  cls <- gate_classify(data.frame(), expected = K("test-a.R", "anything"))
  expect_equal(cls$orphaned, K("test-a.R", "anything"))
  expect_length(cls$stale, 0)
})

test_that("results without a skipped column still classify", {
  df <- data.frame(file = "test-a.R", test = "no skip col",
                   failed = 0, error = 0, stringsAsFactors = FALSE)
  cls <- gate_classify(df, expected = K("test-a.R", "no skip col"))
  expect_equal(cls$stale, K("test-a.R", "no skip col"))
})

test_that("the gate script uses the shared rules rather than its own copy", {
  txt <- paste(readLines(here::here("tests", "run_suite_gate.R"), warn = FALSE),
               collapse = "\n")
  expect_true(grepl("gate_rules.R", txt, fixed = TRUE))
  expect_true(grepl("gate_classify(", txt, fixed = TRUE))
})
