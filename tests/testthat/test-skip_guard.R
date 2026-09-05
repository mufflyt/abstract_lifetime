# The skip guard, exercised against synthetic results.
#
# skip_classify() is the rule that decides whether a skipped test is an
# approved gap or an unannounced loss of coverage. It lives in gate_rules.R
# rather than inside run_suite_gate.R for the same reason gate_classify() does:
# a rule that can only be exercised by a full CI run is a rule whose bugs are
# found in production. The bug that motivated this one (a skip counted as a
# pass) took main red for two days with no correct fix available.

library(testthat)
source(here::here("tests", "gate_rules.R"))

`%||%` <- function(a, b) if (is.null(a)) b else a

mk <- function(file, test, passed = 1, failed = 0, error = FALSE, skipped = 0) {
  data.frame(file = file, test = test, passed = passed, failed = failed,
             error = error, skipped = skipped, stringsAsFactors = FALSE)
}

test_that("an unapproved skip is reported as unapproved", {
  df <- rbind(mk("a.R", "runs"), mk("b.R", "skips", passed = 0, skipped = 1))
  r <- skip_classify(df, approved = character(0))
  expect_equal(r$skipped_keys, "b.R :: skips")
  expect_equal(r$unapproved, "b.R :: skips")
})

test_that("an approved skip is not reported as unapproved", {
  df <- rbind(mk("a.R", "runs"), mk("b.R", "skips", passed = 0, skipped = 1))
  r <- skip_classify(df, approved = "b.R :: skips")
  expect_equal(r$unapproved, character(0))
  expect_equal(r$did_not_skip, character(0))
})

test_that("an approved skip that ran is reported but not treated as a failure", {
  # The case that must NOT fail: the Shiny bundle exists locally and not in CI,
  # so the same entry legitimately skips in one place and runs in the other.
  df <- rbind(mk("a.R", "runs"), mk("b.R", "skips"))
  r <- skip_classify(df, approved = "b.R :: skips")
  expect_equal(r$unapproved, character(0))
  expect_equal(r$did_not_skip, "b.R :: skips")
})

test_that("a test that both partly skips and partly passes counts as skipped", {
  # testthat records one row per test_that block; a block that skips partway
  # still carries whatever assertions passed before the skip. Treating it as
  # covered would be exactly the error this guard exists to prevent.
  df <- mk("a.R", "partial", passed = 3, skipped = 1)
  r <- skip_classify(df, approved = character(0))
  expect_equal(r$unapproved, "a.R :: partial")
})

test_that("an empty run approves nothing and strands every entry", {
  r <- skip_classify(df = data.frame(), approved = c("a.R :: x", "b.R :: y"))
  expect_equal(r$skipped_keys, character(0))
  expect_equal(r$unapproved, character(0))
  expect_equal(r$did_not_skip, c("a.R :: x", "b.R :: y"))
})

test_that("results with no skipped column are treated as nothing skipped", {
  df <- data.frame(file = "a.R", test = "t", passed = 1, failed = 0,
                   error = FALSE, stringsAsFactors = FALSE)
  r <- skip_classify(df, approved = character(0))
  expect_equal(r$unapproved, character(0))
})

test_that("the skip manifest exists and every entry is fully documented", {
  p <- here::here("tests", "expected_skips.yaml")
  expect_true(file.exists(p))
  skip_if_not(file.exists(p), "manifest absent")
  m <- yaml::yaml.load_file(p)$expected_skips
  expect_true(length(m) > 0)
  # A bare test name would turn the manifest into a list of excuses. Every
  # entry has to say why it cannot run and what would make it run.
  incomplete <- character(0)
  for (e in m) {
    missing <- setdiff(c("file", "test", "reason", "to_enable"), names(e))
    if (length(missing)) {
      incomplete <- c(incomplete, sprintf("%s :: %s (missing %s)",
                                          e$file %||% "?", e$test %||% "?",
                                          paste(missing, collapse = ", ")))
    }
  }
  expect_true(length(incomplete) == 0,
              label = paste("skip manifest entries missing required fields:",
                            paste(incomplete, collapse = "; ")))
})

test_that("every skip manifest entry names a test file that exists", {
  p <- here::here("tests", "expected_skips.yaml")
  skip_if_not(file.exists(p), "manifest absent")
  m <- yaml::yaml.load_file(p)$expected_skips
  missing <- unique(vapply(m, `[[`, character(1), "file"))
  missing <- missing[!file.exists(file.path(here::here("tests", "testthat"), missing))]
  expect_true(length(missing) == 0,
              label = paste("skip manifest names test files that do not exist:",
                            paste(missing, collapse = ", ")))
})
