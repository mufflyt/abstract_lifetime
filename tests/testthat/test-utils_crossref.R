# Tests for utils_crossref.R — unit tests (no network calls)

library(testthat)
source(here::here("R", "utils_crossref.R"))

test_that("search_crossref handles empty/NA input", {
  result <- search_crossref(NA)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)

  result2 <- search_crossref("")
  expect_equal(nrow(result2), 0)

  result3 <- search_crossref("ab")  # Too short
  expect_equal(nrow(result3), 0)
})

test_that("search_europmc handles empty/NA input", {
  result <- search_europmc(NA)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)

  result2 <- search_europmc("")
  expect_equal(nrow(result2), 0)
})
