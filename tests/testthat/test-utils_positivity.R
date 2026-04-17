# Tests for utils_positivity.R

library(testthat)
source(here::here("R", "utils_positivity.R"))

test_that("positive results classified correctly", {
  expect_equal(
    classify_result_positivity("The intervention significantly improved outcomes (p < 0.001)."),
    "positive"
  )
  expect_equal(
    classify_result_positivity("Laparoscopic approach was superior to open surgery with reduced blood loss."),
    "positive"
  )
  expect_equal(
    classify_result_positivity("Treatment was effective in reducing pain scores (p = 0.02)."),
    "positive"
  )
})

test_that("negative results classified correctly", {
  expect_equal(
    classify_result_positivity("There was no significant difference between the two groups (p = 0.45)."),
    "negative"
  )
  expect_equal(
    classify_result_positivity("The intervention did not improve outcomes compared to placebo."),
    "negative"
  )
  expect_equal(
    classify_result_positivity("No association was found between the exposure and the outcome."),
    "negative"
  )
})

test_that("neutral results classified correctly", {
  expect_equal(
    classify_result_positivity("The procedure was feasible and safe. Further study is needed."),
    "neutral"
  )
  expect_equal(
    classify_result_positivity("This preliminary pilot study shows trends toward improvement."),
    "neutral"
  )
})

test_that("unclear when text is missing or too short", {
  expect_equal(classify_result_positivity(NA), "unclear")
  expect_equal(classify_result_positivity(""), "unclear")
  expect_equal(classify_result_positivity("Short."), "unclear")
})

test_that("mixed signals resolve by majority", {
  result <- classify_result_positivity(
    "Results showed no significant difference in primary outcome, but the intervention significantly reduced secondary complications."
  )
  expect_true(result %in% c("positive", "negative", "neutral"))
})
