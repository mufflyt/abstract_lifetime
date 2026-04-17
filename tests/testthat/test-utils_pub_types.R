# Tests for utils_pub_types.R

library(testthat)
source(here::here("R", "utils_pub_types.R"))

test_that("Journal Article is the default", {
  expect_equal(canonical_pub_type("Journal Article"), "Journal Article")
  expect_equal(canonical_pub_type("Journal Article; Comparative Study"), "Journal Article")
  expect_equal(canonical_pub_type("Journal Article; Multicenter Study"), "Journal Article")
})

test_that("Reviews are classified correctly", {
  expect_equal(canonical_pub_type("Review"), "Review")
  expect_equal(canonical_pub_type("Journal Article; Systematic Review"), "Review")
  expect_equal(canonical_pub_type("Journal Article; Meta-Analysis"), "Review")
  expect_equal(canonical_pub_type("Scoping Review"), "Review")
})

test_that("RCTs and trials are classified correctly", {
  expect_equal(canonical_pub_type("Journal Article; Randomized Controlled Trial"), "RCT/Trial")
  expect_equal(canonical_pub_type("Clinical Trial; Journal Article"), "RCT/Trial")
  expect_equal(canonical_pub_type("Equivalence Trial; Journal Article"), "RCT/Trial")
})

test_that("Case reports are classified correctly", {
  expect_equal(canonical_pub_type("Case Reports"), "Case Report")
  expect_equal(canonical_pub_type("Journal Article; Case Reports"), "Case Report")
})

test_that("Editorials and letters are classified correctly", {
  expect_equal(canonical_pub_type("Editorial"), "Editorial/Letter")
  expect_equal(canonical_pub_type("Letter"), "Editorial/Letter")
  expect_equal(canonical_pub_type("Comment"), "Editorial/Letter")
})

test_that("Observational studies are classified", {
  expect_equal(canonical_pub_type("Journal Article; Observational Study"), "Observational Study")
})

test_that("Priority order: Review > RCT > Case Report > Editorial", {
  expect_equal(
    canonical_pub_type("Journal Article; Randomized Controlled Trial; Systematic Review"),
    "Review"
  )
  expect_equal(
    canonical_pub_type("Journal Article; Case Reports; Randomized Controlled Trial"),
    "RCT/Trial"
  )
})

test_that("NA and empty input handled", {
  expect_equal(canonical_pub_type(NA), NA_character_)
  expect_equal(canonical_pub_type(""), NA_character_)
})
