# Tests for utils_acog.R

library(testthat)
source(here::here("R", "utils_acog.R"))

test_that("major states map to correct ACOG districts", {
  expect_equal(acog_district_for_state("NY"), "II")
  expect_equal(acog_district_for_state("CA"), "IX")
  expect_equal(acog_district_for_state("TX"), "X")
  expect_equal(acog_district_for_state("FL"), "XI")
  expect_equal(acog_district_for_state("MA"), "I")
  expect_equal(acog_district_for_state("PA"), "III")
  expect_equal(acog_district_for_state("DC"), "IV")
  expect_equal(acog_district_for_state("OH"), "V")
  expect_equal(acog_district_for_state("MN"), "VI")
  expect_equal(acog_district_for_state("IL"), "VII")
  expect_equal(acog_district_for_state("CO"), "VIII")
})

test_that("Armed Forces codes return AFS", {
  expect_equal(acog_district_for_state("AA"), "AFS")
  expect_equal(acog_district_for_state("AE"), "AFS")
  expect_equal(acog_district_for_state("AP"), "AFS")
})

test_that("territories map correctly", {
  expect_equal(acog_district_for_state("PR"), "XI")
  expect_equal(acog_district_for_state("HI"), "VIII")
  expect_equal(acog_district_for_state("AK"), "VIII")
})

test_that("Canadian provinces map correctly", {
  expect_equal(acog_district_for_state("ON"), "II")
  expect_equal(acog_district_for_state("BC"), "VIII")
  expect_equal(acog_district_for_state("NS"), "I")
})

test_that("NA and empty return NA", {
  expect_equal(acog_district_for_state(NA), NA_character_)
  expect_equal(acog_district_for_state(""), NA_character_)
})

test_that("unknown codes return NA", {
  expect_equal(acog_district_for_state("ZZ"), NA_character_)
  expect_equal(acog_district_for_state("XX"), NA_character_)
})

test_that("case insensitive input works", {
  expect_equal(acog_district_for_state("ny"), "II")
  expect_equal(acog_district_for_state("ca"), "IX")
})
