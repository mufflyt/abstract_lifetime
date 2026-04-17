# Tests for utils_states.R

library(testthat)
source(here::here("R", "utils_states.R"))

test_that("spelled-out state names are resolved", {
  expect_equal(parse_us_state("Department of OB/GYN, Springfield, Illinois."), "IL")
  expect_equal(parse_us_state("Montefiore Medical Center, New York, New York."), "NY")
  expect_equal(parse_us_state("University of Texas, Houston, Texas, USA."), "TX")
  expect_equal(parse_us_state("Magee-Womens Hospital, Pittsburgh, Pennsylvania."), "PA")
  expect_equal(parse_us_state("Boston, Massachusetts, USA."), "MA")
})

test_that("2-letter code with ZIP is resolved", {
  expect_equal(parse_us_state("West Bloomfield, MI 48322, USA"), "MI")
  expect_equal(parse_us_state("Nanjing, 210096, China"), NA_character_)
})

test_that("city lookup works", {
  expect_equal(parse_us_state("Department of Surgery, Cedars-Sinai, Los Angeles"), "CA")
  expect_equal(parse_us_state("Henry Ford Hospital, Detroit"), "MI")
})

test_that("institution lookup works", {
  expect_equal(parse_us_state("Mayo Clinic, Rochester"), "MN")
  expect_equal(parse_us_state("Cleveland Clinic Foundation"), "OH")
  expect_equal(parse_us_state("Johns Hopkins University School of Medicine"), "MD")
  expect_equal(parse_us_state("Icahn School of Medicine at Mount Sinai"), "NY")
  expect_equal(parse_us_state("Magee-Womens Hospital, UPMC"), "PA")
})

test_that("non-US affiliations return NA", {
  expect_equal(parse_us_state("Nagoya University, Nagoya, Japan."), NA_character_)
  expect_equal(parse_us_state("Shanghai Chest Hospital, China."), NA_character_)
  expect_equal(parse_us_state("Sapienza University, Rome, Italy."), NA_character_)
})

test_that("NA and empty handled", {
  expect_equal(parse_us_state(NA), NA_character_)
  expect_equal(parse_us_state(""), NA_character_)
})

test_that("is_us_affiliation works", {
  expect_true(is_us_affiliation("Department of OB/GYN, Boston, Massachusetts, USA"))
  expect_false(is_us_affiliation("University of Tokyo, Japan"))
  expect_false(is_us_affiliation("Sapienza University, Rome, Italy"))
  expect_true(is_us_affiliation("Cleveland Clinic Foundation"))
})

test_that("West Virginia distinguished from Virginia", {
  expect_equal(parse_us_state("WVU Medicine, Morgantown, West Virginia"), "WV")
  expect_equal(parse_us_state("University of Virginia, Charlottesville, Virginia"), "VA")
})
