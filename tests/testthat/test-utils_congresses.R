# Tests for utils_congresses.R

library(testthat)
source(here::here("R", "utils_congresses.R"))

mock_cfg <- list(
  congresses = list(
    list(year = 2021, date = "2021-11-14"),
    list(year = 2022, date = "2022-11-06"),
    list(year = 2023, date = "2023-11-07")
  ),
  conference = list(date = "2023-11-07")
)

test_that("congress_date_lookup returns named date vector", {
  lkp <- congress_date_lookup(mock_cfg)
  expect_equal(length(lkp), 3)
  expect_equal(names(lkp), c("2021", "2022", "2023"))
  expect_s3_class(lkp, "Date")
  expect_equal(unname(lkp[1]), as.Date("2021-11-14"))
})

test_that("conference_date_for returns correct date per year", {
  d2021 <- conference_date_for(2021, mock_cfg)
  expect_equal(d2021, as.Date("2021-11-14"))

  d2023 <- conference_date_for(2023, mock_cfg)
  expect_equal(d2023, as.Date("2023-11-07"))
})

test_that("conference_date_for handles vector input", {
  dates <- conference_date_for(c(2021, 2023, 2022), mock_cfg)
  expect_equal(length(dates), 3)
  expect_equal(dates[1], as.Date("2021-11-14"))
  expect_equal(dates[2], as.Date("2023-11-07"))
  expect_equal(dates[3], as.Date("2022-11-06"))
})

test_that("unknown year falls back to legacy config", {
  d <- conference_date_for(2019, mock_cfg)
  expect_equal(d, as.Date("2023-11-07"))
})

test_that("NA year falls back to legacy", {
  d <- conference_date_for(NA, mock_cfg)
  expect_equal(d, as.Date("2023-11-07"))
})

test_that("legacy-only config works when congresses is NULL", {
  legacy_cfg <- list(conference = list(date = "2023-11-07"))
  d <- conference_date_for(2023, legacy_cfg)
  expect_equal(d, as.Date("2023-11-07"))
})
