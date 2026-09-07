# The boundary recovery turns twelve PDFs into twelve numbers nobody can eyeball,
# so the identity of the inputs is part of the evidence. These tests are about
# the ways a file can be the wrong file while looking right.

source(testthat::test_path("..", "..", "R", "utils_acquisition.R"))

tmp <- function(bytes, name) {
  p <- file.path(tempdir(), name)
  con <- file(p, "wb"); writeBin(charToRaw(bytes), con); close(con)
  p
}
PDF <- function(name, body = "1.4\nfake supplement\n") tmp(paste0("%PDF-", body), name)

test_that("a real PDF header is required", {
  expect_true(is_real_pdf(PDF("aagl_supplement_2015.pdf")))
  # A paywall or login page saved by a browser: plausible size, plausible name,
  # parses to nothing.
  expect_false(is_real_pdf(tmp("<!DOCTYPE html><html><body>Sign in",
                               "aagl_supplement_2016.pdf")))
})

test_that("the year is read from the filename, and its absence is detectable", {
  expect_equal(year_from_filename("aagl_supplement_2012.pdf"), 2012L)
  expect_equal(year_from_filename("jmig-2023-suppl.pdf"), 2023L)
  expect_true(is.na(year_from_filename("supplement-final-v2.pdf")))
})

test_that("a clean set of twelve reports no problems", {
  d <- file.path(tempdir(), "clean"); dir.create(d, showWarnings = FALSE)
  paths <- vapply(2012:2023, function(y) {
    p <- file.path(d, sprintf("aagl_supplement_%d.pdf", y))
    con <- file(p, "wb"); writeBin(charToRaw(paste0("%PDF-1.4\nyear ", y)), con); close(con)
    p
  }, character(1))
  expect_equal(nrow(check_acquisition(paths)), 0L)
})

test_that("HTML saved as .pdf is a blocker, not a warning", {
  p <- tmp("<html>Access denied", file.path("aagl_supplement_2014.pdf"))
  r <- check_acquisition(c(p), expected_years = 2014L)
  expect_true(any(grepl("%PDF-", r$problem)))
  expect_true(all(r$severity[grepl("%PDF-", r$problem)] == "blocker"))
})

test_that("the same bytes under two years is caught", {
  # This is the failure that would otherwise yield two identical boundaries and
  # be mistaken for agreement between congresses.
  d <- file.path(tempdir(), "dup"); dir.create(d, showWarnings = FALSE)
  body <- "%PDF-1.4\nidentical"
  a <- file.path(d, "aagl_supplement_2018.pdf"); b <- file.path(d, "aagl_supplement_2019.pdf")
  for (p in c(a, b)) { con <- file(p, "wb"); writeBin(charToRaw(body), con); close(con) }
  r <- check_acquisition(c(a, b), expected_years = c(2018L, 2019L))
  expect_true(any(grepl("identical file bytes", r$problem)))
})

test_that("a missing year is named, not merely counted", {
  p <- PDF("aagl_supplement_2022.pdf")
  r <- check_acquisition(p, expected_years = c(2021L, 2022L))
  expect_true(any(grepl("2021", r$problem)))
})

test_that("a missing validation control is its own blocker", {
  # Without 2022 or 2023 the parser cannot be checked against a known boundary,
  # so every historical boundary it produces is unverifiable.
  p <- PDF("aagl_supplement_2012.pdf")
  r <- check_acquisition(p, expected_years = c(2012L, 2022L))
  expect_true(any(grepl("validation control missing", r$problem)))
})

test_that("zero-byte files are caught", {
  p <- file.path(tempdir(), "aagl_supplement_2020.pdf")
  file.create(p)
  r <- check_acquisition(p, expected_years = 2020L)
  expect_true(any(r$problem == "zero bytes"))
})

test_that("every problem is reported, not just the first", {
  d <- file.path(tempdir(), "multi"); dir.create(d, showWarnings = FALSE)
  bad_html <- file.path(d, "aagl_supplement_2013.pdf")
  con <- file(bad_html, "wb"); writeBin(charToRaw("<html>"), con); close(con)
  noyear <- file.path(d, "supplement.pdf")
  con <- file(noyear, "wb"); writeBin(charToRaw("%PDF-1.4"), con); close(con)
  r <- check_acquisition(c(bad_html, noyear), expected_years = c(2013L, 2022L))
  expect_gte(nrow(r), 3L)   # html, no-year, missing 2022 control
})

test_that("no candidate files at all is a blocker rather than a pass", {
  r <- check_acquisition(character(0))
  expect_equal(nrow(r), 1L)
  expect_equal(r$severity, "blocker")
})

test_that("sha256 is stable and distinguishes content", {
  a <- PDF("aagl_supplement_2012.pdf", "1.4\nA\n")
  b <- PDF("aagl_supplement_2013.pdf", "1.4\nB\n")
  expect_equal(file_sha256(a), file_sha256(a))
  expect_false(identical(file_sha256(a), file_sha256(b)))
  expect_true(is.na(file_sha256(file.path(tempdir(), "does-not-exist.pdf"))))
})
