# The ToC parser exists to supply one number per congress: the page at which the
# oral block ends. Ten congresses are truncated and cannot be completed without
# it, so the failure that matters is not "wrong page" but "confidently wrong
# page". These tests are mostly about the ways a heading can be misread.

source(testthat::test_path("..", "..", "R", "utils_toc.R"))

test_that("printed heading variants all collapse to one family", {
  expect_equal(normalize_session_label("Oral Presentations"), "Oral")
  expect_equal(normalize_session_label("ORAL PRESENTATIONS"), "Oral")
  expect_equal(normalize_session_label("Video Sessions"), "Video")      # 2022
  expect_equal(normalize_session_label("Video Presentations"), "Video") # 2023
  expect_equal(normalize_session_label("Poster Session I"), "Poster")
  expect_true(is.na(normalize_session_label("Open Communications 4")))
  expect_true(is.na(normalize_session_label("")))
})

test_that("an ambiguous heading resolves to neither family", {
  # "Oral and Video Presentations" must not be silently filed as Oral. Guessing
  # here would move the boundary and no downstream check would catch it.
  expect_true(is.na(normalize_session_label("Oral and Video Presentations")))
})

test_that("S-page numbers are read, and non-pages are not", {
  expect_equal(toc_page_number(c("x S1", "y S37", "z Pages S12-S13")),
               c(1L, 37L, 12L))
  expect_equal(toc_page_number("no page here"), NA_integer_)
  expect_equal(toc_page_number(character(0)), integer(0))
  # A bare number is not an S-page; supplements paginate S1.. and abstracts are
  # also numbered 1.., so reading "44." as page 44 would be catastrophic.
  expect_equal(toc_page_number("44. A study title"), NA_integer_)
})

test_that("the boundary is found in a supplement-shaped table of contents", {
  toc <- c("Oral Presentations",
           "1. A study .......... S1",
           "Open Communications 4",       # sub-heading, still oral
           "44. Third ........... S30",
           "Video Sessions",
           "91. A video ......... S37",
           "92. Another ......... S40")
  b <- find_session_boundaries(toc)
  expect_equal(b$section, c("Oral", "Video"))
  expect_equal(b$last_page[b$section == "Oral"], 30L)
  expect_equal(b$first_page[b$section == "Video"], 37L)
})

test_that("a session split across repeated headings is merged, not double-counted", {
  toc <- c("Oral Presentations", "1. a ... S1",
           "Oral Presentations", "2. b ... S20",
           "Video Presentations", "3. c ... S30")
  b <- find_session_boundaries(toc)
  expect_equal(nrow(b), 2L)
  expect_equal(b$last_page[b$section == "Oral"], 20L)
  expect_equal(b$n_entries[b$section == "Oral"], 2L)
})

test_that("a cross-reference in running text is not mistaken for a heading", {
  toc <- c("Oral Presentations", "1. a ... S1",
           "see Video Presentations beginning on page S37 for related work",
           "2. b ... S10")
  b <- find_session_boundaries(toc)
  expect_equal(nrow(b), 1L)
  expect_equal(b$section, "Oral")
  expect_equal(b$last_page, 37L)   # the cross-reference page is still text
})

test_that("no recognised heading returns zero rows rather than a guess", {
  # The caller must be able to tell "parser failed" from "supplement has one
  # session". Returning a plausible single block would hide a format change.
  b <- find_session_boundaries(c("Table of Contents", "1. a ... S1"))
  expect_equal(nrow(b), 0L)
})

test_that("a heading with too few entries is flagged rather than trusted", {
  toc <- c("Oral Presentations", "1. a ... S1", "2. b ... S2", "3. c ... S3",
           "Poster Presentations", "4. d ... S99")
  b <- find_session_boundaries(toc, min_items = 3L)
  expect_false(b$sparse[b$section == "Oral"])
  expect_true(b$sparse[b$section == "Poster"])
})

test_that("blocks come back in printed order", {
  toc <- c("Video Presentations", "1. v ... S37",
           "Oral Presentations", "2. o ... S1")
  b <- find_session_boundaries(toc)
  expect_equal(b$section, c("Oral", "Video"))
})
