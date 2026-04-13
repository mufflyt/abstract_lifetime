# Tests for utils_text.R

library(testthat)
source(here::here("R", "utils_text.R"))

test_that("normalize_title works correctly", {
  expect_equal(
    normalize_title("Effect of Laparoscopic Surgery on Outcomes!"),
    "effect of laparoscopic surgery on outcomes"
  )
  expect_equal(normalize_title("  Multiple   Spaces  "), "multiple spaces")
  expect_equal(normalize_title("Title-With-Hyphens"), "title with hyphens")
})

test_that("normalize_author handles various formats", {
  expect_equal(normalize_author("Smith, John"), "smith J")
  expect_equal(normalize_author("John Smith"), "smith J")
  expect_equal(normalize_author("J Smith"), "smith J")      # FI LastName format
  expect_equal(normalize_author("DA Lum"), "lum D")          # Two-letter initials
  expect_equal(normalize_author("van der Berg, Anna"), "van der berg A")
  expect_equal(normalize_author(NA), NA_character_)
  expect_equal(normalize_author(""), NA_character_)
})

test_that("jaccard_similarity computes correctly", {
  # Identical strings
  expect_equal(jaccard_similarity("hello world", "hello world"), 1.0)
  # Completely different
  expect_equal(jaccard_similarity("cat dog", "fish bird"), 0.0)
  # Partial overlap
  sim <- jaccard_similarity("hello world foo", "hello world bar")
  expect_true(sim > 0.4 && sim < 0.8)
  # NA handling

  expect_equal(jaccard_similarity(NA, "hello"), 0)
})

test_that("distinctive_phrase returns reasonable phrases", {
  phrase <- distinctive_phrase("Laparoscopic Hysterectomy Outcomes in Obese Patients")
  expect_true(nchar(phrase) > 5)
  words <- strsplit(phrase, "\\s+")[[1]]
  expect_true(length(words) >= 3 && length(words) <= 5)
})

test_that("extract_keywords returns non-empty results", {
  kw <- extract_keywords("Laparoscopic hysterectomy was performed in patients with endometriosis. The operative time was significantly shorter.", top_n = 5)
  expect_true(length(kw[[1]]) > 0)
  expect_true(length(kw[[1]]) <= 5)
  # Should not contain stopwords
  expect_false("the" %in% kw[[1]])
  expect_false("was" %in% kw[[1]])
})
