# Tests for utils_scoring.R

library(testthat)
source(here::here("R", "utils_scoring.R"))

# Mock config for testing
mock_cfg <- list(
  conference = list(date = "2023-11-07"),
  scoring = list(
    title_jaccard_high = 0.80,
    title_jaccard_mid = 0.60,
    title_jaccard_low = 0.40,
    abstract_semantic_high = 0.85,
    abstract_semantic_mid = 0.70,
    author_fuzzy_threshold = 0.90,
    auto_accept = 7,
    manual_review = 4,
    auto_reject_below = 4,
    pre_conference_penalty = -3,
    pub_date_early_months = 18,
    pub_date_late_months = 30
  )
)

test_that("classify_match returns Cochrane-aligned categories", {
  expect_equal(classify_match(8, mock_cfg), "definite")
  expect_equal(classify_match(7, mock_cfg), "definite")
  expect_equal(classify_match(5, mock_cfg), "probable")
  expect_equal(classify_match(4, mock_cfg), "probable")
  expect_equal(classify_match(3, mock_cfg), "no_match")
  expect_equal(classify_match(0, mock_cfg), "no_match")
})

test_that("classify_match handles text evidence gate", {
  expect_equal(classify_match(5, mock_cfg, has_text_evidence = TRUE), "probable")
  expect_equal(classify_match(5, mock_cfg, has_text_evidence = FALSE), "possible")
  expect_equal(classify_match(8, mock_cfg, has_text_evidence = FALSE), "possible")
})

test_that("classify_match marks pre-conference as excluded", {
  expect_equal(classify_match(9, mock_cfg, pre_conference = TRUE), "excluded")
  expect_equal(classify_match(2, mock_cfg, pre_conference = TRUE), "excluded")
})

test_that("compute_text_similarity returns valid range", {
  sim <- compute_text_similarity(
    "laparoscopic hysterectomy for endometriosis treatment outcomes",
    "laparoscopic hysterectomy outcomes in endometriosis patients"
  )
  expect_true(sim >= 0 && sim <= 1)
  expect_true(sim > 0.5)  # Very similar texts

  # Unrelated texts
  sim2 <- compute_text_similarity(
    "cardiac surgery bypass grafting outcomes",
    "laparoscopic hysterectomy endometriosis treatment"
  )
  expect_true(sim2 < sim)

  # NA handling
  expect_equal(compute_text_similarity(NA, "hello"), 0)
})

test_that("score_match returns expected structure", {
  abstract <- list(
    title = "Laparoscopic Hysterectomy Outcomes After Endometriosis Treatment",
    abstract_text = "We studied laparoscopic hysterectomy outcomes after endometriosis",
    first_author_normalized = "smith j",
    last_author_normalized = "jones k",
    all_authors_normalized = c("smith j", "jones k", "doe a"),
    keywords = c("laparoscopic", "hysterectomy", "outcomes")
  )

  candidate <- list(
    pub_title = "Laparoscopic Hysterectomy Outcomes After Endometriosis Treatment in Practice",
    pub_abstract = "We studied laparoscopic hysterectomy outcomes after endometriosis treatment",
    pub_first_author = "Smith, John",
    pub_last_author = "Jones, Karen",
    pub_all_authors = "Smith, John; Doe, Alice; Jones, Karen",
    pub_journal = "Journal of Minimally Invasive Gynecology",
    pub_keywords = "laparoscopic; hysterectomy; outcomes; gynecology",
    pub_year = "2024",
    pub_month = "06",
    pub_day = "01"
  )

  sc <- score_match(abstract, candidate, mock_cfg)

  expect_true(is.list(sc))
  expect_true("total" %in% names(sc))
  expect_true(sc$total > 0)
  expect_true(sc$title_sim >= 0.5)
  expect_true(sc$first_author_points > 0)
  expect_true(sc$last_author_points > 0)
})

test_that("pre-conference publication gets penalty", {
  abstract <- list(
    title = "Test Title",
    abstract_text = NA,
    first_author_normalized = "test a",
    last_author_normalized = "test b",
    all_authors_normalized = c("test a", "test b"),
    keywords = character(0)
  )

  # Published BEFORE conference
  candidate_before <- list(
    pub_title = "Different Title",
    pub_abstract = NA,
    pub_first_author = "Other X",
    pub_last_author = "Other Y",
    pub_all_authors = "Other X; Other Y",
    pub_journal = "Some Journal",
    pub_keywords = NA,
    pub_year = "2023",
    pub_month = "01",
    pub_day = "01"
  )

  sc <- score_match(abstract, candidate_before, mock_cfg)
  expect_equal(sc$date_points, -3)
})
