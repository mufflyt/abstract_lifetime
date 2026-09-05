# output/candidate_pool_index.csv is the committed slice of the 130 MB pool.
#
# It exists so that the candidate assertions run in CI rather than skipping.
# That only holds if the index is a faithful projection of the pool, so where
# the pool is present this compares them directly, and everywhere it checks the
# index's own shape.

library(testthat)

IDX <- here::here("output", "candidate_pool_index.csv")
FULL <- here::here("data", "processed", "pubmed_candidates.csv")

test_that("the committed index exists and has exactly the two columns it promises", {
  expect_true(file.exists(IDX),
              label = "output/candidate_pool_index.csv is missing; run Rscript scripts/build_candidate_index.R")
  skip_if_not(file.exists(IDX), "index absent")
  d <- readr::read_csv(IDX, show_col_types = FALSE)
  expect_setequal(names(d), c("abstract_id", "pmid"))
  expect_gt(nrow(d), 0)
  # A wider index would defeat the purpose: the whole reason this is committable
  # is that it is two columns.
  expect_lt(file.size(IDX), 5e6,
            label = "the index has grown past 5 MB; it is meant to stay a thin projection")
})

test_that("the index carries no duplicate or empty rows", {
  skip_if_not(file.exists(IDX), "index absent")
  d <- readr::read_csv(IDX, show_col_types = FALSE,
                       col_types = readr::cols(.default = readr::col_character()))
  expect_equal(anyDuplicated(d), 0L,
               label = "a duplicated abstract_id/pmid pair would inflate coverage counts")
  expect_true(all(!is.na(d$abstract_id) & nzchar(d$abstract_id)))
  expect_true(all(!is.na(d$pmid) & nzchar(d$pmid)))
})

test_that("the index is a faithful projection of the pool it was built from", {
  skip_if_not(file.exists(FULL), "full candidate pool not on this machine")
  skip_if_not(file.exists(IDX), "index absent")
  full <- readr::read_csv(FULL, show_col_types = FALSE,
                          col_select = c("abstract_id", "pmid"))
  full$pmid <- as.character(full$pmid)
  full <- dplyr::distinct(full)
  idx <- readr::read_csv(IDX, show_col_types = FALSE,
                         col_types = readr::cols(.default = readr::col_character()))

  # A stale index would let CI assert coverage against a pool that no longer
  # exists, which is a quieter version of the defect this whole line of work is
  # about: a check that passes because it is looking at the wrong thing.
  missing_from_index <- dplyr::anti_join(full, idx, by = c("abstract_id", "pmid"))
  extra_in_index     <- dplyr::anti_join(idx, full, by = c("abstract_id", "pmid"))
  expect_equal(nrow(missing_from_index), 0L,
               label = sprintf(paste("%d pairs are in the pool but not the index;",
                                     "run Rscript scripts/build_candidate_index.R"),
                               nrow(missing_from_index)))
  expect_equal(nrow(extra_in_index), 0L,
               label = sprintf("%d pairs are in the index but not the pool",
                               nrow(extra_in_index)))
})

test_that("the resolver prefers the real pool and falls back to the index", {
  d <- candidate_pool()
  expect_false(is.null(d), label = "neither the pool nor the index is available")
  skip_if(is.null(d))
  expect_true(all(c("abstract_id", "pmid") %in% names(d)))
  expect_type(d$pmid, "character")
  expect_true(candidate_pool_source() %in%
                c("data/processed/pubmed_candidates.csv",
                  "output/candidate_pool_index.csv"))
})
