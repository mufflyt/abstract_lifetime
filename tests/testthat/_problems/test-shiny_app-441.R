# Extracted from test-shiny_app.R:441

# prequel ----------------------------------------------------------------------
library(testthat)
library(dplyr)
library(readr)
library(stringr)
library(here)
local({
  # dedup_decisions
  dedup_decisions <- function(decisions) {
    if (nrow(decisions) == 0) return(decisions)
    decisions |>
      group_by(abstract_id, reviewer) |>
      arrange(desc(review_timestamp)) |>
      slice(1) |>
      ungroup()
  }

  test_that("dedup_decisions keeps latest per abstract+reviewer", {
    d <- tibble(
      abstract_id      = c("A1", "A1", "A1"),
      reviewer         = c("TM", "TM", "JD"),
      manual_decision  = c("skip", "match", "no_match"),
      review_timestamp = c("2026-01-01 10:00:00",
                           "2026-01-02 10:00:00",
                           "2026-01-01 09:00:00")
    )
    result <- dedup_decisions(d)
    expect_equal(nrow(result), 2L)
    tm_row <- result |> filter(reviewer == "TM")
    expect_equal(tm_row$manual_decision, "match")
    expect_equal(tm_row$review_timestamp, "2026-01-02 10:00:00")
  })

  test_that("dedup_decisions handles empty input", {
    empty <- tibble(abstract_id = character(), reviewer = character(),
                    manual_decision = character(), review_timestamp = character())
    result <- dedup_decisions(empty)
    expect_equal(nrow(result), 0L)
  })
})

# test -------------------------------------------------------------------------
main   <- here("data", "processed", "abstracts_cleaned.csv")
bundle <- here("shiny", "adjudication_app", "bundle", "data",
                 "processed", "abstracts_cleaned.csv")
skip_if_not(file.exists(bundle))
main_mtime   <- file.info(main)$mtime
bundle_mtime <- file.info(bundle)$mtime
diff_secs <- abs(as.numeric(difftime(main_mtime, bundle_mtime, units = "secs")))
expect_lte(diff_secs, 7200,
             label = "bundle CSV should be synced within 2 hours of main CSV")
