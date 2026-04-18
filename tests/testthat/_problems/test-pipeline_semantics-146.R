# Extracted from test-pipeline_semantics.R:146

# prequel ----------------------------------------------------------------------
library(testthat)
library(readr)
library(dplyr)
skip_if_not <- function(path) {
  if (!file.exists(path)) skip(paste("Missing:", basename(path)))
}

# test -------------------------------------------------------------------------
path <- here::here("output", "abstracts_with_matches.csv")
skip_if_not(path)
d <- read_csv(path, show_col_types = FALSE)
published <- d |> filter(classification == "definite", !is.na(months_to_pub))
if (nrow(published) > 0) {
    expect_true(all(published$months_to_pub >= 0))
  }
