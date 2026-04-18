# Extracted from test-pipeline_semantics.R:223

# prequel ----------------------------------------------------------------------
library(testthat)
library(readr)
library(dplyr)
skip_if_not <- function(path) {
  if (!file.exists(path)) skip(paste("Missing:", basename(path)))
}

# test -------------------------------------------------------------------------
path <- here::here("output", "aim2b_cox_regression.csv")
skip_if_not(path)
cox <- read_csv(path, show_col_types = FALSE)
expect_true(nrow(cox) >= 3, label = "at least 3 terms")
expect_true(all(cox$estimate > 0, na.rm = TRUE), label = "all HRs positive")
