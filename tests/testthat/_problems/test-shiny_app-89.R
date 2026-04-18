# Extracted from test-shiny_app.R:89

# prequel ----------------------------------------------------------------------
library(testthat)
library(dplyr)
library(readr)
library(stringr)
library(here)

# test -------------------------------------------------------------------------
abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                  show_col_types = FALSE)
row <- abs |> filter(abstract_id == "AAGL2012_001")
expect_equal(nrow(row), 1L)
expect_false(is.na(row$abstract_text[1]),
               info = "AAGL2012_001 should have abstract_text after JSON backfill")
expect_match(row$abstract_text[1], "Objective|Radiofrequency|ablation",
               ignore.case = TRUE)
