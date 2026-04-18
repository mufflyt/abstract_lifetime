# Extracted from test-shiny_app.R:86

# prequel ----------------------------------------------------------------------
library(testthat)
library(dplyr)
library(readr)
library(stringr)
library(here)

# test -------------------------------------------------------------------------
abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                  show_col_types = FALSE)
early <- abs |> filter(congress_year %in% 2012:2018)
pct_with_text <- mean(!is.na(early$abstract_text))
expect_lte(pct_with_text, 0.10,
             label = "2012-2018 have minimal abstract text (expected)")
