# Extracted from test-shiny_app.R:125

# prequel ----------------------------------------------------------------------
library(testthat)
library(dplyr)
library(readr)
library(stringr)
library(here)

# test -------------------------------------------------------------------------
abs  <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                   show_col_types = FALSE)
rq   <- read_csv(here("output", "abstracts_with_matches.csv"),
                   show_col_types = FALSE)
cands <- read_csv(here("data", "processed", "pubmed_candidates.csv"),
                    show_col_types = FALSE)
abs_ids  <- unique(abs$abstract_id)
rq_ids   <- unique(rq$abstract_id)
cand_ids <- unique(cands$abstract_id)
orphan_rq <- setdiff(rq_ids, abs_ids)
expect_lte(length(orphan_rq), 5L,
             label = "review queue IDs not in abstracts_cleaned")
