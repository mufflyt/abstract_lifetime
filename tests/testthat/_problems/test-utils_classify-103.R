# Extracted from test-utils_classify.R:103

# prequel ----------------------------------------------------------------------
library(testthat)
source(here::here("R", "utils_classify.R"))

# test -------------------------------------------------------------------------
expect_equal(classify_research_category("simulation training for laparoscopic skills assessment", study_design = "rct"), "device_technology")
