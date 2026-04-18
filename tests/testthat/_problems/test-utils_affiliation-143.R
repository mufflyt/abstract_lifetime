# Extracted from test-utils_affiliation.R:143

# prequel ----------------------------------------------------------------------
library(testthat)
source(here::here("R", "utils_affiliation.R"))

# test -------------------------------------------------------------------------
if (length(TEACHING_HOSPITAL_NAMES) > 0) {
    expect_true(is_teaching_hospital("Brigham and Women's Hospital, Boston"))
    expect_true(is_teaching_hospital("Johns Hopkins Hospital"))
  }
