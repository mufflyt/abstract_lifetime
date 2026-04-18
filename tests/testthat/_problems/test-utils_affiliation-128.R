# Extracted from test-utils_affiliation.R:128

# prequel ----------------------------------------------------------------------
library(testthat)
source(here::here("R", "utils_affiliation.R"))

# test -------------------------------------------------------------------------
expect_equal(classify_career_stage("Assistant Professor, Department of OB/GYN"), "faculty_junior")
