# Extracted from test-utils_affiliation.R:38

# prequel ----------------------------------------------------------------------
library(testthat)
source(here::here("R", "utils_affiliation.R"))

# test -------------------------------------------------------------------------
expect_equal(classify_practice_type("Small Town Hospital, Nowhere, KS"), "community")
