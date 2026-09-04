# test-gender_nppes_tier.R — the NPPES registry tier of the gender waterfall.
#
# Tier 1 previously read `npi_gender` out of the ABOG board-certification
# export. That export's LATEST symlink was repointed upstream to a file with no
# gender column, so the shipped values could be used but not regenerated
# (docs/FAILURE_MODES.md F16). R/09k_gender_from_nppes.R reads registrant-
# reported sex from NPPES keyed on the NPI instead, which is both reproducible
# and not name-inferred.

suppressPackageStartupMessages({
  library(testthat); library(readr); library(dplyr); library(here)
})

skip_if_no_file <- function(p) skip_if_not(file.exists(p), paste("missing:", p))

NPPES  <- here("data", "processed", "gender_from_nppes.csv")
NPI    <- here("data", "processed", "npi_matches.csv")
POLICY <- here("data", "processed", "gender_resolution_policy.csv")
AWM    <- here("output", "abstracts_with_matches.csv")

test_that("the NPPES sidecar covers exactly the high-confidence NPI population", {
  skip_if_no_file(NPPES); skip_if_no_file(NPI)
  g <- read_csv(NPPES, show_col_types = FALSE)
  n <- read_csv(NPI, show_col_types = FALSE)

  expected <- n |> filter(npi_match_confidence == "high", !is.na(npi_number))
  expect_setequal(g$abstract_id, expected$abstract_id)
  expect_equal(anyDuplicated(g$abstract_id), 0L)

  # It must not invent an identity: the population is the one 10_npi already
  # accepted for npi_state, so this adds an attribute, not a new match.
  expect_true(all(g$abstract_id %in% n$abstract_id))
})

test_that("NPPES gender uses the pipeline vocabulary and is well resolved", {
  skip_if_no_file(NPPES)
  g <- read_csv(NPPES, show_col_types = FALSE)

  expect_true(all(g$nppes_gender %in% c("female", "male", NA)))
  expect_true(all(g$nppes_sex_raw %in% c("Female", "Male", NA)))
  # NPPES covers essentially every US clinician with an NPI; a low rate means
  # the lookup silently failed rather than that the registry is blank.
  expect_gte(mean(!is.na(g$nppes_gender)), 0.90,
             label = "NPPES resolution rate")
})

test_that("NPPES agrees with the ABOG value it supersedes", {
  skip_if_no_file(NPPES); skip_if_no_file(NPI)
  g <- read_csv(NPPES, show_col_types = FALSE)
  abog <- read_csv(NPI, show_col_types = FALSE) |>
    filter(npi_match_confidence == "high", !is.na(npi_gender)) |>
    transmute(abstract_id,
              abog = case_when(npi_gender == "F" ~ "female",
                               npi_gender == "M" ~ "male",
                               TRUE ~ NA_character_))

  cmp <- g |>
    inner_join(abog, by = "abstract_id") |>
    filter(!is.na(nppes_gender), !is.na(abog))
  skip_if(nrow(cmp) == 0, "no overlap to compare")

  agreement <- mean(cmp$nppes_gender == cmp$abog)
  # Two registries describing the same people should almost always agree. A
  # drop here means the NPI matches themselves have degraded, not that one
  # registry is wrong.
  expect_gte(agreement, 0.95,
             label = sprintf("NPPES/ABOG agreement (%d/%d)",
                             sum(cmp$nppes_gender == cmp$abog), nrow(cmp)))
})

test_that("the resolution policy puts NPPES first and matches the code", {
  skip_if_no_file(POLICY)
  p <- read_csv(POLICY, show_col_types = FALSE)

  expect_equal(nrow(p), 11L)
  expect_equal(p$tier, 1:11)
  expect_equal(p$source[1], "nppes")
  expect_equal(p$column[1], "gender_nppes")
  expect_equal(p$source[2], "npi")

  # The two registry tiers are the only ones that do not infer from a name.
  expect_equal(p$resolution[p$source %in% c("nppes", "npi")],
               c("registry", "registry"))
  expect_equal(p$resolution[p$source == "ssa"], "initial_only")

  # The policy file is written by 10e from the same object the coalesce uses,
  # so a drift between them means someone edited one and not the other.
  src <- readLines(here("R", "10e_merge_demographics.R"), warn = FALSE)
  coalesce_block <- paste(src[grep("gender_unified = coalesce\\(", src) +
                                 0:4], collapse = " ")
  for (col in p$column) {
    expect_true(grepl(col, coalesce_block, fixed = TRUE),
                info = paste(col, "is in the policy but not in the coalesce"))
  }
  expect_lt(regexpr("gender_nppes", coalesce_block, fixed = TRUE),
            regexpr("gender_npi,", coalesce_block, fixed = TRUE),
            label = "gender_nppes must precede gender_npi in the coalesce")
})

test_that("gender_source labels are drawn from the policy and resolve to it", {
  skip_if_no_file(AWM); skip_if_no_file(POLICY); skip_if_no_file(NPPES)
  d <- read_csv(AWM, show_col_types = FALSE)
  p <- read_csv(POLICY, show_col_types = FALSE)
  g <- read_csv(NPPES, show_col_types = FALSE)

  expect_true(all(na.omit(unique(d$gender_source)) %in% p$source),
              info = "a gender_source value is not a documented tier")

  # A value labelled nppes must actually be the value the sidecar carries.
  from_nppes <- d |>
    filter(gender_source == "nppes") |>
    select(abstract_id, gender_unified) |>
    inner_join(select(g, abstract_id, nppes_gender), by = "abstract_id")
  expect_gt(nrow(from_nppes), 0)
  expect_true(all(from_nppes$gender_unified == from_nppes$nppes_gender),
              info = "gender_source says nppes but the value differs from the sidecar")

  # Nothing may be labelled nppes without a sidecar row.
  expect_equal(
    sum(d$gender_source == "nppes" & !(d$abstract_id %in% g$abstract_id),
        na.rm = TRUE),
    0L
  )
})

test_that("adding a registry tier did not reduce gender coverage", {
  skip_if_no_file(AWM)
  d <- read_csv(AWM, show_col_types = FALSE)
  # 1,066 of 1,106 as of 2026-09-04, up from 1,065 before the tier was added.
  expect_gte(sum(!is.na(d$gender_unified)), 1065L)
  expect_gt(sum(d$gender_source == "nppes", na.rm = TRUE), 200L)
})
