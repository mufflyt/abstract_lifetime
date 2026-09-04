# test-remediation_invariants.R — invariants for the defects fixed in the
# 2026-09-03 remediation pass. Each block names the failure mode it guards, so a
# regression points straight at the write-up in docs/FAILURE_MODES.md.

suppressPackageStartupMessages({
  library(testthat); library(readr); library(dplyr)
})

skip_if_no_file <- function(path) skip_if_not(file.exists(path), paste("missing:", path))

cleaned_path <- here::here("data", "processed", "abstracts_cleaned.csv")
fad_path     <- here::here("output", "final_analytical_dataset.csv")
awm_path     <- here::here("output", "abstracts_with_matches.csv")

# --- F3: predictors must be derived from the text that actually exists --------

test_that("F3: abstract_text carries no scraper footnotes", {
  skip_if_no_file(cleaned_path)
  d <- read_csv(cleaned_path, show_col_types = FALSE)

  present <- !is.na(d$abstract_text)
  # "*: Corresponding author." is 24 characters and was written into all 95
  # abstracts of the 2018 congress by the snippet backfill. It passed the
  # nchar >= 10 gate that 02b/02c use to decide a row needs no backfill, so
  # those rows could never be repaired, and it displaced the title as the
  # source for every text-derived covariate.
  expect_equal(sum(present & nchar(d$abstract_text) < 100), 0L,
               info = "a degenerate abstract_text has reappeared; see 02d")
  expect_equal(sum(present & grepl("^\\s*\\*\\s*:", d$abstract_text)), 0L,
               info = "a footnote has been written into abstract_text")
})

test_that("F3: no text-derived covariate is structurally zero for a whole congress", {
  skip_if_no_file(fad_path)
  d <- read_csv(fad_path, show_col_types = FALSE)

  # A flag that is exactly 0% across every abstract of a congress year is
  # definitional, not empirical. Before the fix, has_numeric_results was 0.0%
  # in each of the seven congresses 2012-2018 because it read only the
  # structured section columns, which the backfills never populate.
  #
  # 2017 and 2018 are exempt: 96 of 97 and 95 of 95 of their abstracts have no
  # recoverable text at all, so a zero there is a true absence of evidence
  # rather than a derivation ordering bug. See docs/COHORT_ASSEMBLY.md.
  textless_years <- c(2017, 2018)
  flags <- c("is_rct", "is_us_based", "is_academic", "has_numeric_results",
             "is_multicenter")

  by_year <- d |>
    filter(!congress_year %in% textless_years) |>
    group_by(congress_year) |>
    summarise(across(all_of(flags), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  for (f in flags) {
    expect_true(all(by_year[[f]] > 0),
                info = paste0(f, " is 0% for congress year(s) ",
                              paste(by_year$congress_year[by_year[[f]] == 0],
                                    collapse = ", ")))
  }
})

test_that("F3: abstract_word_count is nonzero wherever text exists", {
  skip_if_no_file(cleaned_path)
  d <- read_csv(cleaned_path, show_col_types = FALSE)
  has_text <- !is.na(d$abstract_text) & nchar(d$abstract_text) > 100
  expect_equal(sum(has_text & d$abstract_word_count == 0), 0L,
               info = "abstract_word_count was computed before the text backfill")
})

# --- F10: step 5 must not destroy the enrichment columns ----------------------

test_that("F10: abstracts_with_matches retains the enrichment block", {
  skip_if_no_file(awm_path)
  d <- read_csv(awm_path, show_col_types = FALSE)

  # R/05_adjudicate.R rebuilds this file from scratch with ~46 columns. Six
  # later stages add the rest in place. Re-running 05 alone used to delete them.
  enrichment <- c("pub_type_canonical", "n_authors", "gender_unified",
                  "gender_source", "npi_match_confidence", "state_unified",
                  "subspecialty_unified", "cited_by_count")
  expect_true(all(enrichment %in% names(d)),
              info = paste("missing:",
                           paste(setdiff(enrichment, names(d)), collapse = ", ")))
})

# --- Vocabulary harmonisation ------------------------------------------------

test_that("subspecialty_unified uses a single vocabulary", {
  skip_if_no_file(fad_path)
  d <- read_csv(fad_path, show_col_types = FALSE)
  skip_if_not("subspecialty_unified" %in% names(d))

  spelled_out <- c("Female Pelvic Medicine & Reconstructive Surgery",
                   "Generalist", "Gynecologic Oncology",
                   "Reproductive Endocrinology and Infertility", "MIG")
  offenders <- intersect(unique(na.omit(d$subspecialty_unified)), spelled_out)
  expect_equal(offenders, character(0),
               info = "harmonise_subspecialty() in 10e did not run")
})

# --- F4: subgroup tables must declare outcome-conditional stratifiers ---------

test_that("F4: subgroup rate tables carry their availability split", {
  for (f in c("aim1_by_practice_type.csv", "aim1_by_subspecialty.csv")) {
    p <- here::here("output", f)
    skip_if_no_file(p)
    t <- read_csv(p, show_col_types = FALSE)
    expect_true(all(c("availability_among_published",
                      "availability_among_unpublished",
                      "outcome_conditional_stratifier") %in% names(t)),
                info = paste(f, "does not state that its stratifier is a",
                             "function of the outcome"))
  }
})

# --- Sensitivity scenarios must name their denominator -----------------------

test_that("sensitivity scenarios state which population they divide by", {
  p <- here::here("output", "sensitivity_analyses.csv")
  skip_if_no_file(p)
  s <- read_csv(p, show_col_types = FALSE)
  expect_true("denominator" %in% names(s),
              info = "scenarios mix the cohort and the evaluated set silently")
  expect_true(all(!is.na(s$denominator)))
})

# --- F2: the candidate pool must cover what was scored -----------------------

test_that("F2: every winning PMID resolves in the candidate pool", {
  sc_path   <- here::here("data", "processed", "match_scores.csv")
  cand_path <- here::here("data", "processed", "pubmed_candidates.csv")
  skip_if_no_file(sc_path)
  skip_if_no_file(cand_path)

  sc <- read_csv(sc_path, show_col_types = FALSE) |>
    mutate(best_pmid = as.character(best_pmid))
  cand <- read_csv(cand_path, show_col_types = FALSE,
                   col_types = cols(.default = col_character()))

  unresolvable <- sc |>
    filter(!is.na(best_pmid)) |>
    anti_join(distinct(select(cand, abstract_id, pmid)),
              by = c("abstract_id", "best_pmid" = "pmid"))

  expect_equal(nrow(unresolvable), 0L,
               info = paste(nrow(unresolvable), "winning PMIDs are absent from",
                            "the pool; R/03b rewrote it after 04 scored.",
                            "Run scripts/rebuild_candidate_pool.R"))
})

test_that("F2: published abstracts carry a publication date", {
  skip_if_no_file(fad_path)
  d <- read_csv(fad_path, show_col_types = FALSE)

  pub <- d |> filter(!is.na(final_published), final_published)
  dated <- sum(!is.na(pub$months_to_pub))

  # Time-to-event analyses run on the dated subset. Anything below full
  # coverage silently shrinks the event count in the KM and Cox models.
  expect_gte(dated / nrow(pub), 0.95,
             label = sprintf("publication-date coverage among the published (%d/%d)",
                             dated, nrow(pub)))
})

test_that("F12: pre-congress publications are confined and excluded from Aim 2", {
  skip_if_no_file(fad_path)
  d <- read_csv(fad_path, show_col_types = FALSE)
  pub <- d |> filter(!is.na(final_published), final_published, !is.na(months_to_pub))
  neg <- pub |> filter(months_to_pub < 0)

  # A negative interval is legitimate but must be explainable: the paper
  # appeared before the meeting and either the algorithm scored it `definite`
  # or a reviewer confirmed it anyway. It must never arise from an unreviewed
  # low-confidence match.
  expect_true(all(neg$classification == "definite" | neg$manual_decision == "match"),
              info = "a pre-congress publication entered the numerator unreviewed")

  # And it must not enter the time-to-publication summary, where a negative
  # value is not a time to publication.
  a2_path <- here::here("output", "aim2_time_to_pub.csv")
  skip_if_no_file(a2_path)
  a2 <- read_csv(a2_path, show_col_types = FALSE)
  val <- function(m) a2$value[a2$metric == m]

  expect_gte(val("min_months"), 0,
             label = "aim2 min_months (pre-congress rows must be excluded)")
  expect_equal(val("n_pre_congress"), nrow(neg))
  expect_equal(val("n_with_dates") + val("n_pre_congress") + val("n_undated"),
               val("n_published"),
               info = "the Aim 2 population does not partition the published set")
})
