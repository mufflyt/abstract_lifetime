# test-docs_drift.R — Documentation drift detection.
#
# The documentation in docs/ states numbers, dimensions and vocabularies that
# are derived from the analytical outputs. These tests fail when the data moves
# and the documentation does not, or vice versa.
#
# One assertion below (per-congress capture, F1) pins a KNOWN DEFECT at its
# current magnitude rather than asserting the correct invariant. That is
# deliberate: an assertion of the correct invariant would fail today and be
# muted, whereas a pinned value keeps the defect visible and forces
# docs/FAILURE_MODES.md to be updated when it is fixed. See docs/VALIDATION.md
# section 4.
#
# The candidate-pool assertion was pinned the same way until 2026-09-03, when
# scripts/rebuild_candidate_pool.R fixed F2; it now asserts the true invariant.

suppressPackageStartupMessages({
  library(testthat)
  library(readr)
  library(dplyr)
})

skip_if_no_file <- function(path) {
  skip_if_not(file.exists(path), paste("missing:", path))
}

docs_dir <- here::here("docs")
fad_path <- here::here("output", "final_analytical_dataset.csv")

# --- Documented paths resolve ------------------------------------------------

test_that("every script path in the pipeline manifest exists", {
  manifest_path <- file.path(docs_dir, "pipeline_manifest.yml")
  skip_if_no_file(manifest_path)
  skip_if_not_installed("yaml")

  manifest <- yaml::read_yaml(manifest_path)
  scripts <- vapply(manifest$stages, function(s) s$script %||% NA_character_,
                    character(1))
  scripts <- scripts[!is.na(scripts)]

  missing <- scripts[!file.exists(here::here(scripts))]
  expect_equal(missing, character(0),
               info = "pipeline_manifest.yml names scripts that do not exist")
})

test_that("every file marked authoritative in the inventory exists", {
  inv_path <- file.path(docs_dir, "data_inventory.csv")
  skip_if_no_file(inv_path)

  inv <- read_csv(inv_path, show_col_types = FALSE)
  auth <- inv |> dplyr::filter(authoritative == "yes")

  # Gitignored artefacts are absent from a fresh checkout, so only require the
  # files the inventory itself records as git-tracked.
  auth_tracked <- auth |> dplyr::filter(tracked == "yes")
  missing <- auth_tracked$path[!file.exists(here::here(auth_tracked$path))]

  expect_equal(missing, character(0),
               info = "docs/data_inventory.csv marks missing files authoritative")
})

# --- Dataset shape and vocabulary --------------------------------------------

test_that("the final analytical dataset has the documented dimensions", {
  skip_if_no_file(fad_path)
  fad <- read_csv(fad_path, show_col_types = FALSE)

  # Documented in docs/DATA_DICTIONARY.md and docs/COHORT_ASSEMBLY.md.
  expect_equal(nrow(fad), 1106L)
  expect_equal(ncol(fad), 93L)
  expect_equal(dplyr::n_distinct(fad$abstract_id), nrow(fad))
})

test_that("classification levels match the documented vocabulary", {
  skip_if_no_file(fad_path)
  fad <- read_csv(fad_path, show_col_types = FALSE)

  documented <- c("definite", "probable", "possible", "no_match",
                  "no_candidates", "excluded")
  expect_setequal(unique(fad$classification), documented)
})

test_that("manual_decision levels match the documented vocabulary", {
  skip_if_no_file(fad_path)
  fad <- read_csv(fad_path, show_col_types = FALSE)

  expect_setequal(unique(na.omit(fad$manual_decision)),
                  c("match", "no_match", "skip"))
})

# --- Data dictionary completeness --------------------------------------------

test_that("no undocumented column appears in the final analytical dataset", {
  skip_if_no_file(fad_path)
  dict_path <- file.path(docs_dir, "data_dictionary.csv")
  skip_if_no_file(dict_path)

  fad <- read_csv(fad_path, show_col_types = FALSE)
  dict <- read_csv(dict_path, show_col_types = FALSE)

  undocumented <- setdiff(names(fad), dict$variable)
  expect_equal(undocumented, character(0),
               info = "add these columns to docs/data_dictionary.csv")
})

test_that("no documented column has disappeared from the dataset", {
  skip_if_no_file(fad_path)
  dict_path <- file.path(docs_dir, "data_dictionary.csv")
  skip_if_no_file(dict_path)

  fad <- read_csv(fad_path, show_col_types = FALSE)
  dict <- read_csv(dict_path, show_col_types = FALSE)

  vanished <- setdiff(dict$variable, names(fad))
  expect_equal(vanished, character(0),
               info = "remove these from docs/data_dictionary.csv or restore them")
})

test_that("every fitted model term is documented in the data dictionary", {
  dict_path <- file.path(docs_dir, "data_dictionary.csv")
  cox_path <- here::here("data", "processed", "cox_model.rds")
  logit_path <- here::here("data", "processed", "logistic_model.rds")
  skip_if_no_file(dict_path)
  skip_if_no_file(cox_path)
  skip_if_no_file(logit_path)

  dict <- read_csv(dict_path, show_col_types = FALSE)

  model_vars <- unique(c(
    all.vars(formula(readRDS(cox_path))),
    all.vars(formula(readRDS(logit_path)))
  ))
  # Derived-in-model terms have no column of their own.
  model_vars <- setdiff(model_vars, c("Surv", "time", "event",
                                      "published_int", "log_sample_size"))

  undocumented <- setdiff(model_vars, dict$variable)
  expect_equal(undocumented, character(0),
               info = "an analysis variable is missing from the data dictionary")
})

# --- Headline numbers ---------------------------------------------------------

test_that("the documented numerator and denominator agree with the dataset", {
  skip_if_no_file(fad_path)
  fad <- read_csv(fad_path, show_col_types = FALSE)

  n_cohort    <- nrow(fad)
  n_pending   <- sum(is.na(fad$final_published))
  n_evaluated <- n_cohort - n_pending
  n_published <- sum(fad$final_published, na.rm = TRUE)

  # Documented across docs/COHORT_ASSEMBLY.md, RESULTS_PROVENANCE.md and README.
  expect_equal(n_cohort, 1106L)
  expect_equal(n_pending, 55L)
  expect_equal(n_evaluated, 1051L)
  expect_equal(n_published, 170L)
  expect_equal(round(n_published / n_evaluated * 100, 1), 16.2)
})

test_that("aim1_publication_rate.csv agrees with the dataset it summarises", {
  skip_if_no_file(fad_path)
  aim1_path <- here::here("output", "aim1_publication_rate.csv")
  skip_if_no_file(aim1_path)

  fad  <- read_csv(fad_path, show_col_types = FALSE)
  aim1 <- read_csv(aim1_path, show_col_types = FALSE)
  val  <- function(m) aim1$value[aim1$metric == m]

  expect_equal(val("total_abstracts"), nrow(fad))
  expect_equal(val("pending_review"), sum(is.na(fad$final_published)))
  expect_equal(val("n_evaluated"), nrow(fad) - sum(is.na(fad$final_published)))
  expect_equal(val("published"), sum(fad$final_published, na.rm = TRUE))
  expect_equal(val("publication_rate"),
               round(val("published") / val("n_evaluated") * 100, 1))
})

test_that("the README headline agrees with the generated result", {
  readme_path <- here::here("README.md")
  skip_if_no_file(readme_path)
  skip_if_no_file(fad_path)

  fad <- read_csv(fad_path, show_col_types = FALSE)
  n_pending   <- sum(is.na(fad$final_published))
  n_evaluated <- nrow(fad) - n_pending
  n_published <- sum(fad$final_published, na.rm = TRUE)
  rate <- round(n_published / n_evaluated * 100, 1)

  readme <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")

  expect_true(grepl(sprintf("\\*\\*%s%%\\*\\*", format(rate, nsmall = 1)),
                    readme),
              info = "README headline rate does not match the generated rate")
  expect_true(grepl(format(n_published, big.mark = ","), readme, fixed = TRUE),
              info = "README does not state the current numerator")
  expect_true(grepl(format(n_evaluated, big.mark = ","), readme, fixed = TRUE),
              info = "README does not state the current denominator")
  expect_true(grepl(format(nrow(fad), big.mark = ","), readme, fixed = TRUE),
              info = "README does not state the current cohort size")
  expect_true(grepl(paste0("\\b", n_pending, "\\b"), readme),
              info = "README does not state the current unresolved count")
})

# --- Duplicated logic must not diverge ---------------------------------------

test_that("the inline decision logic in 07 and 08 still agrees with utils_decisions", {
  awm_path <- here::here("output", "abstracts_with_matches.csv")
  dec_path <- here::here("output", "manual_review_decisions.csv")
  skip_if_no_file(awm_path)
  skip_if_no_file(dec_path)

  source(here::here("R", "utils_decisions.R"), local = TRUE)

  res <- read_csv(awm_path, show_col_types = FALSE)
  dec <- read_csv(dec_path, show_col_types = FALSE)

  canonical <- assign_final_published(res, dedup_decisions_for_analysis(dec))

  # The dedup that R/07_make_tables.R and R/08_make_figures.R inline: latest
  # timestamp per abstract, with no human-over-AUTO precedence rule.
  inline_dedup <- dec |>
    dplyr::filter(!is.na(reviewer)) |>
    dplyr::group_by(abstract_id) |>
    dplyr::arrange(dplyr::desc(review_timestamp), .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  inline <- res |>
    dplyr::left_join(dplyr::select(inline_dedup, abstract_id, manual_decision,
                                   reviewer),
                     by = "abstract_id") |>
    dplyr::mutate(final_published = dplyr::case_when(
      # Both PI decisions of 2026-09-05 are branches here. A reference
      # implementation without them would assert the absence of the rules.
      !is.na(months_to_pub) & months_to_pub < 0 ~ FALSE,
      manual_decision == "no_match" & !is.na(reviewer) & reviewer != "AUTO" ~ FALSE,
      classification == "definite" ~ TRUE,
      manual_decision == "match" ~ TRUE,
      manual_decision == "no_match" ~ FALSE,
      classification %in% c("no_match", "no_candidates", "excluded") ~ FALSE,
      TRUE ~ NA
    ))

  expect_identical(canonical$final_published, inline$final_published,
                   info = paste("R/07_make_tables.R and R/08_make_figures.R have",
                                "diverged from R/utils_decisions.R; see",
                                "docs/FAILURE_MODES.md F9"))
})

test_that("07 and 08 adopt the outcome the analysis settled", {
  # Recomputing the cascade is no longer sufficient for them to agree with 06.
  # 06 refreshes months_to_pub against the credited PMID before applying the
  # pre-congress exclusion; 07 and 08 do not, so two abstracts would be
  # unpublished in the analysis and published in the tables. Both must adopt
  # the settled outcome. See appendix A19.3.
  for (f in c("R/07_make_tables.R", "R/08_make_figures.R")) {
    p <- here::here(f)
    skip_if_not(file.exists(p))
    txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
    expect_true(
      grepl("adopt_analysis_outcome(", txt, fixed = TRUE),
      label = paste(f, "does not call adopt_analysis_outcome(), so it can",
                    "disagree with R/06_analyze_results.R about which abstracts",
                    "are published"))
  }
})

# --- Pinned known defects (see docs/FAILURE_MODES.md) -------------------------

test_that("F2 (FIXED 2026-09-03): the candidate pool covers every scored pair", {
  sc_path <- here::here("data", "processed", "match_scores.csv")
  skip_if_no_file(sc_path)
  cand <- candidate_pool()
  skip_if(is.null(cand), "neither the candidate pool nor its committed index is available")

  sc <- read_csv(sc_path, show_col_types = FALSE) |>
    dplyr::mutate(best_pmid = as.character(best_pmid))

  unresolvable <- sc |>
    dplyr::filter(!is.na(best_pmid)) |>
    dplyr::anti_join(dplyr::distinct(dplyr::select(cand, abstract_id, pmid)),
                     by = c("abstract_id", "best_pmid" = "pmid"))

  # Was 283. scripts/rebuild_candidate_pool.R refetched the metadata that
  # R/03b_search_crossref.R had overwritten, and the pool now covers every
  # scored pair plus every reviewer-supplied PMID. The invariant is asserted
  # directly rather than pinned, because it is now true.
  expect_equal(nrow(unresolvable), 0L,
               info = paste("R/03b rewrote the pool after 04 scored it.",
                            "Run scripts/rebuild_candidate_pool.R"))
})

test_that("PINNED DEFECT F1: per-congress capture is still ceilinged near 100", {
  parsed_path <- here::here("data", "processed", "abstracts_parsed_web.csv")
  skip_if_no_file(parsed_path)

  parsed <- read_csv(parsed_path, show_col_types = FALSE)
  per_year <- parsed |> dplyr::count(congress_year, name = "n")

  # Every congress captured 93-100 listing items against 392-852 supplement
  # items deposited in Crossref. See docs/COHORT_ASSEMBLY.md section 5.
  expect_true(all(per_year$n <= 100L),
              info = "a congress now exceeds the 100-item listing ceiling")
  expect_equal(nrow(parsed), 1154L,
               info = paste("the parsed row count changed. If the supplement",
                            "listing has been re-ingested, update",
                            "docs/COHORT_ASSEMBLY.md and docs/FAILURE_MODES.md F1."))
})
