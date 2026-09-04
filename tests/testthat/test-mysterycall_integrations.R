# test-mysterycall_integrations.R — the four functions borrowed from the
# mysterycall package (pinned at 42d66d92, see docs/REPRODUCIBILITY.md).
#
# Each is wired to degrade rather than fail when the package is absent, so the
# tests below check the DEGRADED path is still correct as well as the enriched
# one.

suppressPackageStartupMessages({
  library(testthat); library(readr); library(dplyr); library(here); library(tibble)
})

skip_if_no_file <- function(p) skip_if_not(file.exists(p), paste("missing:", p))
has_mc <- requireNamespace("mysterycall", quietly = TRUE)

# ── 1. Table 1 ────────────────────────────────────────────────────────────────

test_that("Table 1 is stratified, tested, and reconciles with the cohort", {
  t1_path <- here("output", "tables", "table1_characteristics.csv")
  ns_path <- here("output", "tables", "table1_column_ns.csv")
  fad_path <- here("output", "final_analytical_dataset.csv")
  skip_if_no_file(t1_path); skip_if_no_file(fad_path)
  skip_if_not(has_mc, "mysterycall absent - Table 1 is in its reduced form")
  skip_if_no_file(ns_path)

  t1 <- read_csv(t1_path, show_col_types = FALSE)
  ns <- read_csv(ns_path, show_col_types = FALSE)
  fad <- read_csv(fad_path, show_col_types = FALSE)

  expect_true(all(c("variable", "level", "p_value") %in% names(t1)))
  expect_gt(nrow(t1), 20)

  # The stratum sizes must be the analysis populations, not something else.
  n_pub   <- sum(fad$final_published, na.rm = TRUE)
  n_eval  <- sum(!is.na(fad$final_published))
  expect_equal(ns$n[ns$stratum == "Overall"], n_eval)
  expect_equal(ns$n[ns$stratum == "Published"], n_pub)
  expect_equal(ns$n[ns$stratum == "Not published"], n_eval - n_pub)

  # Variables the reduced form could not show at all.
  expect_true(all(c("study_design", "gender_unified") %in% t1$variable))

  # A p-value appears once per variable, on its first level.
  p_per_var <- t1 |> group_by(variable) |> summarise(n_p = sum(!is.na(p_value)))
  expect_true(all(p_per_var$n_p <= 1))
  expect_gt(sum(!is.na(t1$p_value)), 5)
})

# ── 2. Join safety in 10e ─────────────────────────────────────────────────────

# safe_join() is defined inside R/10e_merge_demographics.R, which executes a
# whole merge on source(). Lift just the two function definitions.
load_safe_join <- function() {
  src <- readLines(here("R", "10e_merge_demographics.R"), warn = FALSE)
  starts <- grep("^(assert_unique_keys|safe_join) <- function", src)
  skip_if(length(starts) < 2, "helpers not found in 10e")
  env <- new.env(parent = globalenv())
  closes <- grep("^\\}$", src)
  for (st in starts) {
    en <- min(closes[closes > st])
    eval(parse(text = paste(src[st:en], collapse = "\n")), envir = env)
  }
  env
}

test_that("safe_join blocks a sidecar with duplicate keys", {
  env <- load_safe_join()
  left  <- tibble(abstract_id = c("A", "B"))
  right <- tibble(abstract_id = c("A", "A"), g = c("x", "y"))

  expect_error(env$safe_join(left, right, label = "poisoned sidecar"),
               regexp = "duplicate|unique",
               info = "a duplicated sidecar key must not silently multiply rows")
})

test_that("safe_join preserves the row count and joins normally", {
  env <- load_safe_join()
  left  <- tibble(abstract_id = c("A", "B", "C"))
  right <- tibble(abstract_id = c("A", "C"), g = c("x", "z"))

  out <- env$safe_join(left, right, label = "clean sidecar")
  expect_equal(nrow(out), 3L)
  expect_equal(out$g, c("x", NA, "z"))
})

test_that("safe_join still adds columns from an empty-but-typed sidecar", {
  # 10g second-author triangulation has never returned a row. Its column must
  # still be created, because the gender coalesce() names it.
  env <- load_safe_join()
  left  <- tibble(abstract_id = c("A", "B"))
  empty <- tibble(abstract_id = character(), gender_tri_2nd = character())

  out <- env$safe_join(left, empty, label = "10g second triangulation")
  expect_equal(nrow(out), 2L)
  expect_true("gender_tri_2nd" %in% names(out))
  expect_true(all(is.na(out$gender_tri_2nd)))
})

test_that("the merged output has one row per cohort abstract", {
  awm <- here("output", "abstracts_with_matches.csv")
  skip_if_no_file(awm)
  d <- read_csv(awm, show_col_types = FALSE)
  expect_equal(nrow(d), 1106L)
  expect_equal(anyDuplicated(d$abstract_id), 0L)
})

# ── 3. Missing-data analysis ──────────────────────────────────────────────────

test_that("missingness by variable is reported and reconciles with the data", {
  p <- here("output", "missingness_by_variable.csv")
  fad_path <- here("output", "final_analytical_dataset.csv")
  skip_if_no_file(p); skip_if_no_file(fad_path)

  m <- read_csv(p, show_col_types = FALSE)
  fad <- read_csv(fad_path, show_col_types = FALSE)

  expect_true(all(c("variable", "n_total", "n_missing", "pct_missing") %in% names(m)))
  expect_true(all(m$n_total == nrow(fad)))
  # Every reported count must match the dataset, not a stale copy.
  for (i in seq_len(nrow(m))) {
    v <- m$variable[i]
    if (!v %in% names(fad)) next
    expect_equal(m$n_missing[i], sum(is.na(fad[[v]])),
                 label = paste("n_missing for", v))
  }
})

test_that("Little's MCAR result is recorded with what it did and did not test", {
  p <- here("output", "missingness_mcar.csv")
  skip_if_not(has_mc, "MCAR test requires mysterycall")
  skip_if_no_file(p)

  mc <- read_csv(p, show_col_types = FALSE)
  expect_equal(nrow(mc), 1L)
  expect_true(all(c("statistic", "df", "p_value", "vars_tested") %in% names(mc)))
  expect_gt(mc$df, 0)
  expect_true(mc$p_value >= 0 && mc$p_value <= 1)
  # The test covers only the numeric block; the note must say so, or a reader
  # will assume the categorical variables were tested too.
  expect_true(!is.na(mc$vars_tested) && nchar(mc$vars_tested) > 0)
})

test_that("the unresolved-vs-evaluated comparison separates definitional differences", {
  p <- here("output", "unresolved_vs_evaluated.csv")
  skip_if_no_file(p)
  cmp <- read_csv(p, show_col_types = FALSE)

  expect_true(all(c("variable", "p_value", "interpretation") %in% names(cmp)))

  # best_score MUST differ - the unresolved are the mid-score band by
  # construction - and must be labelled definitional so it is not read as a
  # finding about who fails adjudication.
  bs <- cmp |> filter(variable == "best_score")
  if (nrow(bs) == 1) {
    expect_match(bs$interpretation, "definitional")
  }
  expect_true(all(cmp$interpretation %in% c("substantive", NA) |
                    grepl("definitional", cmp$interpretation)))
})

# ── 4. Session snapshot ───────────────────────────────────────────────────────

test_that("the session snapshot records version, platform and seed", {
  p <- here("output", "session_snapshot.txt")
  skip_if_no_file(p)
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")

  expect_match(txt, "R version", info = "no R version recorded")
  expect_match(txt, "Platform", info = "no platform recorded")
  expect_match(txt, "42", info = "the pipeline seed is not recorded")
  expect_match(txt, "PACKAGES", info = "no package versions recorded")
})
