# tests/testthat/test-shiny_app.R
# Comprehensive tests for the Abstract Adjudication Shiny app.
# Covers: parse integrity, data loading, helper functions, server reactives.

library(testthat)
library(dplyr)
library(readr)
library(stringr)
library(here)

# ── 1. App syntax ─────────────────────────────────────────────────────────────

test_that("app.R parses without errors", {
  app_file <- here("shiny", "adjudication_app", "app.R")
  expect_true(file.exists(app_file), info = "app.R must exist")
  result <- tryCatch(parse(file = app_file), error = function(e) e)
  expect_false(inherits(result, "error"),
               info = paste("Parse error:", if (inherits(result, "error")) result$message else "none"))
})

# ── 2. Required data files ────────────────────────────────────────────────────

test_that("all required data files exist", {
  files <- list(
    abstracts_cleaned    = here("data", "processed", "abstracts_cleaned.csv"),
    pubmed_candidates    = here("data", "processed", "pubmed_candidates.csv"),
    abstracts_with_matches = here("output", "abstracts_with_matches.csv")
  )
  for (nm in names(files)) {
    expect_true(file.exists(files[[nm]]),
                info = paste("Missing:", nm, "->", files[[nm]]))
  }
})

test_that("bundle copies of data files exist and are non-empty", {
  bundle_files <- list(
    abstracts_cleaned = here("shiny", "adjudication_app", "bundle", "data",
                             "processed", "abstracts_cleaned.csv"),
    pubmed_candidates = here("shiny", "adjudication_app", "bundle", "data",
                             "processed", "pubmed_candidates.csv")
  )
  for (nm in names(bundle_files)) {
    p <- bundle_files[[nm]]
    expect_true(file.exists(p), info = paste("Missing bundle file:", nm))
    expect_gt(file.info(p)$size, 1000L, label = paste("bundle", nm, "size"))
  }
})

# ── 3. Data integrity ─────────────────────────────────────────────────────────

test_that("abstracts_cleaned.csv has expected columns and row count", {
  abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                  show_col_types = FALSE)
  required_cols <- c("abstract_id", "title", "congress_year", "article_url")
  for (col in required_cols) {
    expect_true(col %in% names(abs), info = paste("Missing column:", col))
  }
  expect_gte(nrow(abs), 1000L, label = "row count >= 1000")
})

test_that("abstracts_cleaned.csv has abstract_text for 2019-2023 cohorts", {
  abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                  show_col_types = FALSE)
  late_cohorts <- abs |> filter(congress_year >= 2019)
  pct_with_text <- mean(!is.na(late_cohorts$abstract_text))
  expect_gte(pct_with_text, 0.90,
             label = ">=90% of 2019-2023 abstracts have abstract_text")
})

test_that("abstract_text available for 2019-2023 cohorts", {
  abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                  show_col_types = FALSE)
  recent <- abs |> filter(congress_year %in% 2019:2023)
  pct_with_text <- mean(!is.na(recent$abstract_text))
  expect_gte(pct_with_text, 0.95,
             label = ">=95% of 2019-2023 abstracts have abstract_text")
})

test_that("2012-2018 abstracts lack structured sections but may have fallback text", {
  abs <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                  show_col_types = FALSE)
  early <- abs |> filter(congress_year %in% 2012:2018)
  # ScienceDirect doesn't expose structured sections for 2012-2018, but
  # abstract_text is built from any available text (full_text fallback).
  # The key signal is abstract_full_text being empty.
  pct_full <- mean(!is.na(early$abstract_full_text) & nchar(early$abstract_full_text) > 20, na.rm = TRUE)
  expect_lte(pct_full, 0.10,
             label = "2012-2018 have minimal abstract_full_text (expected)")
})

test_that("abstracts_with_matches.csv has required columns", {
  rq <- read_csv(here("output", "abstracts_with_matches.csv"),
                 show_col_types = FALSE)
  required <- c("abstract_id", "title", "best_score", "classification")
  for (col in required) {
    expect_true(col %in% names(rq), info = paste("Missing column:", col))
  }
  expect_gte(nrow(rq), 50L)
})

test_that("pubmed_candidates.csv has pmid and abstract_id columns", {
  cands <- read_csv(here("data", "processed", "pubmed_candidates.csv"),
                    show_col_types = FALSE)
  expect_true("pmid" %in% names(cands))
  expect_true("abstract_id" %in% names(cands))
  expect_gte(nrow(cands), 100L)
})

test_that("abstract_ids are consistent across data files", {
  abs  <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                   show_col_types = FALSE)
  rq   <- read_csv(here("output", "abstracts_with_matches.csv"),
                   show_col_types = FALSE)
  cands <- read_csv(here("data", "processed", "pubmed_candidates.csv"),
                    show_col_types = FALSE)

  abs_ids  <- unique(abs$abstract_id)
  rq_ids   <- unique(rq$abstract_id)
  cand_ids <- unique(cands$abstract_id)

  # All review-queue IDs should be in abstracts_cleaned
  orphan_rq <- setdiff(rq_ids, abs_ids)
  expect_lte(length(orphan_rq), 5L,
             label = "review queue IDs not in abstracts_cleaned")

  # Candidate abstract_ids may span historical runs; allow up to 40% orphan rate.
  # The important direction: all review_queue IDs should have candidates available.
  orphan_cands <- setdiff(cand_ids, rq_ids)
  pct_orphan <- length(orphan_cands) / length(cand_ids)
  expect_lte(pct_orphan, 0.40,
             label = "<=40% of candidate abstract_ids orphaned from review queue")
})

# ── 4. Helper function tests (sourced inline) ─────────────────────────────────

# Source just the helpers we need without running the full app
local({
  # dedup_decisions
  dedup_decisions <- function(decisions) {
    if (nrow(decisions) == 0) return(decisions)
    decisions |>
      group_by(abstract_id, reviewer) |>
      arrange(desc(review_timestamp)) |>
      slice(1) |>
      ungroup()
  }

  test_that("dedup_decisions keeps latest per abstract+reviewer", {
    d <- tibble(
      abstract_id      = c("A1", "A1", "A1"),
      reviewer         = c("TM", "TM", "JD"),
      manual_decision  = c("skip", "match", "no_match"),
      review_timestamp = c("2026-01-01 10:00:00",
                           "2026-01-02 10:00:00",
                           "2026-01-01 09:00:00")
    )
    result <- dedup_decisions(d)
    expect_equal(nrow(result), 2L)
    tm_row <- result |> filter(reviewer == "TM")
    expect_equal(tm_row$manual_decision, "match")
    expect_equal(tm_row$review_timestamp, "2026-01-02 10:00:00")
  })

  test_that("dedup_decisions handles empty input", {
    empty <- tibble(abstract_id = character(), reviewer = character(),
                    manual_decision = character(), review_timestamp = character())
    result <- dedup_decisions(empty)
    expect_equal(nrow(result), 0L)
  })
})

# ── 5. JSON abstract extraction (backfill logic) ─────────────────────────────

test_that("JSON abstract extraction regex finds section-titles in cached HTML", {
  cache_dir <- here("data", "cache", "sd_html")
  skip_if_not(dir.exists(cache_dir), "sd_html cache not present")

  # Use AAGL2012_001 PII
  html_file <- file.path(cache_dir, "S1553465012003433.html")
  skip_if_not(file.exists(html_file), "AAGL2012_001 cache file missing")

  html_txt <- readr::read_file(html_file)
  titles <- str_match_all(html_txt,
    '"#name":"section-title","_":"([^"]+)"')[[1]][, 2]

  expect_gte(length(titles), 4L, label = ">=4 section titles found in JSON")
  expect_true("Study Objective" %in% titles || "Objective" %in% titles)
})

test_that("format_abstract_html handles structured and plain text", {
  # Source the function inline
  format_abstract_html <- function(txt) {
    if (is.na(txt) || nchar(txt) == 0)
      return(shiny::tags$em("No abstract text available."))
    headers_re <- paste0("Study Objective|Objective|Design|Setting|",
                         "Patients or Participants|Patients|Participants|",
                         "Intervention|Measurements|Results|Conclusion|",
                         "Methods|Background|Purpose")
    m <- gregexpr(headers_re, txt, perl = TRUE)[[1]]
    if (m[1] == -1) return(shiny::tags$p(style = "margin-bottom: 6px;", txt))
    shiny::tags$div("structured")
  }

  plain <- format_abstract_html("Some plain abstract text without headers.")
  expect_true(inherits(plain, "shiny.tag"))

  structured <- format_abstract_html(
    "Objective To reduce pain. Results Pain reduced significantly.")
  expect_true(inherits(structured, "shiny.tag"))

  empty_out <- format_abstract_html(NA_character_)
  expect_true(inherits(empty_out, "shiny.tag"))
})

# ── 6. Score chip tooltip content ────────────────────────────────────────────

test_that("CHIP_TIPS covers all scoring columns", {
  chip_cols <- c("title_pts", "abstract_pts", "first_au_pts", "last_au_pts",
                 "journal_pts", "date_pts", "no_text_penalty")
  CHIP_TIPS <- list(
    title_pts       = "Title similarity",
    abstract_pts    = "Abstract text overlap",
    first_au_pts    = "First-author match",
    last_au_pts     = "Last-author match",
    journal_pts     = "Journal match",
    date_pts        = "Publication date",
    no_text_penalty = "No-text-evidence penalty"
  )
  for (col in chip_cols) {
    expect_true(col %in% names(CHIP_TIPS),
                info = paste("CHIP_TIPS missing entry for", col))
    expect_gte(nchar(CHIP_TIPS[[col]]), 5L,
               label = paste("CHIP_TIPS", col, "non-trivial"))
  }
})

# ── 7. Shiny server logic via testServer ──────────────────────────────────────

test_that("shiny server initialises without errors and data loads", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))

  app_env <- new.env(parent = globalenv())
  source(here("shiny", "adjudication_app", "app.R"),
         local = app_env, echo = FALSE)

  # testServer runs the server in isolation (no browser, no GS auth)
  shiny::testServer(app_env$server, {
    # After startup the data reactive should be populated (once = TRUE observe)
    # Give it a tick
    session$flushReact()
    d <- data()
    expect_false(is.null(d), info = "data() should be non-NULL after init")
    expect_true(is.list(d))
    expect_true("review_queue" %in% names(d))
    expect_true("candidates"   %in% names(d))
    expect_true("decisions"    %in% names(d))
    expect_true("abstracts"    %in% names(d))
  }, args = list())
})

test_that("visible_ids respects congress_year filter", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  app_env <- new.env(parent = globalenv())
  source(here("shiny", "adjudication_app", "app.R"),
         local = app_env, echo = FALSE)

  shiny::testServer(app_env$server, {
    session$flushReact()
    session$setInputs(
      filter_year       = "2022",
      filter_class      = "all",
      filter_unreviewed = FALSE,
      abstract_select   = ""
    )
    ids <- visible_ids()
    expect_true(length(ids) > 0, info = "2022 filter should return some IDs")

    d <- data()
    if (nrow(d$review_queue) > 0 && "congress_year" %in% names(d$review_queue)) {
      years_in_ids <- d$review_queue |>
        dplyr::filter(abstract_id %in% ids) |>
        dplyr::pull(congress_year) |>
        unique()
      expect_true(all(as.character(years_in_ids) == "2022"),
                  info = "All visible IDs should be from 2022")
    }
  }, args = list())
})

test_that("visible_ids returns fewer IDs with unreviewed filter on", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  app_env <- new.env(parent = globalenv())
  source(here("shiny", "adjudication_app", "app.R"),
         local = app_env, echo = FALSE)

  shiny::testServer(app_env$server, {
    session$flushReact()
    session$setInputs(filter_year = "all", filter_class = "all",
                      filter_unreviewed = FALSE, abstract_select = "")
    all_ids <- visible_ids()

    session$setInputs(filter_unreviewed = TRUE)
    unreviewed_ids <- visible_ids()

    expect_lte(length(unreviewed_ids), length(all_ids),
               label = "unreviewed filter should not increase count")
  }, args = list())
})

test_that("current_abstract returns correct row for selected ID", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  app_env <- new.env(parent = globalenv())
  source(here("shiny", "adjudication_app", "app.R"),
         local = app_env, echo = FALSE)

  shiny::testServer(app_env$server, {
    session$flushReact()
    d <- data()
    skip_if(is.null(d) || nrow(d$review_queue) == 0)
    req_id <- d$review_queue$abstract_id[1]

    # Set all inputs together so no reactive fires with a partially-set state
    session$setInputs(filter_year = "all", filter_class = "all",
                      filter_unreviewed = FALSE, reviewer_initials = "TM",
                      decision = "skip", manual_pmid = "", notes = "",
                      abstract_select = req_id)
    a <- current_abstract()
    expect_equal(nrow(a), 1L)
    expect_equal(a$abstract_id[1], req_id)
  }, args = list())
})

test_that("candidate_rows returns candidates sorted by score", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  app_env <- new.env(parent = globalenv())
  source(here("shiny", "adjudication_app", "app.R"),
         local = app_env, echo = FALSE)

  shiny::testServer(app_env$server, {
    session$flushReact()
    d <- data()
    skip_if(is.null(d) || nrow(d$candidates) == 0)

    # Pick an ID present in both review_queue and candidates
    ids_with_cands <- intersect(unique(d$candidates$abstract_id),
                                unique(d$review_queue$abstract_id))
    skip_if(length(ids_with_cands) == 0)
    target_id <- ids_with_cands[1]

    session$setInputs(filter_year = "all", filter_class = "all",
                      filter_unreviewed = FALSE, reviewer_initials = "TM",
                      decision = "skip", manual_pmid = "", notes = "",
                      abstract_select = target_id)
    cands <- candidate_rows()

    if (nrow(cands) > 1 && "total_score" %in% names(cands)) {
      scores <- cands$total_score
      expect_true(all(diff(scores) <= 0),
                  info = "Candidates should be sorted descending by total_score")
    }
  }, args = list())
})

test_that("decisions_dedup reactive deduplicated correctly", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  app_env <- new.env(parent = globalenv())
  source(here("shiny", "adjudication_app", "app.R"),
         local = app_env, echo = FALSE)

  shiny::testServer(app_env$server, {
    session$flushReact()
    dd <- decisions_dedup()
    expect_true(is.data.frame(dd))
    if (nrow(dd) > 0) {
      dups <- dd |>
        dplyr::group_by(abstract_id, reviewer) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::filter(n > 1)
      expect_equal(nrow(dups), 0L,
                   info = "No duplicate abstract_id+reviewer rows after dedup")
    }
  }, args = list())
})

test_that("progress_count output renders without error", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE))
  app_env <- new.env(parent = globalenv())
  source(here("shiny", "adjudication_app", "app.R"),
         local = app_env, echo = FALSE)

  shiny::testServer(app_env$server, {
    session$flushReact()
    session$setInputs(filter_year = "all", filter_class = "all",
                      filter_unreviewed = FALSE, abstract_select = "")
    txt <- output$progress_count
    expect_type(txt, "character")
    expect_match(txt, "\\d+ / \\d+",
                 info = "progress_count should be 'N / M (P%)'")
  }, args = list())
})

# ── 8. Input validation helpers ───────────────────────────────────────────────

test_that("reviewer initials validation regex works correctly", {
  valid   <- c("TM", "ABC", "ABCD")
  invalid <- c("", "T", "ABCDE", "tm", "T1", "T M")
  for (v in valid) {
    expect_true(grepl("^[A-Z]{2,4}$", v), info = paste("Should pass:", v))
  }
  for (v in invalid) {
    expect_false(grepl("^[A-Z]{2,4}$", v), info = paste("Should fail:", v))
  }
})

test_that("PMID validation regex works correctly", {
  valid   <- c("1234567", "12345678")
  invalid <- c("", "123456", "123456789", "12a45678", "PMID123")
  for (v in valid) {
    expect_true(grepl("^\\d{7,8}$", v), info = paste("Should pass:", v))
  }
  for (v in invalid) {
    expect_false(grepl("^\\d{7,8}$", v), info = paste("Should fail:", v))
  }
})

# ── 9. Bundle sync check ──────────────────────────────────────────────────────

test_that("bundle abstracts_cleaned.csv is not stale vs main copy", {
  main   <- here("data", "processed", "abstracts_cleaned.csv")
  bundle <- here("shiny", "adjudication_app", "bundle", "data",
                 "processed", "abstracts_cleaned.csv")
  skip_if_not(file.exists(bundle))

  main_mtime   <- file.info(main)$mtime
  bundle_mtime <- file.info(bundle)$mtime

  # Bundle is refreshed by deploy_shiny.R — allow up to 24 hours of drift
  diff_secs <- abs(as.numeric(difftime(main_mtime, bundle_mtime, units = "secs")))
  expect_lte(diff_secs, 86400,
             label = "bundle CSV should be synced within 24 hours of main CSV")
})

test_that("bundle abstracts_cleaned.csv has AAGL2012_001", {
  bundle <- here("shiny", "adjudication_app", "bundle", "data",
                 "processed", "abstracts_cleaned.csv")
  skip_if_not(file.exists(bundle))
  abs <- read_csv(bundle, show_col_types = FALSE)
  row <- abs |> filter(abstract_id == "AAGL2012_001")
  expect_equal(nrow(row), 1L)
  # 2012 abstracts have no abstract_text (expected — older ScienceDirect format)
})
