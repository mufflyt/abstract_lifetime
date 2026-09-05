# Cycle 8 of 24. Mix: 3 BVA, 4 semantic, 3 adversarial.
#
# Targets: identifier contracts, pipeline ordering, document rendering, and
# environment independence. Cycle 7 ended with an undefined variable that would
# have broken the knit and was caught by rendering rather than by a test; 8.6
# closes that gap.

library(testthat)
library(dplyr)

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_RUN   <- here::here("00_run_all.R")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")
rd <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")

# ============================================================
# BVA 8.1 — identifier format at its edges
# ============================================================
test_that("abstract_id format holds at the first and last id of every congress", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  expect_true(all(grepl("^AAGL[0-9]{4}_[0-9]{3}$", f$abstract_id)),
              label = "an abstract_id departs from AAGL<year>_<nnn>")
  edges <- f |> group_by(congress_year) |>
    summarise(lo = min(abstract_id), hi = max(abstract_id), n = n(), .groups = "drop")
  # Sequence numbers are zero-padded to three digits, so a congress with more
  # than 999 abstracts would silently collide. Assert the headroom is real.
  expect_true(all(edges$n <= 999),
              label = "a congress has more abstracts than the id format can encode")
  expect_true(all(as.integer(sub(".*_", "", edges$lo)) >= 1))
})

# ============================================================
# BVA 8.2 — congress_year is a plain year, not a coerced factor or string
# ============================================================
test_that("congress_year is numeric and inside the study window", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  expect_true(is.numeric(f$congress_year),
              label = paste("congress_year read back as", class(f$congress_year)[1]))
  expect_true(all(f$congress_year == floor(f$congress_year)),
              label = "a fractional congress_year would break every date join")
  expect_gte(min(f$congress_year), 2000)
  expect_lte(max(f$congress_year), as.integer(format(Sys.Date(), "%Y")))
})

# ============================================================
# BVA 8.3 — no duplicate or empty identifiers
# ============================================================
test_that("abstract_id is unique and non-empty across the cohort", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  expect_equal(anyDuplicated(f$abstract_id), 0L,
               label = "a duplicated abstract_id would double-weight that abstract")
  expect_false(any(is.na(f$abstract_id) | trimws(f$abstract_id) == ""))
})

# ============================================================
# SEMANTIC 8.4 — the identifier's year is the congress year
# ============================================================
test_that("the year embedded in abstract_id matches congress_year", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  id_year <- as.integer(substr(f$abstract_id, 5, 8))
  mismatch <- sum(id_year != f$congress_year)
  expect_equal(mismatch, 0L,
               label = paste(mismatch, "abstracts carry an id year different from",
                             "congress_year; every per-year denominator depends on these agreeing"))
})

# ============================================================
# SEMANTIC 8.5 — the runner sources stages in dependency order
# ============================================================
test_that("00_run_all.R sources stages in dependency order", {
  need(P_RUN)
  src <- rd(P_RUN)
  sourced <- gsub('.*"R", "|"\\)\\)', "",
                  regmatches(src, gregexpr('source\\(here\\("R", "[^"]+"\\)\\)', src))[[1]])
  skip_if(length(sourced) < 5, "too few staged sources")
  pos <- function(pattern) {
    i <- grep(pattern, sourced)
    if (length(i) == 0) NA_integer_ else min(i)
  }
  # File numbering is NOT the dependency order: the 09* and 10* enrichment
  # stages deliberately run before 06_analyze_results, because the analysis
  # consumes the demographics they produce. An earlier draft of this test
  # asserted ascending numeric order and its premise was wrong. These are the
  # relationships that actually have to hold.
  deps <- list(
    c("^02_clean",    "^03_search"),
    c("^03_search",   "^04_score"),
    c("^04_score",    "^05_adjudicate"),
    c("^05_adjudicate", "^06_analyze"),
    c("^09_enrich",   "^06_analyze"),
    c("^06_analyze",  "^07_make_tables"),
    c("^06_analyze",  "^08_make_figures")
  )
  for (d in deps) {
    a <- pos(d[1]); b <- pos(d[2])
    if (is.na(a) || is.na(b)) next
    expect_lt(a, b, label = paste(d[1], "must be sourced before", d[2]))
  }
})

# ============================================================
# SEMANTIC 8.6 — the manuscript documents actually render
# ============================================================
test_that("both Rmd documents knit without error", {
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE), "rmarkdown absent")
  # Sys.which("pandoc") misses the copy rmarkdown bundles, which made this test
  # skip on a machine where rendering demonstrably works. A test that never runs
  # gives false assurance, so ask rmarkdown itself.
  skip_if_not(rmarkdown::pandoc_available(), "pandoc unavailable")
  for (nm in c("abstract_results_section.Rmd", "technical_appendix.Rmd")) {
    p <- here::here("docs", nm)
    if (!file.exists(p)) next
    out <- tempfile(fileext = ".docx")
    res <- tryCatch({ rmarkdown::render(p, output_file = out, quiet = TRUE); "ok" },
                    error = function(e) conditionMessage(e))
    expect_equal(res, "ok",
                 label = paste(nm, "failed to knit:", res))
  }
})

# ============================================================
# SEMANTIC 8.7 — the analysis stage is idempotent
# ============================================================
test_that("the analysis transform is idempotent and order-independent", {
  need(P_FINAL)
  source(here::here("R", "utils_decisions.R"))
  dec_p <- here::here("output", "manual_review_decisions.csv")
  skip_if(!file.exists(dec_p), "decisions absent")

  d <- readr::read_csv(dec_p, show_col_types = FALSE)
  # months_to_pub is part of the contract since the pre-congress exclusion
  # became the first branch of the cascade (PI decision, 2026-09-05).
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE) |>
    dplyr::select(abstract_id, classification, best_pmid, months_to_pub)

  # Shelling out to 06_analyze_results.R made this test skip whenever the stage
  # could not run in the harness, which is a hole rather than a safeguard. The
  # property that matters is that the transform is a pure function of its
  # inputs: same inputs give the same output, twice and in any row order.
  run <- function(dd, ff) {
    publication_rate_summary(assign_final_published(ff, dedup_decisions_for_analysis(dd)))
  }
  a <- run(d, f)
  b <- run(d, f)
  expect_equal(a, b, label = "the transform is not idempotent on identical inputs")

  set.seed(42)
  c_ <- run(d[sample(nrow(d)), ], f[sample(nrow(f)), ])
  expect_equal(c_$n_cohort, a$n_cohort)
  expect_equal(c_$n_pending, a$n_pending)
  expect_equal(c_$n_published, a$n_published)
  expect_equal(c_$publication_rate, a$publication_rate,
               label = "shuffling input rows changed the publication rate")
})

# ============================================================
# ADVERSARIAL 8.8 — no script depends on the working directory
# ============================================================
test_that("R scripts address files through here(), not relative paths", {
  files <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)
  skip_if(length(files) == 0, "no R scripts")
  offenders <- character(0)
  for (p in files) {
    lines <- readLines(p, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)]
    # read_csv("data/...") or write_csv(x, "output/...") without here()
    hits <- grep('(read|write)_[a-z]+\\(\\s*[^)]*"(data|output|docs)/', lines, value = TRUE)
    hits <- hits[!grepl("here\\(", hits)]
    if (length(hits)) offenders <- c(offenders, paste0(basename(p), ": ", trimws(hits[1])))
  }
  expect_length(offenders, 0L)
  if (length(offenders)) {
    fail(paste("relative paths make the stage depend on the caller's cwd:",
               paste(offenders, collapse = " | ")))
  }
})

# ============================================================
# ADVERSARIAL 8.9 — rendered documents are not older than their sources
# ============================================================
# NOTE: mtime ordering catches only "source newer than output". A .docx can be
# newer than its .Rmd and still report stale NUMBERS, because the chunks read
# output/ at knit time. Content-level staleness is not detectable here; the
# cross-artifact tests in cycle 7 cover the claims that matter most.
test_that("rendered .docx files are not staler than the .Rmd they came from", {
  for (nm in c("abstract_results_section", "technical_appendix")) {
    rmd_p  <- here::here("docs", paste0(nm, ".Rmd"))
    docx_p <- here::here("docs", paste0(nm, ".docx"))
    if (!file.exists(rmd_p) || !file.exists(docx_p)) next
    gap <- as.numeric(difftime(file.info(docx_p)$mtime, file.info(rmd_p)$mtime,
                               units = "secs"))
    expect_gte(gap, -60,
               label = paste(nm, ".docx predates its .Rmd by",
                             round(-gap / 60), "minutes; it describes older numbers"))
  }
  succeed()
})

# ============================================================
# ADVERSARIAL 8.10 — the Shiny bundle keeps the main schema
# ============================================================
test_that("the Shiny bundle shares the main cohort's schema and ids", {
  main   <- here::here("data", "processed", "abstracts_cleaned.csv")
  bundle <- here::here("shiny", "adjudication_app", "bundle", "data",
                       "processed", "abstracts_cleaned.csv")
  if (!file.exists(bundle) || !file.exists(main)) skip("bundle absent")
  a <- readr::read_csv(main, show_col_types = FALSE, n_max = 5000)
  b <- readr::read_csv(bundle, show_col_types = FALSE, n_max = 5000)
  expect_equal(sort(names(a)), sort(names(b)),
               label = "bundle and main cohort have diverged in schema")
  expect_true(setequal(a$abstract_id, b$abstract_id),
              label = "bundle and main cohort contain different abstracts")
})
