# test-deidentification_guard.R — study-staff identities must not re-enter the
# repository.
#
# The adjudication log records who made each decision. Those are study staff and
# their identifiers do not belong in a public repository. R/utils_reviewer_ids.R
# maps them to stable pseudonyms; this file is the guard that stops the mapping
# being bypassed, reverted, or forgotten in a new artifact.
#
# DELIBERATELY NOT GUARDED: published author names in the abstract, PubMed,
# Crossref and OpenAlex data. Those are the public scientific record and the
# dataset is meaningless without them. The scan below is scoped to the files
# that carry staff identity, not to author fields.

library(testthat)
source(here::here("R", "utils_reviewer_ids.R"))

# Files that record who did the work, as opposed to who wrote the science.
STAFF_FILES <- c(
  file.path("output", "manual_review_decisions.csv"),
  file.path("output", "manual_review_queue.csv"),
  file.path("docs", "ADJUDICATION.md")
)

# ============================================================
# The mapping itself
# ============================================================
test_that("pseudonymise_reviewer maps known staff to stable pseudonyms", {
  expect_equal(pseudonymise_reviewer(c("GW", "JM", "TMM")), c("R01", "R02", "R03"))
  # Deterministic across calls, so re-running a stage cannot renumber reviewers
  # and silently break a per-reviewer comparison.
  expect_equal(pseudonymise_reviewer("GW"), pseudonymise_reviewer("GW"))
  # Idempotent, so applying it twice is safe.
  expect_equal(pseudonymise_reviewer(pseudonymise_reviewer("GW")), "R01")
})

test_that("the algorithmic marker and missing values are preserved", {
  # R/06_analyze_results.R, R/10_interrater.R and the
  # Shiny app all branch on the literal "AUTO". Renaming it would silently
  # change adjudication precedence.
  expect_equal(pseudonymise_reviewer("AUTO"), "AUTO")
  expect_true(is.na(pseudonymise_reviewer(NA_character_)))
  expect_equal(pseudonymise_reviewer(""), "")
})

test_that("an unrecognised reviewer is pseudonymised rather than passed through", {
  out <- pseudonymise_reviewer("SOMEONE_NEW")
  expect_match(out, "^R[0-9]{2}$")
  expect_false(identical(out, "SOMEONE_NEW"))
  # A new reviewer joining the study must not leak an identity merely by being
  # absent from the lookup.
  expect_false(any(grepl("SOMEONE_NEW", out)))
})

test_that("pseudonymisation preserves the grouping structure it is applied to", {
  x <- c("GW", "GW", "JM", "AUTO", "TMM", "GW")
  y <- pseudonymise_reviewer(x)
  # Interrater agreement, per-reviewer counts and the human/AUTO precedence rule
  # all depend only on which rows share a reviewer. That must be unchanged.
  expect_equal(unname(table(y)[order(names(table(y)))]),
               unname(table(x)[order(c("AUTO", "R01", "R02", "R03"))]),
               ignore_attr = TRUE)
  expect_equal(length(unique(y)), length(unique(x)))
  expect_equal(y == y[1], x == x[1])
})

# ============================================================
# The guard
# ============================================================
test_that("no staff identity appears in the files that record who did the work", {
  pats <- reviewer_identity_patterns()
  offenders <- character(0)
  # Scan CSVs COLUMN-WISE, not as raw text. An earlier draft grepped whole lines
  # and flagged "GW" and "JM" occurring inside published abstract titles and
  # author strings. Those are the public scientific record; treating them as
  # staff identity would be a false positive that pressures a real deletion.
  identity_cols <- c("reviewer", "adjudicator", "rater", "reviewer_notes")
  for (rel in STAFF_FILES) {
    p <- here::here(rel)
    if (!file.exists(p)) next
    if (grepl("\\.csv$", p)) {
      d <- readr::read_csv(p, show_col_types = FALSE, progress = FALSE)
      for (cc in intersect(identity_cols, names(d))) {
        v <- as.character(d[[cc]])
        v <- v[!is.na(v)]
        for (pat in pats) {
          if (any(grepl(pat, v))) {
            offenders <- c(offenders, sprintf("%s$%s matches %s", rel, cc, pat))
          }
        }
      }
    } else {
      txt <- readLines(p, warn = FALSE)
      for (pat in pats) {
        hits <- grep(pat, txt)
        if (length(hits)) {
          offenders <- c(offenders, sprintf("%s:%d matches %s", rel, hits[1], pat))
        }
      }
    }
  }
  expect_true(length(offenders) == 0,
              label = paste("study-staff identity present in tracked files:",
                            paste(offenders, collapse = "; ")))
})

test_that("the reviewer column contains only pseudonyms and the AUTO marker", {
  p <- here::here("output", "manual_review_decisions.csv")
  skip_if(!file.exists(p), "decisions log absent")
  d <- readr::read_csv(p, show_col_types = FALSE)
  skip_if(!"reviewer" %in% names(d), "no reviewer column")
  vals <- unique(d$reviewer[!is.na(d$reviewer)])
  bad <- vals[!grepl("^(R[0-9]{2}|AUTO)$", vals)]
  expect_true(length(bad) == 0,
              label = paste("reviewer column carries non-pseudonymous value(s):",
                            paste(bad, collapse = ", ")))
})

test_that("test fixtures use pseudonyms, so the guard cannot be defeated by a fixture", {
  files <- list.files(here::here("tests", "testthat"), pattern = "\\.R$", full.names = TRUE)
  files <- setdiff(files, here::here("tests", "testthat", "test-deidentification_guard.R"))
  offenders <- character(0)
  for (p in files) {
    txt <- readLines(p, warn = FALSE)
    hits <- grep('"(GW|JM|TMM)"', txt)
    if (length(hits)) offenders <- c(offenders, sprintf("%s:%d", basename(p), hits[1]))
  }
  expect_true(length(offenders) == 0,
              label = paste("test fixtures still use real staff initials:",
                            paste(offenders, collapse = ", ")))
})
