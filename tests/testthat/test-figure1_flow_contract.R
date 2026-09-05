# Figure 1 is the study's participant-flow diagram, and until now nothing in CI
# could catch a wrong number in it.
#
# R/strobe_flowchart.R asserted its own arithmetic, but it runs only from
# 00_run_all.R, which CI does not run: the full pipeline needs gitignored
# caches. test-cycle05_flow_fidelity_tables.R tested a RE-IMPLEMENTATION of
# those assertions against synthetic tuples, which proves the copy is
# self-consistent and nothing about the figure. And the figure itself is a
# tracked PNG whose numbers live in pixels, so it could depict a cohort the
# data no longer describes with the whole suite green.
#
# These tests run the real derivation against the tracked data on every push.

source(testthat::test_path("..", "..", "R", "utils_flow_counts.R"))

suppressPackageStartupMessages(library(readr))

root <- here::here()
P_ANALYTIC <- file.path(root, "output", "final_analytical_dataset.csv")
P_PARSED   <- file.path(root, "data", "processed", "abstracts_parsed.csv")
P_CLEANED  <- file.path(root, "data", "processed", "abstracts_cleaned.csv")
P_FIG      <- file.path(root, "output", "figures", "figure1_strobe_flowchart.png")

have_inputs <- all(file.exists(P_ANALYTIC, P_PARSED, P_CLEANED))

flow <- function() {
  a <- read_csv(P_ANALYTIC, show_col_types = FALSE, progress = FALSE)
  derive_flow_counts(a,
                     nrow(read_csv(P_PARSED, show_col_types = FALSE, progress = FALSE)),
                     nrow(read_csv(P_CLEANED, show_col_types = FALSE, progress = FALSE)))
}

test_that("every Figure 1 box closes against the committed data", {
  skip_if_not(have_inputs, "cohort files absent")
  f <- flow()
  expect_equal(f$parsed - f$video, f$cohort)
  expect_equal(f$evaluated + f$pending, f$cohort)
  expect_equal(f$published + f$not_published, f$evaluated)
  expect_equal(f$no_pub_found + f$pre_congress, f$not_published)
  expect_true(all(vapply(f, function(x) x >= 0L, logical(1))))
})

test_that("no abstract is counted published on a pre-congress paper", {
  # The outcome rule, not arithmetic. If this fails the cascade in
  # R/utils_decisions.R has regressed and Figure 1 is the least of it.
  skip_if_not(have_inputs, "cohort files absent")
  a <- read_csv(P_ANALYTIC, show_col_types = FALSE, progress = FALSE)
  offenders <- a$abstract_id[a$final_published %in% TRUE &
                               !is.na(a$months_to_pub) & a$months_to_pub < 0]
  expect_equal(length(offenders), 0L,
               info = paste("counted published despite a pre-congress date:",
                            paste(offenders, collapse = ", ")))
})

test_that("the derivation rejects data whose arithmetic does not close", {
  # Without this the contract above is only as good as the inputs: a derivation
  # that never refuses anything would pass on a corrupted dataset too.
  skip_if_not(have_inputs, "cohort files absent")
  a <- read_csv(P_ANALYTIC, show_col_types = FALSE, progress = FALSE)
  n_p <- nrow(read_csv(P_PARSED, show_col_types = FALSE, progress = FALSE))
  n_c <- nrow(read_csv(P_CLEANED, show_col_types = FALSE, progress = FALSE))

  # cohort size disagrees with the analytic table
  expect_error(derive_flow_counts(a, n_p, n_c - 1L), regexp = "n_cohort|==")
  # an abstract published on a paper that predates its congress
  bad <- a
  i <- which(!is.na(bad$months_to_pub) & bad$months_to_pub < 0)[1]
  skip_if(is.na(i), "no pre-congress row to doctor")
  bad$final_published[i] <- TRUE
  expect_error(derive_flow_counts(bad, n_p, n_c))
})

test_that("the README's Figure 1 alt text states the current numbers", {
  # The alt text carries seven typed numbers. test-docs_drift.R covers the
  # headline rate and denominator; the flow breakdown was covered by nothing,
  # and a typed number is exactly what goes stale after a cohort decision.
  skip_if_not(have_inputs, "cohort files absent")
  f <- flow()
  alt <- grep("STROBE participant-flow", readLines(file.path(root, "README.md"),
                                                   warn = FALSE), value = TRUE)
  expect_length(alt, 1)
  nums <- as.integer(gsub(",", "", regmatches(alt, gregexpr("[0-9][0-9,]*", alt))[[1]]))
  for (nm in c("parsed", "video", "cohort", "pending", "evaluated",
               "published", "not_published", "no_pub_found", "pre_congress")) {
    expect_true(f[[nm]] %in% nums,
                info = paste0("README Figure 1 alt text omits ", nm, " = ", f[[nm]]))
  }
})

test_that("the committed Figure 1 is not older than the data it depicts", {
  # git does not preserve mtimes, so this is a local guard, skipped on CI. It
  # catches the real case: re-running the pipeline and committing the dataset
  # without re-rendering the figure.
  #
  # GRACE exists because a checkout writes every file at once and their order
  # within that instant is arbitrary -- a fresh worktree had the figure 0.042s
  # behind the dataset. Forgetting to re-render is a gap of minutes at least,
  # so a five-minute window separates the two cases without hiding either.
  skip_if_not(have_inputs, "cohort files absent")
  skip_if_not(file.exists(P_FIG), "figure absent")
  skip_on_ci()
  GRACE <- 300
  lag <- as.numeric(file.mtime(P_ANALYTIC)) - as.numeric(file.mtime(P_FIG))
  expect_lt(lag, GRACE,
            label = sprintf(
              "figure1_strobe_flowchart.png is %.0f min older than final_analytical_dataset.csv; re-run R/strobe_flowchart.R -- lag",
              lag / 60))
})

test_that("strobe_flowchart.R derives its counts from the shared function", {
  # Guards the fix itself: reinstating a private copy of the derivation inside
  # the script would put the figure back outside CI's reach.
  src <- readLines(file.path(root, "R", "strobe_flowchart.R"), warn = FALSE)
  expect_true(any(grepl("derive_flow_counts", src, fixed = TRUE)))
  expect_false(any(grepl("^n_published <- sum", src)),
               info = "counts must come from derive_flow_counts(), not be recomputed inline")
})

test_that("exactly one artifact claims to be Figure 1", {
  # Two files were named figure1_*: the STROBE chart, and a classification-tier
  # view from 08_make_figures.R. They are different cuts of the cohort, so the
  # tier view was not deleted -- it was renamed to classification_tiers.*,
  # because the ambiguity was in the NAME, not the content. A reader who cites
  # "Figure 1" must land on one file.
  figs <- list.files(file.path(root, "output", "figures"), pattern = "^figure1")
  stems <- unique(sub("[.][^.]+$", "", figs))
  expect_equal(stems, "figure1_strobe_flowchart",
               info = paste("figure1_* artifacts found:", paste(figs, collapse = ", ")))
})

test_that("figure numbering has no gaps or repeats", {
  # A second figure1_ was possible because nothing checked the numbering.
  figs <- list.files(file.path(root, "output", "figures"), pattern = "^figure[0-9]+_")
  n <- as.integer(sub("^figure([0-9]+)_.*$", "\\1", figs))
  expect_equal(anyDuplicated(n), 0L,
               info = paste("two artifacts share a figure number:",
                            paste(figs[duplicated(n) | duplicated(n, fromLast = TRUE)],
                                  collapse = ", ")))
  expect_equal(sort(n), seq_len(length(n)),
               info = paste("figure numbers are not 1..n:", paste(sort(n), collapse = ", ")))
})

test_that("the retired flow generator is gone and nothing calls it", {
  expect_false(file.exists(file.path(root, "R", "strobe_flow_diagram.R")))
  live <- c(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
            file.path(root, "00_run_all.R"))
  live <- live[file.exists(live)]
  callers <- Filter(function(f) any(grepl("strobe_flow_diagram", readLines(f, warn = FALSE))), live)
  expect_length(callers, 0L)
})
