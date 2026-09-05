# The publication date is the PRINT ISSUE date. PI decision, 2026-09-05.
#
# PubMed carries at least two dates for an article: ArticleDate, the electronic
# release, and JournalIssue/PubDate, the print issue. They routinely differ by
# months. Which one the study means was never written down, and the repository
# had quietly adopted both at once: R/utils_pubmed.R reads JournalIssue/PubDate
# for every interval in the analysis, while
# output/excluded_pre_congress_publications.csv was built from ArticleDate. On
# the four contested pre-congress abstracts the two disagree by 1.5 to 4.9
# months, and for one of them by the difference between "five months before the
# congress" and "six days before".
#
# The decision is the print issue date, so the analysis is already correct. What
# was missing was anything preventing it from drifting back. These tests are
# that.

library(testthat)
suppressPackageStartupMessages({library(readr); library(dplyr)})

repo_root <- here::here()
PARSER <- file.path(repo_root, "R", "utils_pubmed.R")

test_that("the PubMed parser reads the issue date, not the electronic date", {
  skip_if_not(file.exists(PARSER))
  txt <- readLines(PARSER, warn = FALSE)

  # The date the parser actually assembles must come from JournalIssue/PubDate.
  uses_issue <- grep("JournalIssue/PubDate/(Year|Month|Day)", txt)
  expect_gt(length(uses_issue), 0,
            label = paste("R/utils_pubmed.R no longer reads JournalIssue/PubDate.",
                          "The publication date is the print issue date (PI decision,",
                          "2026-09-05); see docs/OUTCOME_DEFINITION.md"))

  # ArticleDate may be parsed and carried for reference, but must not be the
  # source of the year/month/day the interval is computed from.
  article_date_assigned <- grep(
    "^\\s*(year|month|day)\\s*<-.*ArticleDate", txt)
  expect_equal(
    length(article_date_assigned), 0,
    label = paste("R/utils_pubmed.R assigns the publication date from",
                  "ArticleDate. The study dates publications to the print issue;",
                  "an electronic date shortens every interval and moves papers",
                  "across the pre-congress boundary. Lines:",
                  paste(article_date_assigned, collapse = ", ")))
})

test_that("month-only issue dates resolve to the first of the month", {
  skip_if_not(file.exists(PARSER))
  txt <- paste(readLines(PARSER, warn = FALSE), collapse = "\n")
  # The convention is stated in the manuscript Methods, so it has to hold here.
  expect_match(
    txt, 'JournalIssue/PubDate/Day"\\)\\)\\s*%\\|\\|%\\s*"01"',
    label = paste("the parser no longer defaults an absent issue day to 01.",
                  "The Methods state that month-only issue dates resolve to the",
                  "first of the month"))
})

test_that("the manuscript states which date it means", {
  p <- file.path(repo_root, "docs", "abstract_results_section.Rmd")
  skip_if_not(file.exists(p))
  txt <- paste(readLines(p, warn = FALSE), collapse = " ")

  expect_match(txt, "print issue date", fixed = TRUE,
               label = paste("the Methods no longer define the publication date.",
                             "'the publication date' unqualified is ambiguous:",
                             "PubMed offers an electronic date and a print date",
                             "that differ by months"))
  expect_match(txt, "ahead of print", fixed = TRUE,
               label = "the Methods no longer say how online-ahead-of-print is dated")
})

test_that("the contested intervals reproduce the issue date, not the electronic one", {
  # The sharpest available check, on committed data. For the four abstracts
  # whose pre-congress status is under review, the two candidate date bases give
  # very different answers, so the recorded interval identifies which basis
  # produced it beyond doubt.
  #
  #   abstract       issue date -> months    electronic date -> months
  #   AAGL2021_002   2021-07-01     4.47     2021-05-13          6.09
  #   AAGL2021_049   2021-10-01     1.45     2021-06-12          5.10
  #   AAGL2023_042   2023-08-01     3.22     2023-04-23          6.46
  #   AAGL2023_048   2023-11-01     0.20     2023-06-04          5.10
  #
  # Verified against data/cache/pubmed_xml/<pmid>.xml on 2026-09-05.
  fad <- file.path(repo_root, "output", "final_analytical_dataset.csv")
  skip_if_not(file.exists(fad), "analytical dataset absent")
  f <- read_csv(fad, show_col_types = FALSE)

  expected_issue <- c(AAGL2021_002 = -4.47, AAGL2021_049 = -1.45,
                      AAGL2023_042 = -3.22, AAGL2023_048 = -0.20)
  expected_epub  <- c(AAGL2021_002 = -6.09, AAGL2021_049 = -5.10,
                      AAGL2023_042 = -6.46, AAGL2023_048 = -5.10)

  for (id in names(expected_issue)) {
    row <- f[f$abstract_id == id, , drop = FALSE]
    if (nrow(row) != 1 || is.na(row$months_to_pub)) next
    got <- row$months_to_pub
    expect_lt(
      abs(got - expected_issue[[id]]), 0.05,
      label = sprintf(paste("%s has an interval of %.2f months. The print issue",
                            "date gives %.2f and the electronic date %.2f, so this",
                            "is %s. The publication date is the print issue date",
                            "(PI decision, 2026-09-05)"),
                      id, got, expected_issue[[id]], expected_epub[[id]],
                      if (abs(got - expected_epub[[id]]) < 0.05)
                        "the electronic date" else "neither"))
  }
})
