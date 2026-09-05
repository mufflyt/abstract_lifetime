# docs/DECISIONS_PENDING.md is generated from tests/expected_failures.yaml.
#
# A generated file that nothing checks is a file that silently goes stale. The
# failure mode here is specific and bad: someone resolves a decision, removes
# the manifest entry, and the document keeps presenting a settled question as
# open. Or the reverse, and a decision registered last week never reaches the
# page the author actually reads.

library(testthat)

GEN <- here::here("R", "generate_decisions_pending.R")
DOC <- here::here("docs", "DECISIONS_PENDING.md")

test_that("the generator and the committed document exist", {
  expect_true(file.exists(GEN))
  expect_true(file.exists(DOC),
              label = "docs/DECISIONS_PENDING.md is missing; run Rscript R/generate_decisions_pending.R")
})

test_that("the committed document matches what the manifest generates now", {
  skip_if_not(file.exists(GEN) && file.exists(DOC), "inputs absent")
  skip_if_not(requireNamespace("yaml", quietly = TRUE), "yaml absent")
  # Source in a private environment so the script's write-on-run block, which is
  # guarded on globalenv(), does not fire and overwrite the file under test.
  env <- new.env(parent = globalenv())
  sys.source(GEN, envir = env)
  fresh     <- env$build_decisions_markdown()
  committed <- paste0(paste(readLines(DOC, warn = FALSE), collapse = "\n"), "\n")
  expect_identical(committed, fresh,
                   label = paste("docs/DECISIONS_PENDING.md is out of date with",
                                 "tests/expected_failures.yaml; regenerate with",
                                 "Rscript R/generate_decisions_pending.R"))
})

test_that("every open decision reaches the document", {
  skip_if_not(file.exists(DOC), "document absent")
  skip_if_not(requireNamespace("yaml", quietly = TRUE), "yaml absent")
  m <- yaml::read_yaml(here::here("tests", "expected_failures.yaml"))
  txt <- paste(readLines(DOC, warn = FALSE), collapse = "\n")
  missing <- character(0)
  for (e in m$expected_failures) {
    # Compare on the reflowed form: the manifest wraps folded scalars at the
    # source width, the document does not.
    t <- trimws(gsub("\\s+", " ", e$test))
    if (!grepl(t, txt, fixed = TRUE)) missing <- c(missing, t)
  }
  expect_true(length(missing) == 0,
              label = paste("manifest entries absent from the document:",
                            paste(missing, collapse = "; ")))
  # The count in the opening sentence is generated from the manifest; if it
  # disagrees the document is describing a different list than it contains.
  expect_true(grepl(sprintf("keeps %d tests failing", length(m$expected_failures)), txt),
              label = "the stated count does not match the number of manifest entries")
})
