# The list of registered decisions must exist in exactly one place.
#
# It briefly existed in three: tests/expected_failures.yaml (the source),
# docs/DECISIONS_PENDING.md (generated from it), and hand-maintained tables in
# README.md and docs/VALIDATION.md. The two hand-maintained copies drifted to
# three rows while the manifest grew to twenty-three, so both were telling
# readers that three decisions were open when twenty-three were.
#
# That is precisely the failure the manifest gate exists to prevent, reappearing
# one level up: a description of the truth, kept beside the truth, updated
# separately, and wrong. The tables are gone. What remains in prose is a count,
# and these tests fail when a count stops matching its manifest or when a
# duplicate table reappears.

library(testthat)

repo_root <- here::here()

read_text <- function(rel) {
  p <- file.path(repo_root, rel)
  if (!file.exists(p)) return(NA_character_)
  paste(readLines(p, warn = FALSE), collapse = "\n")
}

# Counts are written as <!--name-->N<!--/name--> so prose can carry a number
# that something else is responsible for checking.
marked_count <- function(txt, name) {
  m <- regmatches(txt, gregexpr(
    sprintf("<!--%s-->\\s*([0-9]+)\\s*<!--/%s-->", name, name), txt))[[1]]
  if (!length(m)) return(integer(0))
  as.integer(sub(sprintf(".*<!--%s-->\\s*([0-9]+)\\s*<!--/%s-->.*", name, name),
                 "\\1", m))
}

DOCS <- c("README.md", "docs/VALIDATION.md")

test_that("every stated manifest count matches tests/expected_failures.yaml", {
  n <- length(yaml::read_yaml(file.path(repo_root, "tests", "expected_failures.yaml"))$expected_failures)
  found_any <- FALSE
  for (d in DOCS) {
    txt <- read_text(d)
    if (is.na(txt)) next
    for (stated in marked_count(txt, "manifest-count")) {
      found_any <- TRUE
      expect_equal(
        stated, n,
        label = sprintf(paste("%s states %d registered failures but",
                              "tests/expected_failures.yaml holds %d"), d, stated, n))
    }
  }
  expect_true(found_any,
              label = "no document states the manifest count; the markers were removed")
})

test_that("every stated skip count matches tests/expected_skips.yaml", {
  p <- file.path(repo_root, "tests", "expected_skips.yaml")
  skip_if_not(file.exists(p), "no skip manifest")
  n <- length(yaml::read_yaml(p)$expected_skips)
  for (d in DOCS) {
    txt <- read_text(d)
    if (is.na(txt)) next
    for (stated in marked_count(txt, "skip-count")) {
      expect_equal(
        stated, n,
        label = sprintf("%s states %d approved skips but the manifest holds %d",
                        d, stated, n))
    }
  }
})

test_that("no document reintroduces a hand-maintained table of manifest entries", {
  # A row naming a specific test file and line is the shape the drifted tables
  # took. Prose may reference a test by name; a table of them is the thing that
  # rots, because it has to be edited every time the manifest changes.
  for (d in DOCS) {
    txt <- read_text(d)
    if (is.na(txt)) next
    # A manifest row cites a specific LINE (`test-foo.R:179`). The test
    # inventory in VALIDATION.md legitimately tabulates files without line
    # numbers, and that table is not a duplicate of the manifest.
    rows <- grep("^\\|\\s*`test-[A-Za-z0-9_]+\\.R:[0-9]+`\\s*\\|",
                 strsplit(txt, "\n")[[1]], value = TRUE)
    expect_equal(
      length(rows), 0,
      label = paste0(
        d, " lists manifest entries in a table again. That table drifted to ",
        "three rows once while the manifest held twenty-three. Link to ",
        "docs/DECISIONS_PENDING.md instead:\n  ",
        paste(head(rows, 5), collapse = "\n  ")))
  }
})

test_that("the generated registry is current with the manifest", {
  doc <- read_text("docs/DECISIONS_PENDING.md")
  skip_if(is.na(doc), "docs/DECISIONS_PENDING.md absent")
  n <- length(yaml::read_yaml(file.path(repo_root, "tests", "expected_failures.yaml"))$expected_failures)

  # One "### " heading per registered decision.
  headings <- grep("^### ", strsplit(doc, "\n")[[1]], value = TRUE)
  expect_equal(
    length(headings), n,
    label = paste("docs/DECISIONS_PENDING.md holds", length(headings),
                  "entries against", n, "in the manifest; regenerate with",
                  "Rscript R/generate_decisions_pending.R"))

  expect_match(doc, "GENERATED FILE", fixed = TRUE,
               label = "the registry lost its do-not-edit banner")
})
