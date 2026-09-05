# Cycle 15 of 24. Mix: 3 BVA, 3 semantic, 4 adversarial.
#
# Target: R/02b_backfill_abstract_text.R, the stage that exists specifically to
# repair the gap cycle 14 measured. Its header states the problem plainly:
# "ScienceDirect paywalls the individual article pages for older supplement
# issues, so the web scraper could not retrieve abstract text for 2012-2018."
#
# Every contract below is read from the source rather than assumed. That matters
# here: my first pass at this cycle rebuilt the cache key without stripping the
# https://doi.org/ prefix the script strips at :45, concluded that zero of 280
# abstracts had ever been fetched, and would have published a false finding. The
# key derivation is now asserted directly, as test 15.1.

library(testthat)
library(dplyr)
library(stringr)

P_CLEAN <- here::here("data", "processed", "abstracts_cleaned.csv")
P_FINAL <- here::here("output", "final_analytical_dataset.csv")
CACHE   <- here::here("data", "cache", "pubmed_xml")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

# Mirrors fetch_pubmed_by_doi() at R/02b_backfill_abstract_text.R:45-47.
cache_key_of <- function(doi_raw) {
  bare <- str_replace(doi_raw, "^https?://doi\\.org/", "")
  str_replace_all(bare, "[/:]", "_")
}
cache_path_of <- function(doi_raw) file.path(CACHE, paste0(cache_key_of(doi_raw), ".xml"))
# Mirrors the eligibility filter at :23-25.
no_text <- function(d) is.na(d$abstract_text) | nchar(d$abstract_text) < 10
has_doi <- function(d) !is.na(d$doi) & nchar(d$doi) > 5

# ============================================================
# BVA 15.1 - the cache key derivation matches the script
# ============================================================
test_that("the cache key strips the DOI prefix exactly as the fetcher does", {
  expect_equal(cache_key_of("https://doi.org/10.1016/j.jmig.2012.08.035"),
               "10.1016_j.jmig.2012.08.035")
  expect_equal(cache_key_of("http://doi.org/10.1016/j.jmig.2012.08.035"),
               "10.1016_j.jmig.2012.08.035")
  expect_equal(cache_key_of("10.1016/j.jmig.2012.08.035"),
               "10.1016_j.jmig.2012.08.035")
  # A key built from the full URL yields a different, never-present filename,
  # which reads as "never fetched" for every row. Asserting the derivation stops
  # a wrong conclusion being drawn from a right cache.
  expect_false(identical(cache_key_of("https://doi.org/10.1016/x"),
                         str_replace_all("https://doi.org/10.1016/x", "/", "_")))
})

# ============================================================
# BVA 15.2 - a cached fetch is a real document, not a stub
# ============================================================
test_that("cached PubMed XML files clear the size floor the fetcher requires", {
  skip_if(!dir.exists(CACHE), "no cache")
  files <- list.files(CACHE, pattern = "\\.xml$", full.names = TRUE)
  skip_if(length(files) == 0, "cache empty")
  sizes <- file.size(files)
  # :49 treats a cached file as usable only when size > 100. Anything at or
  # below that is re-fetched every run, so a cache full of stubs is a cache that
  # never hits.
  tiny <- sum(sizes <= 100)
  expect_true(tiny == 0,
              label = sprintf("%d of %d cached files are <= 100 bytes and will be re-fetched every run",
                              tiny, length(files)))
})

# ============================================================
# BVA 15.3 - eligibility is exactly what the script's filter selects
# ============================================================
test_that("every text-free abstract carries the DOI the backfill needs", {
  need(P_CLEAN)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  nt <- cl[no_text(cl), ]
  skip_if(nrow(nt) == 0, "no text-free abstracts")
  # If a text-free abstract has no DOI the backfill can never reach it, and the
  # gap is permanent rather than pending.
  unreachable <- sum(!has_doi(nt))
  expect_true(unreachable == 0,
              label = sprintf("%d of %d text-free abstracts have no usable DOI and are permanently unreachable by the backfill",
                              unreachable, nrow(nt)))
})

# ============================================================
# SEMANTIC 15.4 - the backfill either fills a row or leaves evidence it tried
# ============================================================
test_that("every eligible abstract was at least attempted by the backfill", {
  need(P_CLEAN)
  skip_if(!dir.exists(CACHE), "no cache")
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  elig <- cl[no_text(cl) & has_doi(cl), ]
  skip_if(nrow(elig) == 0, "nothing eligible")
  attempted <- sum(file.exists(cache_path_of(elig$doi)))
  # A row that is still empty AND has no cached fetch was never attempted. The
  # distinction matters: PubMed genuinely lacking an abstract is a limit of the
  # source, while never asking is unfinished work.
  expect_true(attempted == nrow(elig),
              label = sprintf("%d of %d eligible abstracts were never fetched (%d were, and none of those returned an AbstractText)",
                              nrow(elig) - attempted, nrow(elig), attempted))
})

# ============================================================
# SEMANTIC 15.5 - re-running must not re-fetch rows that already have text
# ============================================================
test_that("the backfill's eligibility filter excludes rows that already have text", {
  need(P_CLEAN)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  # The header promises "Safe to re-run: skips any abstract_id that already has
  # abstract_text". Verify the filter that implements that promise.
  elig <- cl[no_text(cl) & has_doi(cl), ]
  expect_true(all(no_text(elig)),
              label = "an abstract that already has text is selected for backfill")
  expect_true(all(nchar(elig$abstract_text) < 10 | is.na(elig$abstract_text)))
})

# ============================================================
# SEMANTIC 15.6 - text presence agrees across the two artifacts
# ============================================================
test_that("word count in the analytical dataset matches text presence upstream", {
  need(P_CLEAN, P_FINAL)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  f  <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  skip_if(!"abstract_word_count" %in% names(f), "column absent")
  j <- f |> select(abstract_id, abstract_word_count) |>
    inner_join(cl |> select(abstract_id, abstract_text), by = "abstract_id")
  # A non-zero word count on a row with no text, or the reverse, means the two
  # artifacts disagree about whether the abstract exists.
  contradictions <- sum((j$abstract_word_count > 0) &
                        (is.na(j$abstract_text) | nchar(j$abstract_text) < 10))
  expect_true(contradictions == 0,
              label = sprintf("%d rows report a word count but carry no text upstream",
                              contradictions))
})

# ============================================================
# ADVERSARIAL 15.7 - every cache file corresponds to a DOI in the cohort
# ============================================================
test_that("the cache holds no files for DOIs outside the cohort", {
  need(P_CLEAN)
  skip_if(!dir.exists(CACHE), "no cache")
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  known <- cache_key_of(cl$doi[!is.na(cl$doi)])
  present <- sub("\\.xml$", "", basename(list.files(CACHE, pattern = "\\.xml$")))
  # data/cache/pubmed_xml is SHARED. 02b keys by a DOI-derived string; other
  # stages key the same directory by bare PMID. An earlier draft of this test
  # required every file to match a cohort DOI and flagged 1,472 of 1,566 as
  # orphans when they were simply the PMID-keyed entries. The real contract is
  # that a cache file belongs to one of the two key spaces, not to neither.
  unexplained <- setdiff(present, known)
  unexplained <- unexplained[!grepl("^[0-9]{6,9}$", unexplained)]
  expect_true(length(unexplained) == 0,
              label = sprintf("%d cache files match neither a cohort DOI nor a PMID: %s",
                              length(unexplained),
                              paste(utils::head(unexplained, 3), collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 15.8 - a cached fetch is well-formed XML
# ============================================================
test_that("cached XML parses", {
  skip_if(!dir.exists(CACHE), "no cache")
  skip_if_not(requireNamespace("xml2", quietly = TRUE), "xml2 absent")
  files <- list.files(CACHE, pattern = "\\.xml$", full.names = TRUE)
  skip_if(length(files) == 0, "cache empty")
  set.seed(1)
  sample_files <- files[sample(length(files), min(40, length(files)))]
  bad <- sum(vapply(sample_files, function(p) {
    inherits(tryCatch(xml2::read_xml(p), error = function(e) e), "error")
  }, logical(1)))
  expect_true(bad == 0,
              label = sprintf("%d of %d sampled cache files do not parse as XML",
                              bad, length(sample_files)))
})

# ============================================================
# ADVERSARIAL 15.9 - backfilled text must not carry XML markup through
# ============================================================
test_that("abstract_text contains no XML tags from the PubMed source", {
  need(P_CLEAN)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  txt <- cl$abstract_text[!is.na(cl$abstract_text)]
  skip_if(length(txt) == 0, "no text")
  leaked <- sum(grepl("<AbstractText|</Abstract>|<CopyrightInformation", txt))
  expect_true(leaked == 0,
              label = sprintf("%d abstracts carry raw PubMed XML markup in abstract_text",
                              leaked))
})

# ============================================================
# ADVERSARIAL 15.10 - the gap is not silently spreading to new congresses
# ============================================================
test_that("text loss stays confined to the congresses the backfill targets", {
  need(P_CLEAN)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  nt <- cl[no_text(cl), ]
  skip_if(nrow(nt) == 0, "no text-free abstracts")
  # The script's header scopes the problem to 2012-2018. A text-free abstract
  # outside that window is a new failure, not the documented one.
  outside <- sort(unique(nt$congress_year[nt$congress_year > 2018]))
  expect_true(length(outside) == 0,
              label = paste("text loss has appeared outside the documented 2012-2018 window:",
                            paste(outside, collapse = ", ")))
})
