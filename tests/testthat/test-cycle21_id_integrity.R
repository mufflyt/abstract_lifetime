# Cycle 21 of 24. Mix: 3 BVA, 3 semantic, 4 adversarial.
#
# Target: abstract_id itself, across every committed artefact that carries it.
#
# abstract_id is the join key for the entire study. Thirty-six tracked files use
# it, and every denominator, every enrichment merge and every reviewer decision
# is keyed on it. Individual joins have been tested (cycle 16 parsed-to-cleaned,
# F2 for candidates, cycle 12 for covariates); nobody had asked whether the KEY
# is well-formed and consistent everywhere at once. A key that silently changes
# shape between stages does not error, it just fails to join, and a failed join
# looks like missing data.
#
# The adversarial weighting reflects what actually goes wrong with keys:
# duplicates, whitespace, case, a stage that reorders rows, and an artefact left
# behind from an older cohort.
#
# Format read from the data rather than assumed: AAGL<year>_<nnn>.

library(testthat)
library(dplyr)

ID_RE <- "^AAGL[0-9]{4}_[0-9]{3}$"
P_PARSED <- here::here("data", "processed", "abstracts_parsed.csv")
P_CLEAN  <- here::here("data", "processed", "abstracts_cleaned.csv")
P_FINAL  <- here::here("output", "final_analytical_dataset.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

# Every tracked csv whose header names abstract_id. Discovered, not listed, so a
# new artefact is covered the day it lands.
id_files <- local({
  roots <- c(here::here("data", "processed"), here::here("output"))
  fs <- unlist(lapply(roots, function(r)
    if (dir.exists(r)) list.files(r, pattern = "\\.csv$", full.names = TRUE) else character(0)))
  keep <- vapply(fs, function(p) {
    h <- tryCatch(readLines(p, n = 1L, warn = FALSE), error = function(e) "")
    length(h) == 1 && grepl("(^|,)\"?abstract_id\"?(,|$)", h)
  }, logical(1))
  sort(fs[keep])
})

read_ids <- function(p) {
  d <- tryCatch(readr::read_csv(p, show_col_types = FALSE, col_select = "abstract_id",
                                col_types = readr::cols(.default = readr::col_character())),
                error = function(e) NULL)
  if (is.null(d)) return(character(0))
  x <- d$abstract_id
  x[!is.na(x)]
}

# ============================================================
# BVA 21.1 - the cohort chain narrows and never widens
# ============================================================
test_that("the parsed, cleaned and analytical cohorts nest in that order", {
  need(P_PARSED, P_CLEAN, P_FINAL)
  p <- read_ids(P_PARSED); c_ <- read_ids(P_CLEAN); f <- read_ids(P_FINAL)
  # The denominator chain in technical appendix A13: parsed 1,154 minus 48 video
  # gives 1,106. Each stage must be a subset of the one before it. A stage that
  # ADDS an id is inventing an abstract.
  expect_true(all(c_ %in% p),
              label = sprintf("%d cleaned abstracts are not in the parsed set",
                              sum(!c_ %in% p)))
  expect_true(all(f %in% c_),
              label = sprintf("%d analysed abstracts are not in the cleaned cohort",
                              sum(!f %in% c_)))
  expect_lte(length(f), length(c_))
  expect_lte(length(c_), length(p))
})

# ============================================================
# BVA 21.2 - the key is well-formed everywhere
# ============================================================
test_that("every abstract_id in every artefact matches the documented format", {
  skip_if(length(id_files) == 0, "no artefacts found")
  offenders <- character(0)
  for (p in id_files) {
    ids <- read_ids(p)
    if (!length(ids)) next
    bad <- unique(ids[!grepl(ID_RE, ids)])
    if (length(bad)) {
      offenders <- c(offenders, sprintf("%s: %s", basename(p),
                                        paste(utils::head(bad, 3), collapse = ", ")))
    }
  }
  expect_true(length(offenders) == 0,
              label = paste("malformed abstract_id(s):",
                            paste(offenders, collapse = "; ")))
})

# ============================================================
# BVA 21.3 - the embedded year is a real congress year
# ============================================================
test_that("the year embedded in each id is a congress that exists", {
  need(P_PARSED)
  ids <- read_ids(P_PARSED)
  skip_if(!length(ids), "no ids")
  yrs <- as.integer(substr(ids, 5, 8))
  expect_true(all(yrs >= 2012 & yrs <= 2023),
              label = sprintf("id year(s) outside the 2012-2023 congress range: %s",
                              paste(unique(yrs[yrs < 2012 | yrs > 2023]), collapse = ", ")))
  # And the id's year must agree with the row's own congress_year, or the key
  # says one thing while the data says another.
  d <- readr::read_csv(P_PARSED, show_col_types = FALSE)
  if ("congress_year" %in% names(d)) {
    mism <- sum(as.integer(substr(d$abstract_id, 5, 8)) != as.integer(d$congress_year),
                na.rm = TRUE)
    expect_equal(mism, 0L,
                 label = sprintf("%d rows whose id year disagrees with congress_year", mism))
  }
})

# ============================================================
# SEMANTIC 21.4 - one row per abstract where that is the contract
# ============================================================
test_that("the per-abstract artefacts hold exactly one row per abstract", {
  one_per <- c("abstracts_parsed.csv", "abstracts_cleaned.csv",
               "final_analytical_dataset.csv", "match_scores.csv",
               "abstracts_with_matches.csv", "author_characteristics.csv")
  offenders <- character(0)
  for (p in id_files) {
    if (!basename(p) %in% one_per) next
    ids <- read_ids(p)
    if (!length(ids)) next
    dup <- sum(duplicated(ids))
    # A duplicated key in a one-row-per-abstract file double-weights that
    # abstract in every count computed from it, and silently multiplies rows on
    # any join. This is the defect class that produced the duplicate-abstract_id
    # guard in assign_final_published().
    if (dup > 0) offenders <- c(offenders, sprintf("%s (%d duplicates)", basename(p), dup))
  }
  expect_true(length(offenders) == 0,
              label = paste("duplicated abstract_id in per-abstract artefact(s):",
                            paste(offenders, collapse = ", ")))
})

# ============================================================
# SEMANTIC 21.5 - enrichment never introduces an unknown abstract
# ============================================================
test_that("no artefact references an abstract the parse never produced", {
  need(P_PARSED)
  known <- read_ids(P_PARSED)
  skip_if(!length(known), "no parsed ids")
  offenders <- character(0)
  for (p in id_files) {
    if (identical(normalizePath(p), normalizePath(P_PARSED))) next
    ids <- unique(read_ids(p))
    if (!length(ids)) next
    orph <- setdiff(ids, known)
    # An id in a downstream artefact that no parse produced means either a
    # stale file from an older cohort or a key rewritten mid-pipeline. Either
    # way its rows will never join and will read as missing data.
    if (length(orph)) {
      offenders <- c(offenders, sprintf("%s (%d, e.g. %s)", basename(p), length(orph),
                                        paste(utils::head(orph, 2), collapse = ", ")))
    }
  }
  expect_true(length(offenders) == 0,
              label = paste("artefact(s) referencing unknown abstracts:",
                            paste(offenders, collapse = "; ")))
})

# ============================================================
# SEMANTIC 21.6 - the excluded set is disjoint from the analysed set
# ============================================================
test_that("a pre-congress publication is not counted as a publication", {
  p_ex <- here::here("output", "excluded_pre_congress_publications.csv")
  need(p_ex, P_FINAL)
  ex <- readr::read_csv(p_ex, show_col_types = FALSE)
  f  <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  skip_if(nrow(ex) == 0, "no exclusions recorded")

  # These abstracts REMAIN in the cohort: the denominator is 1,106, not 1,067.
  # My first version of this test asserted they were absent from the analytical
  # dataset, which was the wrong contract. What the exclusion means is that
  # their pre-congress publication does not count as the abstract having led to
  # a publication.
  #
  # The rule is applied to 35 of the 39. For the other four, best_pmid is
  # exactly the PMID listed as excluded and months_to_pub is negative, so they
  # sit in the numerator credited with a paper that appeared before the congress
  # at which the abstract was presented.
  j <- f |> filter(abstract_id %in% ex$abstract_id)
  expect_equal(nrow(j), nrow(ex),
               label = "an excluded abstract is missing from the cohort entirely")

  counted <- j |> filter(final_published %in% c(TRUE, "TRUE"))
  expect_equal(nrow(counted), 0L,
               label = paste0(
                 nrow(counted), " of ", nrow(ex), " abstracts with a pre-congress ",
                 "publication are still counted as published: ",
                 paste(sprintf("%s (%.1f months before its congress)",
                               counted$abstract_id, abs(counted$months_to_pub)),
                       collapse = "; "),
                 ". They are in the numerator of ",
                 sum(f$final_published %in% c(TRUE, "TRUE")),
                 ", so the headline publication rate depends on whether the ",
                 "study's own exclusion rule applies to all 39 or to 35."))
})

# ============================================================
# ADVERSARIAL 21.7 - keys carry no invisible whitespace or case variation
# ============================================================
test_that("no abstract_id differs from another only by case or whitespace", {
  skip_if(length(id_files) == 0, "no artefacts found")
  offenders <- character(0)
  for (p in id_files) {
    ids <- unique(read_ids(p))
    if (!length(ids)) next
    if (any(ids != trimws(ids))) {
      offenders <- c(offenders, sprintf("%s (padded)", basename(p)))
      next
    }
    # Two ids equal after case folding but not before would join under one
    # comparison and not another, which is the worst kind of key defect
    # because it is invisible in a printed table.
    if (any(duplicated(toupper(ids))) && !any(duplicated(ids))) {
      offenders <- c(offenders, sprintf("%s (case-variant duplicates)", basename(p)))
    }
  }
  expect_true(length(offenders) == 0,
              label = paste("abstract_id whitespace or case problems in:",
                            paste(offenders, collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 21.8 - results do not depend on row order
# ============================================================
test_that("the cohort is a set, not a sequence", {
  need(P_CLEAN, P_FINAL)
  c_ <- read_ids(P_CLEAN); f <- read_ids(P_FINAL)
  # Nothing downstream may depend on the order rows happen to sit in. Sorting
  # both and comparing sets catches a stage that silently relies on position,
  # which would reorder under a different locale or a re-run.
  expect_setequal(f, intersect(c_, f))
  expect_equal(sort(unique(f)), sort(f)[!duplicated(sort(f))])
})

# ============================================================
# ADVERSARIAL 21.9 - the two parse artefacts have not diverged
# ============================================================
test_that("abstracts_parsed and its web twin describe the same abstracts", {
  p_web <- here::here("data", "processed", "abstracts_parsed_web.csv")
  need(P_PARSED, p_web)
  a <- read_ids(P_PARSED); b <- read_ids(p_web)
  # 01c_compare_sources.R exists to QA a PDF parse against the web parse, but
  # no PDF artefact and no source_comparison.csv are present, so that check has
  # never run on this data. The one comparison still available is that the two
  # committed parse artefacts agree about the cohort.
  expect_setequal(a, b)
})

# ============================================================
# ADVERSARIAL 21.10 - per-abstract child tables stay inside the cohort
# ============================================================
test_that("child tables never cover more abstracts than the cohort they describe", {
  need(P_PARSED)
  known <- unique(read_ids(P_PARSED))
  skip_if(!length(known), "no parsed ids")
  offenders <- character(0)
  for (p in id_files) {
    # The search artefacts predate the parse correction and cover 1,742
    # abstracts. That is already reported once, by 21.5, which names the file
    # and the count. Reporting it again here would be two failures for one
    # defect and would hide any OTHER artefact that overruns the cohort.
    if (grepl("strategy_results|candidate", basename(p))) next
    ids <- unique(read_ids(p))
    if (!length(ids)) next
    # A child table (authors, affiliations, candidates) legitimately has many
    # rows per abstract, but it can never span more DISTINCT abstracts than
    # were ever parsed.
    if (length(ids) > length(known)) {
      offenders <- c(offenders, sprintf("%s covers %d abstracts against %d parsed",
                                        basename(p), length(ids), length(known)))
    }
  }
  expect_true(length(offenders) == 0,
              label = paste(offenders, collapse = "; "))
})
