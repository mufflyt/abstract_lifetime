# Cycle 9 of 24. Mix: 3 BVA, 3 semantic, 4 adversarial.
#
# Ground not covered by cycles 1-8 or by the concurrent remediation suite
# (docs drift, model stability, gender tiers, bundle currency, mysterycall
# integrations): character encoding, locale dependence, RNG seeding, timestamp
# ambiguity, and CSV round-trip fidelity. These are the failure modes that
# reproduce differently on another machine rather than failing outright here.
#
# This file is deliberately pure ASCII. Every non-ASCII character it needs is
# written as a \u escape, so the test for mangled encoding cannot itself be
# mangled by whatever moves the file around.

library(testthat)
library(dplyr)

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_CLEAN <- here::here("data", "processed", "abstracts_cleaned.csv")
P_DEC   <- here::here("output", "manual_review_decisions.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

# ============================================================
# BVA 9.1 - the censoring date must sit beyond the last congress
# ============================================================
test_that("the search end date is after every congress it censors against", {
  cfg <- config::get(file = here::here("config.yml"))
  source(here::here("R", "utils_congresses.R"))
  end <- suppressWarnings(as.Date(cfg$pubmed$date_end, "%Y/%m/%d"))
  skip_if(is.na(end), "pubmed$date_end not parseable")
  lkp <- congress_date_lookup(cfg)
  gaps <- as.numeric(difftime(end, lkp, units = "days"))
  expect_true(all(gaps > 0),
              label = paste("a congress falls after the search end date, so its",
                            "abstracts would censor at a negative follow-up time"))
  # The most recent congress binds: under a month of follow-up there makes every
  # survival estimate for that cohort noise.
  expect_gt(min(gaps), 30,
            label = paste("shortest follow-up is", round(min(gaps)), "days"))
})

# ============================================================
# BVA 9.2 - numeric precision survives the CSV round trip
# ============================================================
test_that("scores survive write/read without precision loss", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  skip_if(!"best_score" %in% names(f), "best_score absent")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  keep <- f |> select(abstract_id, best_score, title_sim) |> filter(!is.na(best_score))
  readr::write_csv(keep, tmp)
  back <- readr::read_csv(tmp, show_col_types = FALSE)
  expect_equal(back$best_score, keep$best_score, tolerance = 0,
               label = "best_score changed value through a CSV round trip")
  expect_equal(back$title_sim, keep$title_sim, tolerance = 0)
  expect_true(is.numeric(back$best_score),
              label = "best_score read back as a non-numeric type")
})

# ============================================================
# BVA 9.3 - multi-byte characters survive ingestion
# ============================================================
test_that("non-ASCII text is preserved rather than mangled", {
  need(P_CLEAN)
  d <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  cols <- intersect(c("title", "author_name_first", "author_name_last", "authors"),
                    names(d))
  skip_if(length(cols) == 0, "no text columns")
  blob <- unlist(lapply(cols, function(cc) as.character(d[[cc]])))
  blob <- blob[!is.na(blob)]
  expect_true(all(validUTF8(blob)),
              label = paste(sum(!validUTF8(blob)), "field(s) are not valid UTF-8"))

  # Mojibake signature: UTF-8 bytes decoded as Latin-1 leave a capital A-tilde
  # (U+00C3) or a small a-circumflex (U+00E2) in front of the intended
  # character, and U+FFFD is the replacement character a failed decode inserts.
  moj <- paste0("Ã[©¨¶¼¡]",
                "|â",
                "|�")
  bad <- grep(moj, blob, value = TRUE)
  expect_length(bad, 0L)
  if (length(bad)) fail(paste("mojibake present, e.g.:", substr(bad[1], 1, 80)))
})

# ============================================================
# SEMANTIC 9.4 - the pipeline seed is declared and is a real seed
# ============================================================
test_that("a reproducibility seed is configured and used by the runner", {
  cfg <- config::get(file = here::here("config.yml"))
  seed <- cfg$pipeline$seed
  expect_false(is.null(seed), label = "config declares no pipeline seed")
  expect_true(is.numeric(seed) && seed == floor(seed),
              label = "the seed is not an integer")
  runner <- paste(readLines(here::here("00_run_all.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("set\\.seed\\(", runner),
              label = "config declares a seed but 00_run_all.R never calls set.seed()")
})

# ============================================================
# SEMANTIC 9.5 - review timestamps are unambiguous
# ============================================================
test_that("review timestamps parse to a single instant, not a local guess", {
  need(P_DEC)
  d <- readr::read_csv(P_DEC, show_col_types = FALSE)
  skip_if(!"review_timestamp" %in% names(d), "no timestamps")
  ts <- d$review_timestamp
  expect_equal(sum(is.na(ts)), 0L,
               label = "an unparseable timestamp silently loses its ordering")
  # Adjudication precedence is decided by comparing these. Without a timezone
  # the same file can order differently on another machine, which is the class
  # of defect already fixed once in dedup_decisions_for_analysis().
  expect_true(inherits(ts, "POSIXct"),
              label = paste("review_timestamp read back as", class(ts)[1]))
  tz <- attr(ts, "tzone")
  expect_false(is.null(tz) || identical(tz, ""),
               label = "review_timestamp carries no timezone; ordering is machine-dependent")
})

# ============================================================
# SEMANTIC 9.6 - every candidate row belongs to a cohort abstract
# ============================================================
test_that("candidate rows reference known abstracts, and cohort orphans are excluded videos", {
  need(P_FINAL)
  parsed_p <- here::here("data", "processed", "abstracts_parsed.csv")
  skip_if(!file.exists(parsed_p), "parsed abstracts absent")
  f  <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  pa <- readr::read_csv(parsed_p, show_col_types = FALSE)

  # An earlier draft required every candidate to map into the CLEANED cohort.
  # That premise was wrong: the searches run against the parsed set, so
  # candidates legitimately survive for abstracts the video filter later removes
  # at R/02_clean_abstracts.R:34. Those rows are never consumed. What must hold
  # is that no candidate references an id the pipeline has never seen, and that
  # every cohort orphan is explained by the documented exclusion.
  unknown_total <- 0L
  unexplained   <- character(0)
  for (p in list.files(here::here("data", "processed"),
                       pattern = "candidates[.]csv$", full.names = TRUE)) {
    d <- readr::read_csv(p, show_col_types = FALSE, n_max = 50000)
    if (!"abstract_id" %in% names(d)) next
    ids <- unique(d$abstract_id)
    unknown_total <- unknown_total + length(setdiff(ids, pa$abstract_id))
    orphan <- setdiff(ids, f$abstract_id)
    if (length(orphan)) {
      st <- pa$session_type[match(orphan, pa$abstract_id)]
      if (any(is.na(st) | st != "Video")) {
        unexplained <- c(unexplained,
                         paste0(basename(p), ": ",
                                sum(is.na(st) | st != "Video"), " orphan(s)"))
      }
    }
  }
  expect_equal(unknown_total, 0L,
               label = "a candidate file references an abstract_id absent from the parsed set")
  expect_length(unexplained, 0L)
  if (length(unexplained)) {
    fail(paste("cohort orphans not explained by the video exclusion:",
               paste(unexplained, collapse = "; ")))
  }
})

# ============================================================
# ADVERSARIAL 9.7 - results must not depend on collation locale
# ============================================================
test_that("cohort ordering is stable across collation locales", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", old)), add = TRUE)
  ok_c <- suppressWarnings(Sys.setlocale("LC_COLLATE", "C"))
  skip_if(!nzchar(ok_c), "cannot switch collation locale here")
  order_c <- f$abstract_id[order(f$abstract_id)]
  suppressWarnings(Sys.setlocale("LC_COLLATE", old))
  order_native <- f$abstract_id[order(f$abstract_id)]
  expect_equal(order_c, order_native,
               label = "abstract_id ordering differs between C and native collation")
})

# ============================================================
# ADVERSARIAL 9.8 - no column silently arrives as the wrong type
# ============================================================
test_that("numeric columns are not read back as character", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  should_be_numeric <- intersect(
    c("congress_year", "best_score", "title_sim", "n_candidates", "author_count",
      "months_to_pub", "abstract_word_count"), names(f))
  wrong <- should_be_numeric[!vapply(should_be_numeric,
                                     function(cc) is.numeric(f[[cc]]), logical(1))]
  expect_length(wrong, 0L)
  if (length(wrong)) {
    fail(paste("read back as non-numeric, so comparisons become string comparisons:",
               paste(wrong, collapse = ", ")))
  }
})

# ============================================================
# ADVERSARIAL 9.9 - candidate pools carry no duplicate rows
# ============================================================
test_that("no candidate file lists the same source record twice for one abstract", {
  cand <- list.files(here::here("data", "processed"),
                     pattern = "candidates[.]csv$", full.names = TRUE)
  skip_if(length(cand) == 0, "no candidate files")
  # An earlier draft keyed on (abstract_id, pmid, doi). That is not a key for
  # the OpenAlex and Semantic Scholar pools, where most records carry neither a
  # PMID nor a DOI; it flagged 2,159 "duplicates" in one file that were distinct
  # works with distinct oa_id/s2_id, titles and authors. Key on the source's own
  # identifier where it has one.
  offenders <- character(0)
  for (p in cand) {
    d <- readr::read_csv(p, show_col_types = FALSE, n_max = 50000)
    if (!"abstract_id" %in% names(d)) next
    src_id <- intersect(c("oa_id", "s2_id", "pmid", "doi", "candidate_id"), names(d))
    if (length(src_id) == 0) next
    key <- c("abstract_id", src_id[1])
    sub <- d[, key, drop = FALSE]
    sub <- sub[!is.na(sub[[src_id[1]]]), , drop = FALSE]
    if (nrow(sub) == 0) next
    n_dup <- sum(duplicated(sub))
    # A repeated (abstract, source record) pair is counted twice by anything
    # that aggregates the pool before scoring, including tie detection.
    if (n_dup > 0) {
      offenders <- c(offenders,
                     paste0(basename(p), ": ", n_dup, " repeated ",
                            paste(key, collapse = "/"), " rows"))
    }
  }
  expect_length(offenders, 0L)
  if (length(offenders)) fail(paste(offenders, collapse = "; "))
})

# ============================================================
# ADVERSARIAL 9.10 - no shipped CSV carries a byte-order mark
# ============================================================
test_that("shipped CSVs have no BOM to corrupt their first column name", {
  files <- c(P_FINAL, P_CLEAN,
             list.files(here::here("output"), pattern = "\\.csv$", full.names = TRUE))
  files <- unique(files[file.exists(files)])
  skip_if(length(files) == 0, "no CSVs")
  offenders <- character(0)
  for (p in files) {
    con <- file(p, "rb")
    b <- readBin(con, "raw", 3)
    close(con)
    if (length(b) == 3 && identical(b, as.raw(c(0xEF, 0xBB, 0xBF)))) {
      offenders <- c(offenders, basename(p))
    }
  }
  expect_length(offenders, 0L)
  if (length(offenders)) {
    fail(paste("BOM present, which renames the first column on read in some tools:",
               paste(offenders, collapse = ", ")))
  }
})
