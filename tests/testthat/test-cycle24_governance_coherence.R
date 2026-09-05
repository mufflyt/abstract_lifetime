# Cycle 24 of 24. Mix: 3 BVA, 3 semantic, 4 adversarial.
#
# Target: the governance layer itself.
#
# This repository now carries a lot of machinery whose only job is to stop
# things drifting: a data contract, a CI contract, an estimand baseline and
# drift report, a manuscript claims table, an expected-failure manifest, an
# approved-skip manifest, a bundle manifest, a candidate-pool index and a
# generated decisions document. Every one of them is itself a committed artefact
# that can go stale, and a stale guard is worse than no guard because it reports
# that it checked.
#
# Nothing was watching the watchers. The adversarial weighting is the point of
# the cycle: the failure mode here is not a wrong calculation, it is a file that
# quietly stops describing the thing it names.
#
# Fitting close to the loop, since cycles 15, 20, 21 and 23 all turned out to
# hinge on exactly this: an artefact left behind by a run that regenerated its
# neighbour.

library(testthat)
library(dplyr)

`%||%` <- function(a, b) if (is.null(a)) b else a

R_ROOT   <- here::here()
P_FINAL  <- here::here("output", "final_analytical_dataset.csv")
P_CLAIMS <- here::here("docs", "manuscript_claims.csv")
P_DC     <- here::here("config", "data_contract.yml")
P_CI     <- here::here("config", "ci_contract.yml")
P_EB     <- here::here("docs", "estimand_baseline.yml")
P_EC     <- here::here("output", "estimand_current.yml")
P_FAIL   <- here::here("tests", "expected_failures.yaml")
P_SKIP   <- here::here("tests", "expected_skips.yaml")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

# ============================================================
# BVA 24.1 - every governance artefact exists and carries content
# ============================================================
test_that("no governance artefact is missing or empty", {
  arts <- c(P_DC, P_CI, P_EB, P_EC, P_CLAIMS, P_FAIL, P_SKIP,
            here::here("shiny", "adjudication_app", "bundle_manifest.csv"),
            here::here("output", "candidate_pool_index.csv"),
            here::here("docs", "DECISIONS_PENDING.md"))
  missing <- arts[!file.exists(arts)]
  expect_true(length(missing) == 0,
              label = paste("governance artefact(s) absent:",
                            paste(basename(missing), collapse = ", ")))
  present <- arts[file.exists(arts)]
  empty <- present[file.size(present) < 10]
  # A guard file that exists but holds nothing passes every "does it exist"
  # check while enforcing nothing.
  expect_true(length(empty) == 0,
              label = paste("governance artefact(s) effectively empty:",
                            paste(basename(empty), collapse = ", ")))
})

# ============================================================
# BVA 24.2 - both manifests stay inside their declared ceilings
# ============================================================
test_that("the manifests obey the ceilings the contract sets for them", {
  need(P_CI, P_FAIL)
  ct <- yaml::read_yaml(P_CI)
  fails <- yaml::read_yaml(P_FAIL)$expected_failures
  cap <- ct$manifest$max_entries
  skip_if(is.null(cap), "no ceiling declared")
  expect_lte(length(fails), cap,
             label = sprintf("%d expected-failure entries against a ceiling of %d",
                             length(fails), cap))
  # The ceiling is a ratchet. Being AT it is not a failure, but it is the point
  # at which the next finding forces either a decision or a deliberate raise.
  if (length(fails) >= cap) {
    warning(sprintf("the expected-failure manifest is at its ceiling (%d of %d)",
                    length(fails), cap))
  }
  succeed("ceiling checked")
})

# ============================================================
# BVA 24.3 - the drift report is numeric and finite where it claims a number
# ============================================================
test_that("the estimand drift report holds usable values", {
  p <- here::here("output", "estimand_drift.csv")
  need(p)
  d <- readr::read_csv(p, show_col_types = FALSE)
  expect_gt(nrow(d), 0)
  num <- d[vapply(d, is.numeric, logical(1))]
  if (ncol(num) > 0) {
    bad <- vapply(num, function(x) any(is.infinite(x)), logical(1))
    expect_true(!any(bad),
                label = paste("infinite value(s) in drift columns:",
                              paste(names(num)[bad], collapse = ", ")))
  } else {
    succeed("no numeric drift columns")
  }
})

# ============================================================
# SEMANTIC 24.4 - the two manifests cannot both claim the same test
# ============================================================
test_that("no test is both expected to fail and approved to skip", {
  need(P_FAIL, P_SKIP)
  f <- yaml::read_yaml(P_FAIL)$expected_failures
  s <- yaml::read_yaml(P_SKIP)$expected_skips
  key <- function(e) paste(e$file, e$test, sep = " :: ")
  fk <- vapply(f, key, character(1))
  sk <- vapply(s, key, character(1))
  both <- intersect(fk, sk)
  # A test registered in both manifests is unfalsifiable: it is excused whether
  # it fails or does not run, so nothing it asserts can ever be enforced.
  expect_true(length(both) == 0,
              label = paste("test(s) registered as BOTH an expected failure and",
                            "an approved skip, so neither manifest can enforce",
                            "anything about them:", paste(both, collapse = "; ")))
})

# ============================================================
# SEMANTIC 24.5 - the data contract describes columns that exist
# ============================================================
test_that("every column the data contract governs exists in the dataset it names", {
  need(P_DC)
  dc <- yaml::read_yaml(P_DC)
  skip_if(is.null(dc$datasets), "the contract declares no datasets")

  problems <- character(0)
  checked <- 0L
  for (ds in dc$datasets) {
    p <- file.path(R_ROOT, ds$path)
    if (!file.exists(p)) {
      problems <- c(problems, sprintf("%s: the dataset does not exist", ds$path))
      next
    }
    hdr <- names(readr::read_csv(p, show_col_types = FALSE, n_max = 0))
    govern <- names(ds$columns %||% list())
    # A rule naming a column that is not there is not enforced on anything, but
    # the contract still reports as satisfied. That is the failure mode this
    # whole cycle is about: a guard that quietly stops describing its subject.
    missing <- setdiff(govern, hdr)
    if (length(missing)) {
      problems <- c(problems, sprintf("%s: rules for absent column(s) %s",
                                      ds$path, paste(missing, collapse = ", ")))
    }
    # And the key must be a real column, or nothing can be keyed on it.
    if (!is.null(ds$key) && !all(ds$key %in% hdr)) {
      problems <- c(problems, sprintf("%s: key %s is not a column",
                                      ds$path, paste(ds$key, collapse = ", ")))
    }
    checked <- checked + length(govern)
  }
  expect_gt(checked, 0)
  expect_true(length(problems) == 0,
              label = paste("data contract problems:", paste(problems, collapse = "; ")))
})

# ============================================================
# SEMANTIC 24.6 - manuscript claims point at sources that exist
# ============================================================
test_that("every manuscript claim names a source file that is present", {
  need(P_CLAIMS)
  cl <- readr::read_csv(P_CLAIMS, show_col_types = FALSE)
  skip_if(!"source_file" %in% names(cl), "no source_file column")
  srcs <- unique(cl$source_file[!is.na(cl$source_file) & nzchar(cl$source_file)])
  missing <- srcs[!file.exists(file.path(R_ROOT, srcs))]
  # A claim whose source has been renamed or deleted can never be re-verified,
  # so the claims table would report on a number nobody can trace.
  expect_true(length(missing) == 0,
              label = paste("manuscript claims naming absent source file(s):",
                            paste(missing, collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 24.7 - the governance files agree about the cohort size
# ============================================================
test_that("the estimand snapshot's cohort size is the cohort actually shipped", {
  need(P_FINAL, P_EC)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  ec <- yaml::read_yaml(P_EC)

  # My first version grepped every cohort-sized number in the snapshot and
  # complained about 1051. That is not a disagreement: 1,051 is the
  # complete-case modelling sample (Table 1 reports Overall 1051), a different
  # and legitimate quantity from the 1,106-abstract cohort. A guard that cannot
  # tell two quantities apart manufactures conflict, which is worse than
  # silence because it trains people to ignore it.
  #
  # The precise contract instead: wherever the snapshot names the cohort
  # denominator, it must be the number of rows shipped. The denominator has
  # been contested here before (1,067 vs 1,106 vs 1,154), so a guard file
  # quietly carrying a different one would reintroduce that ambiguity while
  # appearing to settle it.
  flat <- unlist(ec)
  get1 <- function(pat) {
    v <- flat[grepl(pat, names(flat), ignore.case = TRUE)]
    if (!length(v)) return(NA_real_)
    suppressWarnings(as.numeric(v[[1]]))
  }
  n_eval <- get1("denominator\\.n_evaluated")
  n_unres <- get1("denominator\\.n_unresolved")
  skip_if(is.na(n_eval) || is.na(n_unres), "the snapshot does not record both counts")

  # The estimand denominator is 1,051, not 1,106, and that is deliberate: the
  # rule recorded in the snapshot is "abstracts whose match status was
  # resolved", excluding the 55 unresolved. My earlier versions of this test
  # treated 1,051 as a disagreement twice over, first by grepping every
  # cohort-sized number and then by assuming the denominator must equal the row
  # count. Both manufactured a conflict that does not exist.
  #
  # The invariant that does exist ties all three together: the evaluated and
  # unresolved counts must partition the shipped cohort exactly. That is the
  # arithmetic the whole denominator question turned on, and it is checkable.
  expect_equal(n_eval + n_unres, nrow(f),
               label = sprintf(paste("the estimand snapshot says %g evaluated plus",
                                     "%g unresolved, which is %g, against %d rows",
                                     "shipped. The denominator chain no longer",
                                     "closes."),
                               n_eval, n_unres, n_eval + n_unres, nrow(f)))
})

# ============================================================
# ADVERSARIAL 24.8 - the estimand baseline and current file share a shape
# ============================================================
test_that("the estimand baseline and the current snapshot are comparable", {
  need(P_EB, P_EC)
  b <- yaml::read_yaml(P_EB); c_ <- yaml::read_yaml(P_EC)
  # A drift report is meaningless if the two documents no longer describe the
  # same fields: sections present in one and not the other are silently not
  # compared, which is drift that hides drift.
  only_b <- setdiff(names(b), names(c_))
  only_c <- setdiff(names(c_), names(b))
  expect_true(length(only_b) == 0 && length(only_c) == 0,
              label = paste("estimand baseline and current disagree about sections;",
                            "baseline only:", paste(only_b, collapse = ", "),
                            "| current only:", paste(only_c, collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 24.9 - no two test files define the same test name
# ============================================================
test_that("test names are unique across the suite", {
  dir <- here::here("tests", "testthat")
  fs <- list.files(dir, pattern = "^test-.*\\.R$", full.names = TRUE)
  skip_if(length(fs) == 0, "no test files")
  rows <- do.call(rbind, lapply(fs, function(p) {
    ln <- readLines(p, warn = FALSE)
    m <- regmatches(ln, regexpr('test_that\\("[^"]+"', ln))
    if (!length(m)) return(NULL)
    data.frame(file = basename(p),
               test = gsub('^test_that\\("|"$', "", m),
               stringsAsFactors = FALSE)
  }))
  skip_if(is.null(rows), "no test names parsed")
  # Both manifests key on "file :: test". A duplicated NAME inside one file
  # would make its key ambiguous, so an entry could excuse a different
  # assertion than the one it was written for.
  dup_in_file <- rows |> count(file, test, name = "k") |> filter(k > 1)
  expect_equal(nrow(dup_in_file), 0L,
               label = paste("duplicate test name(s) within a single file, which",
                             "makes the manifest key ambiguous:",
                             paste(sprintf("%s :: %s", dup_in_file$file, dup_in_file$test),
                                   collapse = "; ")))
})

# ============================================================
# ADVERSARIAL 24.10 - every manifest entry names a real test
# ============================================================
test_that("both manifests reference tests that exist in the suite", {
  need(P_FAIL, P_SKIP)
  dir <- here::here("tests", "testthat")
  present <- do.call(rbind, lapply(
    list.files(dir, pattern = "^test-.*\\.R$", full.names = TRUE), function(p) {
      ln <- readLines(p, warn = FALSE)
      m <- regmatches(ln, regexpr('test_that\\("[^"]+"', ln))
      if (!length(m)) return(NULL)
      data.frame(file = basename(p), test = gsub('^test_that\\("|"$', "", m),
                 stringsAsFactors = FALSE)
    }))
  skip_if(is.null(present), "no tests parsed")
  keys <- paste(present$file, present$test, sep = " :: ")

  problems <- character(0)
  for (nm in c("expected_failures", "expected_skips")) {
    p <- if (nm == "expected_failures") P_FAIL else P_SKIP
    es <- yaml::read_yaml(p)[[nm]]
    if (is.null(es)) next
    k <- vapply(es, function(e) paste(e$file, e$test, sep = " :: "), character(1))
    # The gate already fails on an orphaned expected-failure entry, but the skip
    # manifest had no such rule: a renamed test would leave its skip approved
    # forever, silently excusing a test that no longer exists.
    orph <- setdiff(k, keys)
    # A skip entry may legitimately name testthat's own pseudo-test for
    # top-level code, which is not written as test_that().
    orph <- orph[!grepl("code run outside of", orph, fixed = TRUE)]
    if (length(orph)) {
      problems <- c(problems, sprintf("%s: %s", nm, paste(orph, collapse = "; ")))
    }
  }
  expect_true(length(problems) == 0,
              label = paste("manifest entries naming tests that do not exist:",
                            paste(problems, collapse = " | ")))
})
