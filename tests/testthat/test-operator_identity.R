# Operator identity must not be committed.
#
# Two different things live in this repository and only one of them is personal
# data belonging to the people running the study:
#
#   * Author names, affiliations and contact emails inside data/ come from
#     PubMed, OpenAlex, Crossref and Europe PMC. They are published
#     bibliographic metadata, they are the substance of the matching pipeline,
#     and removing them would destroy the study. They are NOT in scope here.
#
#   * The operator's own identity -- home directory, username, personally named
#     external volumes, contact address used for API polite pools -- is not a
#     property of the science at all. It leaked in through hard-coded paths and
#     a default argument. That is what this file guards.
#
# The fix these tests lock in is R/utils_external_paths.R: external inputs are
# resolved from an environment variable or config.yml and default to "", so a
# machine-specific path lives in .Renviron (gitignored) rather than in git.

library(testthat)

repo_root <- here::here()

# Tracked, reviewable files only. data/ is excluded for the reason above;
# renv/ is vendored.
tracked_text_files <- function() {
  out <- suppressWarnings(system2("git", c("-C", shQuote(repo_root), "ls-files"),
                                  stdout = TRUE, stderr = FALSE))
  if (!length(out) || !is.null(attr(out, "status"))) return(character(0))
  keep <- grepl("\\.(R|Rmd|r|yml|yaml|md|cff|json|sh|toml)$", out)
  out <- out[keep]
  out[!grepl("^(data|renv)/", out)]
}

# CITATION.cff and LICENSE name the author on purpose -- that is attribution,
# not leakage, and a citation file without an author is useless.
ATTRIBUTION_ALLOWLIST <- c("CITATION.cff", "LICENSE")

read_safe <- function(p) {
  f <- file.path(repo_root, p)
  if (!file.exists(f)) return(character(0))
  tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
}

test_that("no absolute home or personal volume path is committed", {
  files <- setdiff(tracked_text_files(), ATTRIBUTION_ALLOWLIST)
  skip_if(length(files) == 0, "git ls-files unavailable")

  offenders <- character(0)
  for (p in files) {
    ln <- read_safe(p)
    # /Users/<name>/ and /home/<name>/ expose a username; /Volumes/<Name> and
    # /media/<Name> expose a personally named removable drive.
    hits <- grep("(/Users/[A-Za-z0-9._-]+|/home/[A-Za-z0-9._-]+|/Volumes/[A-Za-z0-9._-]+|/media/[A-Za-z0-9._-]+)",
                 ln, value = TRUE)
    # A path written as a placeholder or an env var reference is fine.
    hits <- hits[!grepl("\\$[A-Z_]+|<[a-z_]+>|\\{[a-z_]+\\}", hits)]
    if (length(hits)) {
      offenders <- c(offenders, sprintf("%s: %s", p, trimws(substr(hits, 1, 110))))
    }
  }

  expect_equal(
    length(offenders), 0,
    label = paste0(
      "absolute machine paths are committed, which publishes the operator's ",
      "username or external volume names and makes the stage non-portable. ",
      "Resolve them with external_path() instead:\n  ",
      paste(head(offenders, 12), collapse = "\n  ")))
})

test_that("config.yml ships no machine-specific external path", {
  cfg <- config::get(file = file.path(repo_root, "config.yml"))
  ext <- cfg$external_data
  skip_if(is.null(ext), "no external_data block")

  for (k in names(ext)) {
    v <- ext[[k]]
    if (is.null(v) || !length(v) || is.na(v[1])) next
    expect_false(
      grepl("^(/Users/|/home/|/Volumes/|/media/)", v[1]),
      label = sprintf(paste("config.yml external_data$%s is an absolute machine path (%s).",
                            "Leave it empty and set the environment variable instead"), k, v[1]))
  }
})

test_that("no personal contact address is hard-coded as a default", {
  files <- setdiff(tracked_text_files(), ATTRIBUTION_ALLOWLIST)
  skip_if(length(files) == 0, "git ls-files unavailable")

  offenders <- character(0)
  for (p in files) {
    ln <- read_safe(p)
    # An email inside a Sys.getenv() default, or assigned to a MAILTO-ish
    # constant, is an operator address baked into the code.
    hits <- grep("(Sys\\.getenv\\([^)]*@[^)]*\\)|(MAILTO|EMAIL|CONTACT)\\s*(<-|=)\\s*\"[^\"]*@)",
                 ln, value = TRUE, perl = TRUE)
    # example.com / example.org / example.net are reserved by RFC 2606 for
    # documentation. A fallback pointing there is deliberately not a real
    # mailbox, which is exactly what a committed default should be.
    hits <- hits[!grepl("@example\\.(com|org|net)", hits)]
    if (length(hits)) offenders <- c(offenders, sprintf("%s: %s", p, trimws(hits)))
  }

  expect_equal(
    length(offenders), 0,
    label = paste0(
      "a contact address is hard-coded as a fallback. It belongs in the ",
      "environment (OPENALEX_MAILTO), not in git:\n  ",
      paste(head(offenders, 8), collapse = "\n  ")))
})

test_that("external inputs resolve to empty rather than a personal default", {
  source(file.path(repo_root, "R", "utils_external_paths.R"))
  withr::with_envvar(c(ABOG_NPI_PATH = "", NPPES_DUCKDB_PATH = ""), {
    expect_identical(external_path("abog_npi_path", "ABOG_NPI_PATH"), "")
    expect_identical(external_path("nppes_duckdb_path", "NPPES_DUCKDB_PATH"), "")
    expect_false(external_available(""))
  })
  # And an env var still wins.
  withr::with_envvar(c(ABOG_NPI_PATH = "/tmp/some_export.csv"), {
    expect_identical(external_path("abog_npi_path", "ABOG_NPI_PATH"), "/tmp/some_export.csv")
  })
})
