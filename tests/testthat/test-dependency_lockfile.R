# The lockfile has to stay true, or it is worse than not having one.
#
# renv.lock records the package versions that produced the committed outputs.
# A lockfile nobody checks drifts silently: someone adds library(newthing), CI
# installs it from its own hand-maintained extra-packages list, the run goes
# green, and the lockfile now describes an environment that never existed.
#
# These tests are the cheap half of reproducibility. They do not prove the
# environment restores -- several dependencies (duckdb, pdftools, rsvg,
# webshot2) need system libraries, so a full renv::restore() in CI would be
# slow and brittle. They prove the lockfile still describes this codebase.

library(testthat)

repo_root  <- here::here()
lock_path  <- file.path(repo_root, "renv.lock")

# Guarded at every use site, so a machine without them still runs the suite.
# Keep in step with OPTIONAL in scripts/build_lockfile.R.
OPTIONAL <- c("shinytest2")

test_that("renv.lock exists and is well-formed", {
  expect_true(file.exists(lock_path))
  lock <- jsonlite::fromJSON(lock_path, simplifyVector = FALSE)

  expect_true(!is.null(lock$R$Version))
  expect_match(lock$R$Version, "^[0-9]+\\.[0-9]+")
  expect_gt(length(lock$Packages), 20)

  for (nm in names(lock$Packages)) {
    rec <- lock$Packages[[nm]]
    expect_identical(rec$Package, nm,
                     label = sprintf("record %s is keyed inconsistently", nm))
    expect_true(nzchar(rec$Version %||% ""),
                label = sprintf("%s has no version", nm))
    expect_true(rec$Source %in% c("Repository", "GitHub"),
                label = sprintf("%s has an unrecognised Source: %s", nm, rec$Source))
  }
})

test_that("every package the code uses is locked", {
  skip_if_not_installed("renv")
  lock <- jsonlite::fromJSON(lock_path, simplifyVector = FALSE)

  base_pkgs <- rownames(installed.packages(priority = "base"))
  used <- sort(setdiff(unique(renv::dependencies(path = repo_root, quiet = TRUE)$Package),
                       base_pkgs))
  locked <- names(lock$Packages)

  unlocked <- setdiff(used, c(locked, OPTIONAL))
  expect_equal(
    length(unlocked), 0,
    label = paste0(
      "these packages are used by the code but absent from renv.lock, so the ",
      "lockfile no longer describes this project. Run ",
      "`Rscript scripts/build_lockfile.R`:\n  ",
      paste(unlocked, collapse = ", ")))
})

test_that("the lockfile does not carry packages the project no longer uses", {
  skip_if_not_installed("renv")
  lock <- jsonlite::fromJSON(lock_path, simplifyVector = FALSE)
  base_pkgs <- rownames(installed.packages(priority = "base"))
  used <- unique(renv::dependencies(path = repo_root, quiet = TRUE)$Package)

  # Transitive dependencies are legitimately absent from source scans, so this
  # only flags packages that were locked directly and then dropped from the
  # code. It is a staleness signal, not a correctness one.
  stale <- setdiff(names(lock$Packages), c(used, base_pkgs))
  expect_equal(
    length(stale), 0,
    label = paste0(
      "renv.lock pins packages nothing references any more. Re-run ",
      "`Rscript scripts/build_lockfile.R`:\n  ",
      paste(stale, collapse = ", ")))
})

test_that("the mysterycall pin agrees across the lockfile and both workflows", {
  lock <- jsonlite::fromJSON(lock_path, simplifyVector = FALSE)
  rec <- lock$Packages$mysterycall
  skip_if(is.null(rec), "mysterycall not locked")

  # It is GitHub-only. Recorded as CRAN it is unrestorable, because it is not
  # there -- which is what packageDescription() reports for a git-archive
  # install, so this is a live trap rather than a theoretical one.
  expect_identical(rec$Source, "GitHub")
  expect_true(nzchar(rec$RemoteSha %||% ""))

  wf <- c(".github/workflows/tests.yaml", ".github/workflows/R-CMD-check.yaml")
  for (f in wf) {
    p <- file.path(repo_root, f)
    if (!file.exists(p)) next
    txt <- readLines(p, warn = FALSE)
    pin <- grep("mysterycall@", txt, value = TRUE)
    expect_gt(length(pin), 0)
    sha <- sub(".*mysterycall@([0-9a-f]+).*", "\\1", pin[1])
    expect_identical(
      sha, rec$RemoteSha,
      label = sprintf(paste("%s pins mysterycall at %s but renv.lock records %s.",
                            "The environment CI builds is not the one the lockfile describes"),
                      f, sha, rec$RemoteSha))
  }
})
