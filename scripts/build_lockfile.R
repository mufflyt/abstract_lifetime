#!/usr/bin/env Rscript
# build_lockfile.R — regenerate renv.lock from what this project actually uses.
#
# This repository is a research compendium, not a package: there is no
# DESCRIPTION, and CI installs dependencies from a hand-maintained
# `extra-packages:` list in each workflow. Nothing recorded which *versions*
# produced the committed outputs, so "reproducible" meant "whatever CRAN serves
# today". That is the gap this closes.
#
# renv is deliberately NOT activated for the project. Activating it would add
# renv/activate.R and rewrite .Rprofile, changing how every script and both
# workflows resolve libraries -- a large behavioural change to land alongside a
# lockfile. The lockfile here is a *record* in renv's own format, so
# `renv::restore()` consumes it directly when someone wants to rebuild the
# environment, without imposing renv on everyone who just wants to run a script.
#
# Usage: Rscript scripts/build_lockfile.R

suppressPackageStartupMessages({library(renv)})

# Base and recommended packages ship with R; renv does not lock them.
BASE <- rownames(installed.packages(priority = "base"))

# Keep in step with the pin in .github/workflows/*.yaml. Checked by
# tests/testthat/test-dependency_lockfile.R.
MYSTERYCALL_SHA <- "42d66d92ef52a0f85d1f7c61208c2ddd79d9c06e"

# Runtime-only dependencies: never named in the source, but required for the
# code to work. dbplyr is the case here -- R/09j runs dplyr verbs against a
# DuckDB connection, and dplyr dispatches to dbplyr to build the SQL. Nothing
# ever writes `library(dbplyr)`, so a plain source scan misses it; newer renv
# infers it and older renv does not, which is exactly the kind of environment
# difference a lockfile exists to remove. Locked explicitly so both agree, and
# excluded from the "no longer used" check because no scan will ever see it.
RUNTIME_ONLY <- c("dbplyr")

deps <- renv::dependencies(path = ".", quiet = TRUE)
pkgs <- sort(setdiff(union(unique(deps$Package), RUNTIME_ONLY), BASE))

# Packages that are referenced but optional: every use site guards them, so a
# machine without them still runs the suite. They are not locked, and
# tests/testthat/test-dependency_lockfile.R allows exactly this set.
OPTIONAL <- c("shinytest2")

record_for <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) return(NULL)
  d <- packageDescription(p)
  ver <- as.character(utils::packageVersion(p))

  # mysterycall is GitHub-only and is installed here from a git archive, which
  # strips the Remote* fields packageDescription would otherwise carry. Without
  # this it records as CRAN, where it does not exist, and the lockfile would be
  # unrestorable. The SHA is the one both workflows pin.
  if (identical(p, "mysterycall")) {
    return(list(
      Package = p, Version = ver, Source = "GitHub",
      RemoteType = "github", RemoteHost = "api.github.com",
      RemoteUsername = "mufflyt", RemoteRepo = "mysterycall",
      RemoteSha = MYSTERYCALL_SHA))
  }

  # A GitHub install carries Remote* fields; everything else is treated as CRAN.
  if (identical(d$RemoteType, "github") ||
      (!is.null(d$RemoteHost) && grepl("github", d$RemoteHost, fixed = TRUE))) {
    return(list(
      Package        = p,
      Version        = ver,
      Source         = "GitHub",
      RemoteType     = "github",
      RemoteHost     = "api.github.com",
      RemoteUsername = d$RemoteUsername %||% "",
      RemoteRepo     = d$RemoteRepo %||% p,
      RemoteSha      = if (identical(p, "mysterycall")) MYSTERYCALL_SHA else (d$RemoteSha %||% "")
    ))
  }
  list(Package = p, Version = ver, Source = "Repository", Repository = "CRAN")
}
`%||%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a[1]) || !nzchar(a[1])) b else a

records <- Filter(Negate(is.null), lapply(pkgs, record_for))
names(records) <- vapply(records, `[[`, character(1), "Package")

missing <- setdiff(pkgs, names(records))
unexpected <- setdiff(missing, OPTIONAL)
if (length(unexpected)) {
  stop("dependencies used by this project are not installed here, so the ",
       "lockfile would silently omit them: ", paste(unexpected, collapse = ", "),
       ". Install them and re-run, or add them to OPTIONAL with a reason.")
}
if (length(missing)) {
  message("optional, not locked: ", paste(missing, collapse = ", "))
}

lock <- list(
  R = list(
    Version = paste(R.version$major, R.version$minor, sep = "."),
    Repositories = list(list(Name = "CRAN", URL = "https://cloud.r-project.org"))
  ),
  Packages = records
)

json <- jsonlite::toJSON(lock, auto_unbox = TRUE, pretty = TRUE, null = "null")
writeLines(json, "renv.lock")

cat("renv.lock written:", length(records), "packages,",
    "R", lock$R$Version, "\n")
if (length(missing)) cat("optional, not locked:", paste(missing, collapse = ", "), "\n")
