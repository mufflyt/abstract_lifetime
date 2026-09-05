# test-ci_contract.R — the workflows must implement the contract they promise.
#
# config/ci_contract.yml declares what CI verifies. Without this test the
# contract is decorative: a gate could be deleted from a workflow while the file
# continued to promise it, and the only symptom would be a class of defect
# silently going unchecked.
#
# The failure mode this is written against already happened in this repository.
# The result_positivity column was dropped from a select in 05_adjudicate.R and
# the Aim 5 publication-bias block has been gated off since 2026-04-17 with no
# error anywhere. A promise with nothing enforcing it decays exactly that way.

library(testthat)

CONTRACT <- here::here("config", "ci_contract.yml")

read_contract <- function() {
  skip_if(!file.exists(CONTRACT), "contract absent")
  yaml::read_yaml(CONTRACT)
}
workflow_text <- function(name) {
  p <- here::here(".github", "workflows", name)
  if (!file.exists(p)) return(NA_character_)
  paste(readLines(p, warn = FALSE), collapse = "\n")
}

# ============================================================
# The contract is well formed
# ============================================================
test_that("every gate declares what it protects and what a failure means", {
  ct <- read_contract()
  expect_gt(length(ct$gates), 0)
  incomplete <- character(0)
  for (g in ct$gates) {
    missing <- setdiff(c("id", "protects", "implemented_by", "failure_means"), names(g))
    if (length(missing)) {
      incomplete <- c(incomplete, sprintf("%s (missing %s)",
                                          g$id %||% "<unnamed>",
                                          paste(missing, collapse = ", ")))
    }
  }
  expect_true(length(incomplete) == 0,
              label = paste("gate(s) missing required fields:",
                            paste(incomplete, collapse = "; ")))
})

test_that("gate ids are unique", {
  ct <- read_contract()
  ids <- vapply(ct$gates, function(g) g$id, character(1))
  expect_equal(anyDuplicated(ids), 0L,
               label = paste("duplicate gate id:", ids[duplicated(ids)][1]))
})

# ============================================================
# The workflows implement it
# ============================================================
test_that("every gate names a workflow that exists", {
  ct <- read_contract()
  missing <- character(0)
  for (g in ct$gates) {
    wf <- g$implemented_by$workflow
    if (is.null(wf) || is.na(workflow_text(wf))) {
      missing <- c(missing, sprintf("%s -> %s", g$id, wf %||% "<none>"))
    }
  }
  expect_true(length(missing) == 0,
              label = paste("gate(s) point at a workflow that does not exist:",
                            paste(missing, collapse = "; ")))
})

test_that("every gate's implementing step is present in its workflow", {
  ct <- read_contract()
  unimplemented <- character(0)
  for (g in ct$gates) {
    txt <- workflow_text(g$implemented_by$workflow)
    if (is.na(txt)) next
    step <- g$implemented_by$step
    # A gate whose step name no longer appears is a promise with nothing behind
    # it. Matching on the literal step name keeps the contract and the workflow
    # renamed together or not at all.
    if (is.null(step) || !grepl(step, txt, fixed = TRUE)) {
      unimplemented <- c(unimplemented, sprintf("%s expects step '%s' in %s",
                                                g$id, step %||% "<none>",
                                                g$implemented_by$workflow))
    }
  }
  expect_true(length(unimplemented) == 0,
              label = paste("gate(s) not implemented by any workflow step:",
                            paste(unimplemented, collapse = "; ")))
})

test_that("a gate naming a test file points at a file that exists", {
  ct <- read_contract()
  missing <- character(0)
  for (g in ct$gates) {
    tf <- g$implemented_by$test_file
    if (is.null(tf)) next
    if (!file.exists(here::here("tests", "testthat", tf))) {
      missing <- c(missing, sprintf("%s -> %s", g$id, tf))
    }
  }
  expect_true(length(missing) == 0,
              label = paste("gate(s) name a missing test file:",
                            paste(missing, collapse = "; ")))
})

# ============================================================
# The render gate's own inputs
# ============================================================
test_that("every document the contract requires to render exists", {
  ct <- read_contract()
  docs <- ct$rendered_documents
  skip_if(length(docs) == 0, "no documents declared")
  missing <- docs[!file.exists(here::here(docs))]
  expect_true(length(missing) == 0,
              label = paste("declared document(s) missing:",
                            paste(missing, collapse = ", ")))
})

test_that("the render gate has pandoc and rmarkdown available to it", {
  ct <- read_contract()
  gate <- Filter(function(g) identical(g$id, "manuscript_renders"), ct$gates)
  skip_if(length(gate) == 0, "no render gate declared")
  txt <- workflow_text(gate[[1]]$implemented_by$workflow)
  skip_if(is.na(txt), "workflow absent")
  # rmarkdown::render needs pandoc, which setup-r-dependencies does not install.
  # Without it the gate skips or errors for a reason unrelated to the documents.
  expect_true(grepl("setup-pandoc", txt, fixed = TRUE),
              label = "the render gate's workflow does not install pandoc")
  expect_true(grepl("any::rmarkdown", txt, fixed = TRUE),
              label = "the render gate's workflow does not install rmarkdown")
})

# ============================================================
# The manifest the contract points at
# ============================================================
test_that("the expected-failure manifest named by the contract exists and parses", {
  ct <- read_contract()
  mf <- ct$expected_failures_manifest
  skip_if(is.null(mf), "no manifest declared")
  p <- here::here(mf)
  expect_true(file.exists(p),
              label = paste("contract names a manifest that does not exist:", mf))
  skip_if(!file.exists(p), "manifest absent")
  y <- yaml::read_yaml(p)
  expect_true(!is.null(y$expected_failures),
              label = "the manifest has no expected_failures key")
})

test_that("the documents actually render in this environment", {
  ct <- read_contract()
  skip_if_not(requireNamespace("rmarkdown", quietly = TRUE), "rmarkdown absent")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc unavailable")
  failed <- character(0)
  for (d in ct$rendered_documents) {
    p <- here::here(d)
    if (!file.exists(p)) next
    out <- tempfile(fileext = ".docx")
    res <- tryCatch({ rmarkdown::render(p, output_file = out, quiet = TRUE); "ok" },
                    error = function(e) conditionMessage(e))
    if (!identical(res, "ok")) failed <- c(failed, paste0(d, ": ", res))
  }
  expect_true(length(failed) == 0,
              label = paste("document(s) failed to render:",
                            paste(failed, collapse = "; ")))
})
