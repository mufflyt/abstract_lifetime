# config/ci_contract.yml must describe the CI that actually runs.
#
# The definition of "green" used to live in three places -- tests.yaml,
# R-CMD-check.yaml and run_suite_gate.R -- and they had already drifted once,
# when R-CMD-check used stop_on_failure = TRUE and disagreed about whether the
# preserved failures were fatal. Moving the contract into one file only helps
# if something checks that the file is true, which is what this does.

library(testthat)

repo_root <- here::here()
contract_path <- file.path(repo_root, "config", "ci_contract.yml")

test_that("the CI contract exists and is well-formed", {
  expect_true(file.exists(contract_path))
  ct <- yaml::read_yaml(contract_path)

  expect_true(is.numeric(ct$version) || is.character(ct$version))
  expect_gt(length(ct$gates), 0)
  expect_true(!is.null(ct$manifest$path))

  for (g in ct$gates) {
    expect_true(nzchar(g$id %||% ""), label = "a gate has no id")
    expect_true(nzchar(g$name %||% ""), label = sprintf("gate %s has no name", g$id))
    expect_true(nzchar(g$rationale %||% ""),
                label = sprintf("gate %s has no rationale; an undocumented gate is one nobody can safely remove", g$id))
  }
})

test_that("every gate names a file that exists", {
  ct <- yaml::read_yaml(contract_path)
  for (g in ct$gates) {
    target <- g$file %||% g$runner
    expect_true(nzchar(target %||% ""),
                label = sprintf("gate %s names neither a file nor a runner", g$id))
    expect_true(file.exists(file.path(repo_root, target)),
                label = sprintf("gate %s points at %s, which does not exist", g$id, target))
  }
})

test_that("every declared workflow exists and invokes the shared gate", {
  ct <- yaml::read_yaml(contract_path)
  runner <- NULL
  for (g in ct$gates) if (!is.null(g$runner)) runner <- g$runner
  skip_if(is.null(runner), "no runner declared")

  for (w in ct$workflows) {
    p <- file.path(repo_root, w$path)
    expect_true(file.exists(p),
                label = sprintf("declared workflow %s does not exist", w$path))
    if (!file.exists(p)) next
    if (isTRUE(w$must_invoke_gate)) {
      txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
      expect_true(
        grepl(runner, txt, fixed = TRUE),
        label = sprintf(paste("%s does not invoke %s. A workflow that runs the suite its own",
                              "way is how tests.yaml and R-CMD-check.yaml disagreed about",
                              "green the first time"), w$path, runner))
    }
  }
})

test_that("the manifest obeys the contract it is held to", {
  ct <- yaml::read_yaml(contract_path)
  mpath <- file.path(repo_root, ct$manifest$path)
  skip_if_not(file.exists(mpath))
  entries <- yaml::read_yaml(mpath)$expected_failures %||% list()

  # The ratchet. The manifest is meant to shrink.
  if (!is.null(ct$manifest$max_entries)) {
    expect_lte(
      length(entries), ct$manifest$max_entries,
      label = sprintf(paste("%d expected-failure entries against a ceiling of %d.",
                            "Close a decision, or raise max_entries deliberately"),
                      length(entries), ct$manifest$max_entries))
  }

  # Every entry must carry its reason and the decision it waits on, so the
  # manifest cannot decay into a list of bare test names.
  req <- ct$manifest$require_fields %||% character(0)
  for (e in entries) {
    for (f in req) {
      expect_true(
        !is.null(e[[f]]) && nzchar(trimws(as.character(e[[f]]))),
        label = sprintf("manifest entry '%s' is missing required field '%s'",
                        e$test %||% "<unnamed>", f))
    }
  }
})

test_that("the gate reads the contract rather than hard-coding it", {
  gate <- file.path(repo_root, "tests", "run_suite_gate.R")
  skip_if_not(file.exists(gate))
  txt <- paste(readLines(gate, warn = FALSE), collapse = "\n")
  expect_true(grepl("config/ci_contract.yml", txt, fixed = TRUE),
              label = "run_suite_gate.R does not read config/ci_contract.yml, so the contract is decorative")
})
