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
  ct <- yaml::yaml.load_file(contract_path)

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
  ct <- yaml::yaml.load_file(contract_path)
  for (g in ct$gates) {
    target <- g$file %||% g$runner
    expect_true(nzchar(target %||% ""),
                label = sprintf("gate %s names neither a file nor a runner", g$id))
    expect_true(file.exists(file.path(repo_root, target)),
                label = sprintf("gate %s points at %s, which does not exist", g$id, target))
  }
})

test_that("every declared workflow exists and invokes the shared gate", {
  ct <- yaml::yaml.load_file(contract_path)
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
  ct <- yaml::yaml.load_file(contract_path)
  mpath <- file.path(repo_root, ct$manifest$path)
  skip_if_not(file.exists(mpath))
  entries <- yaml::yaml.load_file(mpath)$expected_failures %||% list()

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

test_that("no workflow runs without being declared in the contract", {
  ct <- yaml::yaml.load_file(contract_path)
  declared <- vapply(ct$workflows, `[[`, character(1), "path")

  wf_dir <- file.path(repo_root, ".github", "workflows")
  skip_if_not(dir.exists(wf_dir), "no workflows directory")
  on_disk <- file.path(".github", "workflows",
                       list.files(wf_dir, pattern = "\\.ya?ml$"))

  # The contract previously checked only that a DECLARED workflow exists. That
  # catches a deleted workflow and misses the more likely drift: a new workflow
  # added without being described. manuscript.yaml sat undeclared from the day
  # it was added, so nothing recorded whether it was meant to gate or only to
  # render.
  undeclared <- setdiff(on_disk, declared)
  expect_true(length(undeclared) == 0,
              label = paste("workflow(s) run in CI but are not declared in",
                            "config/ci_contract.yml:",
                            paste(undeclared, collapse = ", "),
                            "- add each with a role and whether it must invoke the gate"))
})

test_that("every must-pass gate is actually run by a declared workflow", {
  ct <- yaml::yaml.load_file(contract_path)
  declared <- vapply(ct$workflows, `[[`, character(1), "path")
  txt <- paste(unlist(lapply(file.path(repo_root, declared), function(p)
    if (file.exists(p)) readLines(p, warn = FALSE) else character(0))),
    collapse = "\n")

  # A gate declared here but invoked by no workflow is a promise nothing keeps.
  # The suite gate is exempt: it is named by `runner` and checked above.
  unrun <- character(0)
  for (g in ct$gates) {
    if (is.null(g$file)) next
    if (!identical(g$policy, "all_must_pass")) next
    if (!grepl(g$file, txt, fixed = TRUE)) unrun <- c(unrun, g$id)
  }
  expect_true(length(unrun) == 0,
              label = paste("gate(s) declared in the contract that no declared",
                            "workflow runs:", paste(unrun, collapse = ", ")))
})

test_that("the approved-skip manifest obeys the contract it is held to", {
  ct <- yaml::yaml.load_file(contract_path)
  skip_if(is.null(ct$skips$path), "no skip manifest declared")
  spath <- file.path(repo_root, ct$skips$path)
  expect_true(file.exists(spath),
              label = sprintf("the contract declares %s, which does not exist",
                              ct$skips$path))
  skip_if_not(file.exists(spath))
  entries <- yaml::yaml.load_file(spath)$expected_skips %||% list()

  # Same rule as the failure manifest: an entry without a reason and a route to
  # enabling it is an exemption, not a backlog item.
  req <- ct$skips$require_fields %||% character(0)
  for (e in entries) {
    for (f in req) {
      expect_true(
        !is.null(e[[f]]) && nzchar(trimws(as.character(e[[f]]))),
        label = sprintf("skip manifest entry '%s' is missing required field '%s'",
                        e$test %||% "<unnamed>", f))
    }
  }
})
