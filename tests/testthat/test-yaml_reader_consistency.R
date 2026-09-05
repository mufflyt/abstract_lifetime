# The repository reads YAML through one function so that a reader-specific
# quirk, if one is ever found, has a single place to be fixed. yaml::read_yaml
# and yaml::yaml.load_file are not such a case: they parse every config and
# manifest in this repo to identical() output, including the five that contain
# non-ASCII bytes. The standardisation is for consistency, not correctness, and
# these tests keep it from drifting back into a mix of the two.

repo_root <- here::here()

r_sources <- function() {
  dirs <- file.path(repo_root, c("R", "tests", "scripts", "shiny", "docs"))
  dirs <- dirs[dir.exists(dirs)]
  f <- list.files(dirs, pattern = "[.](R|Rmd)$", recursive = TRUE, full.names = TRUE)
  # This file names both readers on purpose, to compare them.
  f[basename(f) != "test-yaml_reader_consistency.R"]
}

test_that("no source file calls yaml::read_yaml", {
  srcs <- r_sources()
  expect_true(length(srcs) > 0)
  offenders <- Filter(function(f) any(grepl("read_yaml", readLines(f, warn = FALSE))), srcs)
  expect_true(
    length(offenders) == 0,
    info = paste0(
      "Use yaml::yaml.load_file(). Files still calling read_yaml: ",
      paste(sub(paste0(repo_root, "/"), "", offenders, fixed = TRUE), collapse = ", ")))
})

test_that("the two readers agree on every config and manifest we ship", {
  # If this ever fails, the standardisation stops being cosmetic and the
  # difference needs investigating before either reader is trusted.
  paths <- file.path(repo_root, c(
    "config.yml", "config/ci_contract.yml", "config/data_contract.yml",
    "tests/expected_failures.yaml", "tests/expected_skips.yaml",
    "docs/estimand_baseline.yml"))
  paths <- paths[file.exists(paths)]
  expect_true(length(paths) >= 4)
  for (p in paths) {
    expect_identical(yaml::read_yaml(p), yaml::yaml.load_file(p),
                     info = basename(p))
  }
})

test_that("reading a file with non-ASCII content is not the failure mode", {
  # An em-dash in a comment was reported as breaking the parser. It does not.
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("# a comment — with an em-dash", "key: value",
               "unicode_value: café"), con = tmp, useBytes = FALSE)
  got <- yaml::yaml.load_file(tmp)
  expect_equal(got$key, "value")
  expect_identical(yaml::read_yaml(tmp), got)
})

test_that("write_yaml call sites were not rewritten", {
  # write_yaml is the writer and has no yaml.load_file equivalent; a careless
  # search-and-replace across "yaml" would have taken it out.
  srcs <- r_sources()
  writers <- Filter(function(f) any(grepl("write_yaml", readLines(f, warn = FALSE))), srcs)
  expect_true(length(writers) > 0,
              info = "expected write_yaml to still be used for emitting YAML")
})
