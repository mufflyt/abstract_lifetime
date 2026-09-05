# gate_rules.R — how the suite gate classifies a test run.
#
# Extracted from run_suite_gate.R so the rules can be unit-tested against
# synthetic results. They previously existed only inside a script that runs the
# whole suite, which meant the only way to exercise them was a full CI run --
# and a bug in them (skips counted as passes) took main red with no true fix
# available.

gate_key <- function(file, test) paste(file, test, sep = " :: ")

#' Classify a testthat results data frame against the expected-failure manifest.
#'
#' @param df       as.data.frame(testthat::test_dir(...))
#' @param expected character vector of "file :: test" keys from the manifest
#' @return list(failed_keys, unexpected, stale, orphaned, skipped_entries)
gate_classify <- function(df, expected) {
  if (!nrow(df)) {
    return(list(failed_keys = character(0), unexpected = character(0),
                stale = character(0), orphaned = expected,
                skipped_entries = character(0)))
  }

  skipped_flag <- if ("skipped" %in% names(df)) df$skipped > 0 else rep(FALSE, nrow(df))
  all_keys <- gate_key(df$file, df$test)

  failed <- df$failed > 0 | df$error > 0
  failed_keys <- all_keys[failed]

  # A skipped test proves nothing. It cannot be evidence that an expected
  # failure has started passing.
  passing <- !failed & !skipped_flag

  list(
    failed_keys     = failed_keys,
    unexpected      = failed_keys[!failed_keys %in% expected],
    stale           = expected[expected %in% all_keys[passing]],
    orphaned        = expected[!expected %in% all_keys],
    skipped_entries = expected[expected %in% all_keys[skipped_flag]]
  )
}

#' Classify which tests SKIPPED against the approved-skip manifest.
#'
#' A skipped test asserts nothing. Counted as a pass it is worse than absent,
#' because it occupies the place where coverage is supposed to be: that is how
#' 45 bundle assertions and a backfill-coverage check sat inert in CI for weeks
#' while reporting green.
#'
#' Direction matters here. An UNAPPROVED skip is a failure: a test that stopped
#' asserting without anyone saying so. An approved skip that ran anyway is NOT
#' a failure, because the skip set is genuinely environment-dependent -- the
#' Shiny bundle and the PubMed cache exist on a developer machine and not in a
#' clean checkout, so the same suite legitimately skips different tests in the
#' two places. Those are reported, not enforced.
#'
#' @param df       as.data.frame(testthat::test_dir(...))
#' @param approved character vector of "file :: test" keys from the skip manifest
#' @return list(skipped_keys, unapproved, did_not_skip)
skip_classify <- function(df, approved) {
  if (!nrow(df)) {
    return(list(skipped_keys = character(0), unapproved = character(0),
                did_not_skip = approved))
  }
  skipped_flag <- if ("skipped" %in% names(df)) df$skipped > 0 else rep(FALSE, nrow(df))
  all_keys <- gate_key(df$file, df$test)
  skipped_keys <- all_keys[skipped_flag]

  list(
    skipped_keys = skipped_keys,
    unapproved   = setdiff(skipped_keys, approved),
    did_not_skip = setdiff(approved, skipped_keys)
  )
}
