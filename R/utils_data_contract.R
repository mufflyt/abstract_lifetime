# utils_data_contract.R - the row-level data contract, as a pointblank agent.
#
# This was a hand-rolled validator: a YAML schema plus about 150 lines of
# checking code that reimplemented column types, ranges, regexes, uniqueness and
# per-row reporting. It worked, and every rule it encoded is preserved here, but
# maintaining a validation engine is not this project's job.
#
# pointblank (Iannone & Vanderkam, https://github.com/posit-dev/pointblank) is
# the maintained tool for exactly this. The rules now compose as an agent, which
# brings three things the hand-rolled version did not have:
#
#   * an interrogation report, written to output/, that says which rows failed
#     rather than only how many;
#   * action levels, so a rule can warn rather than stop where that is the
#     honest severity;
#   * `col_vals_expr()` for the cross-field rules, which keeps them declarative
#     instead of eval()-ing strings from YAML.
#
# config/data_contract.yml is still the source of truth for WHICH rules exist.
# This file turns it into an agent. Keeping the YAML means the contract stays
# readable by someone who does not know pointblank, and reviewable in a diff.

suppressPackageStartupMessages({
  library(dplyr); library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Build a pointblank agent from config/data_contract.yml
#'
#' @param df The table to validate.
#' @param ds One `datasets:` entry from the contract.
#' @param label Agent label shown in the report.
#' @return A pointblank agent, not yet interrogated.
build_contract_agent <- function(df, ds, label = ds$path) {
  stopifnot(requireNamespace("pointblank", quietly = TRUE))
  a <- pointblank::create_agent(tbl = df, label = label,
                                actions = pointblank::action_levels(stop_at = 1))

  # pointblank names a step by its assertion and brief, which is right for the
  # HTML report and wrong for tests, which need a stable id. `step()` records a
  # canonical id in step order so the two can be reported side by side.
  ids <- character(0)
  step <- function(id, fn) { ids[[length(ids) + 1L]] <<- id; a <<- fn(a) }

  for (col in names(ds$columns)) {
    spec <- ds$columns[[col]]
    cc <- col

    if (!cc %in% names(df)) {
      if (isTRUE(spec$required)) {
        step("column_present", function(a) pointblank::col_exists(a, columns = dplyr::all_of(cc)))
      }
      next
    }
    step("column_present", function(a) pointblank::col_exists(a, columns = dplyr::all_of(cc)))
    na_ok <- isTRUE(spec$allow_na)

    if (isTRUE(spec$required) && !na_ok) {
      step("required", function(a) pointblank::col_vals_not_null(a, columns = dplyr::all_of(cc)))
    }
    if (isTRUE(spec$unique)) {
      step("unique", function(a) pointblank::rows_distinct(a, columns = dplyr::all_of(cc)))
    }
    if (!is.null(spec$type)) {
      # `type: integer` in the contract means WHOLE NUMBERS, not R's integer
      # type. readr reads CSV numerics as double, so col_is_integer() would fail
      # on any column that round-tripped through a file, which is all of them.
      # Checked as numeric-and-whole instead, which is what the contract meant
      # and what the hand-rolled validator enforced.
      if (identical(spec$type, "integer")) {
        step("type", function(a) pointblank::col_is_numeric(a, columns = dplyr::all_of(cc)))
        step("type", function(a) pointblank::col_vals_expr(
          a, expr = rlang::expr(!!rlang::sym(cc) == floor(!!rlang::sym(cc))),
          brief = paste0(cc, " is a whole number")))
      } else {
        step("type", function(a) switch(spec$type,
          numeric   = pointblank::col_is_numeric(a, columns = dplyr::all_of(cc)),
          logical   = pointblank::col_is_logical(a, columns = dplyr::all_of(cc)),
          character = pointblank::col_is_character(a, columns = dplyr::all_of(cc)),
          a))
      }
    }
    if (!is.null(spec$min)) {
      step("min", function(a) pointblank::col_vals_gte(
        a, columns = dplyr::all_of(cc), value = spec$min, na_pass = na_ok))
    }
    if (!is.null(spec$max)) {
      step("max", function(a) pointblank::col_vals_lte(
        a, columns = dplyr::all_of(cc), value = spec$max, na_pass = na_ok))
    }
    if (!is.null(spec$allowed)) {
      step("allowed", function(a) pointblank::col_vals_in_set(
        a, columns = dplyr::all_of(cc), set = as.character(spec$allowed)))
    }
    if (!is.null(spec$regex)) {
      step("regex", function(a) pointblank::col_vals_regex(
        a, columns = dplyr::all_of(cc), regex = spec$regex, na_pass = na_ok))
    }
  }

  for (r in ds$rules %||% list()) {
    # `holds_when` rules assert a property of the table, not of each row.
    # pointblank has no idiom for that, so they are evaluated separately in
    # validate_data_contract().
    if (!is.null(r$holds_when)) next
    rr <- r
    step(rr$id, function(a) pointblank::col_vals_expr(
      a, expr = rlang::parse_expr(rr$expr),
      brief = trimws(rr$description %||% rr$expr)))
  }

  list(agent = a, ids = ids)
}

#' Validate every dataset named in the contract.
#'
#' @return A tibble of failures, one row per failing validation step. Zero rows
#'   means the contract holds. Row-level detail is in the written report.
validate_data_contract <- function(contract_path, root = here::here(),
                                   report_dir = NULL) {
  ct <- yaml::yaml.load_file(contract_path)
  out <- list()

  for (ds in ct$datasets) {
    p <- file.path(root, ds$path)
    if (!file.exists(p)) {
      out[[length(out) + 1]] <- tibble(
        dataset = ds$path, check = "dataset_present", column = NA_character_,
        n_failed = NA_integer_, detail = "file is absent")
      next
    }
    df <- readr::read_csv(p, show_col_types = FALSE)

    # Dataset-level rules first: these assert a property of the table.
    for (r in ds$rules %||% list()) {
      if (is.null(r$holds_when)) next
      ok <- tryCatch(isTRUE(eval(parse(text = r$holds_when), envir = df)),
                     error = function(e) FALSE)
      if (!ok) {
        out[[length(out) + 1]] <- tibble(
          dataset = ds$path, check = r$id, column = NA_character_,
          n_failed = NA_integer_,
          detail = paste("dataset-level condition failed:", r$holds_when))
      }
    }

    built    <- build_contract_agent(df, ds)
    res      <- pointblank::interrogate(built$agent)
    step_ids <- built$ids

    if (!is.null(report_dir)) {
      dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
      try(pointblank::export_report(
        res, filename = file.path(report_dir,
          paste0("data_contract_", gsub("[^A-Za-z0-9]+", "_", ds$path), ".html")),
        quiet = TRUE), silent = TRUE)
    }

    # Field names come from get_agent_x_list(): `type` is the assertion,
    # `columns` the target, `briefs` the human-readable rule text. There is no
    # all_passed vector; a step with eval_error never ran and must not be read
    # as a pass.
    x <- pointblank::get_agent_x_list(res)
    nf  <- suppressWarnings(as.integer(x$n_failed))
    err <- !is.na(x$eval_error) & x$eval_error
    failed <- which((!is.na(nf) & nf > 0) | err)

    for (i in failed) {
      brief <- tryCatch(as.character(x$briefs[[i]]), error = function(e) NA_character_)
      out[[length(out) + 1]] <- tibble(
        dataset  = ds$path,
        check    = if (i <= length(step_ids)) step_ids[[i]] else as.character(x$type[i]),
        brief    = brief %||% NA_character_,
        column   = tryCatch(paste(as.character(x$columns[[i]]), collapse = ", "),
                            error = function(e) NA_character_),
        n_failed = if (is.na(nf[i])) NA_integer_ else nf[i],
        detail   = if (err[i]) {
          sprintf("%s did not evaluate", x$type[i])
        } else {
          sprintf("%s failed on %s of %s rows", x$type[i], nf[i], x$n[i])
        })
    }
  }

  res <- bind_rows(out)
  if (!nrow(res)) {
    return(tibble(dataset = character(), check = character(), brief = character(),
                  column = character(), n_failed = integer(), detail = character()))
  }
  res
}
