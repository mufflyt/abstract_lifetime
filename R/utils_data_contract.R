# utils_data_contract.R — evaluate config/data_contract.yml row by row.
#
# The point of a row-level contract is that a violation names the row. An
# aggregate check tells you the dataset is wrong; this tells you which
# abstract_id to look at.

suppressPackageStartupMessages({library(dplyr); library(tibble)})

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Check one column specification against a data frame.
#' @return a tibble of violations, one row per offending data row.
check_column <- function(df, col, spec, key) {
  out <- list()
  add <- function(idx, check, detail) {
    if (!length(idx)) return(invisible(NULL))
    out[[length(out) + 1]] <<- tibble(
      column = col, check = check,
      row = idx,
      key_value = as.character(df[[key]])[idx],
      detail = detail)
  }

  if (!col %in% names(df)) {
    if (isTRUE(spec$required)) {
      out[[1]] <- tibble(column = col, check = "column_present", row = NA_integer_,
                         key_value = NA_character_, detail = "column is absent")
    }
    return(bind_rows(out))
  }

  x <- df[[col]]
  na_ok <- isTRUE(spec$allow_na)

  if (isTRUE(spec$required) && !na_ok) {
    add(which(is.na(x)), "required", "value is NA")
  }

  if (isTRUE(spec$unique)) {
    dup <- which(duplicated(x) | duplicated(x, fromLast = TRUE))
    add(dup, "unique", "duplicated key")
  }

  present <- !is.na(x)

  if (!is.null(spec$type)) {
    ok <- switch(spec$type,
      integer = is.numeric(x) && all(x[present] == floor(x[present])),
      numeric = is.numeric(x),
      logical = is.logical(x),
      character = is.character(x),
      TRUE)
    if (!isTRUE(ok)) {
      out[[length(out) + 1]] <- tibble(
        column = col, check = "type", row = NA_integer_, key_value = NA_character_,
        detail = sprintf("expected %s, found %s", spec$type, class(x)[1]))
    }
  }

  if (!is.null(spec$min) && is.numeric(x)) {
    add(which(present & x < spec$min), "min",
        sprintf("below minimum %s", spec$min))
  }
  if (!is.null(spec$max) && is.numeric(x)) {
    add(which(present & x > spec$max), "max",
        sprintf("above maximum %s", spec$max))
  }
  if (!is.null(spec$allowed)) {
    add(which(present & !(as.character(x) %in% as.character(spec$allowed))),
        "allowed", "value outside the allowed set")
  }
  if (!is.null(spec$regex)) {
    add(which(present & !grepl(spec$regex, as.character(x))),
        "regex", sprintf("does not match %s", spec$regex))
  }

  bind_rows(out)
}

#' Evaluate the cross-field rules.
check_rules <- function(df, rules, key) {
  out <- list()
  for (r in rules) {
    # `holds_when` lets a rule assert a property of the dataset as a whole
    # rather than of each row.
    if (!is.null(r$holds_when)) {
      ok <- tryCatch(isTRUE(eval(parse(text = r$holds_when), envir = df)),
                     error = function(e) FALSE)
      if (!ok) {
        out[[length(out) + 1]] <- tibble(
          column = NA_character_, check = r$id, row = NA_integer_,
          key_value = NA_character_,
          detail = sprintf("dataset-level condition failed: %s", r$holds_when))
      }
      next
    }
    val <- tryCatch(eval(parse(text = r$expr), envir = df),
                    error = function(e) {
                      structure(FALSE, err = conditionMessage(e))
                    })
    if (length(val) == 1 && !is.null(attr(val, "err"))) {
      out[[length(out) + 1]] <- tibble(
        column = NA_character_, check = r$id, row = NA_integer_,
        key_value = NA_character_,
        detail = paste("rule did not evaluate:", attr(val, "err")))
      next
    }
    bad <- which(!(val %in% TRUE))
    if (length(bad)) {
      out[[length(out) + 1]] <- tibble(
        column = NA_character_, check = r$id, row = bad,
        key_value = as.character(df[[key]])[bad],
        detail = r$description %||% r$expr)
    }
  }
  bind_rows(out)
}

#' Validate every dataset named in the contract.
#' @return a tibble of violations; zero rows means the contract holds.
validate_data_contract <- function(contract_path, root = here::here()) {
  ct <- yaml::read_yaml(contract_path)
  all <- list()
  for (ds in ct$datasets) {
    p <- file.path(root, ds$path)
    if (!file.exists(p)) {
      all[[length(all) + 1]] <- tibble(
        dataset = ds$path, column = NA_character_, check = "dataset_present",
        row = NA_integer_, key_value = NA_character_, detail = "file is absent")
      next
    }
    df <- readr::read_csv(p, show_col_types = FALSE)
    key <- ds$key
    v <- list()
    for (col in names(ds$columns)) {
      v[[length(v) + 1]] <- check_column(df, col, ds$columns[[col]], key)
    }
    v[[length(v) + 1]] <- check_rules(df, ds$rules %||% list(), key)
    v <- bind_rows(v)
    if (nrow(v)) v$dataset <- ds$path
    all[[length(all) + 1]] <- v
  }
  res <- bind_rows(all)
  if (!nrow(res)) {
    return(tibble(dataset = character(), column = character(), check = character(),
                  row = integer(), key_value = character(), detail = character()))
  }
  dplyr::relocate(res, dataset)
}
