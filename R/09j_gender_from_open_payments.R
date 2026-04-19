#!/usr/bin/env Rscript
# 09j_gender_from_open_payments.R — Gender inference via CMS Open Payments
#
# For each AAGL presenter still missing gender, search the CMS Open Payments
# (Sunshine Act) database for payments received during the AAGL conference
# window. Matching criteria:
#   1. Last name  — exact, case-insensitive
#   2. First initial — first character of first name
#   3. OB/GYN specialty — eliminates same-named dentists, orthopedists, etc.
#   4. Date window — conf_start-2 days through conf_end+2 days
#
# If exactly one unique first name survives those filters, that name is used
# for gender inference via SSA + genderize.io.
#
# Coverage:
#   2013 — open_payments_2013  (Physician_* columns; Aug–Dec 2013)
#   2014 — open_payments_2014  (Physician_* columns)
#   2015 — SKIPPED (corrupt payment dates in DuckDB table)
#   2016–2023 — open_payments_all_years (Covered_Recipient_* columns)
#
# DuckDB: /Volumes/MufflySamsung/DuckDB/nber_my_duckdb.duckdb
# Cache:  data/cache/open_payments/<last>_<init>_<year>.rds
# Writes: data/processed/gender_from_open_payments.csv
# Updates: output/abstracts_with_matches.csv

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(stringr)
  library(purrr); library(cli); library(DBI); library(duckdb)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

DUCKDB_PATH  <- "/Volumes/MufflySamsung/DuckDB/nber_my_duckdb.duckdb"
cache_dir    <- here("data", "cache", "open_payments")
matches_path <- here("output", "abstracts_with_matches.csv")
out_path     <- here("data", "processed", "gender_from_open_payments.csv")

dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

cli_h1("Gender enrichment from CMS Open Payments (Sunshine Act)")

if (!file.exists(DUCKDB_PATH)) {
  cli_alert_warning("DuckDB not found at {DUCKDB_PATH} — skipping")
  write_csv(tibble(), out_path)
  invisible(NULL)
}

# ── AAGL conference dates ──────────────────────────────────────────────────────
# 2015 omitted — corrupt payment dates in the open_payments_2015 DuckDB table
aagl_dates <- data.frame(
  congress_year = c(2013L, 2014L, 2016L, 2017L, 2018L, 2019L, 2020L, 2021L, 2022L, 2023L),
  conf_start    = as.Date(c("2013-11-13","2014-11-12","2016-11-16","2017-11-12",
                             "2018-11-07","2019-11-06","2020-11-18","2021-11-10",
                             "2022-11-02","2023-11-08")),
  conf_end      = as.Date(c("2013-11-16","2014-11-15","2016-11-19","2017-11-15",
                             "2018-11-10","2019-11-09","2020-11-21","2021-11-13",
                             "2022-11-05","2023-11-11")),
  # Which DuckDB table / column schema to use
  db_table      = c("open_payments_2013","open_payments_2014",
                    rep("open_payments_all_years", 8)),
  col_schema    = c("physician","physician",
                    rep("covered_recipient", 8)),
  stringsAsFactors = FALSE
)

#' Build a SQL query for Open Payments physician lookup
#'
#' @param last Character. Uppercase last name.
#' @param init1 Character. Uppercase first initial.
#' @param win_start Character. Date window start (YYYY-MM-DD).
#' @param win_end Character. Date window end (YYYY-MM-DD).
#' @param table Character. DuckDB table name.
#' @param schema Character. Column naming scheme ("physician" or "covered_recipient").
#' @return Character. SQL query string.
#' @keywords internal
make_sql <- function(last, init1, win_start, win_end, table, schema) {
  obgyn_filter <- "('%Obstetrics%', '%Gynecology%')"

  if (schema == "physician") {
    sprintf(
      "SELECT DISTINCT
         Physician_First_Name  AS first_name,
         Physician_Last_Name   AS last_name,
         Physician_Specialty   AS specialty,
         Nature_of_Payment_or_Transfer_of_Value AS payment_type,
         Total_Amount_of_Payment_USDollars       AS amount,
         Date_of_Payment
       FROM %s
       WHERE UPPER(Physician_Last_Name) = '%s'
         AND UPPER(LEFT(Physician_First_Name, 1)) = '%s'
         AND Date_of_Payment BETWEEN DATE '%s' AND DATE '%s'
         AND (Physician_Specialty LIKE '%%Obstetrics%%'
              OR Physician_Specialty LIKE '%%Gynecology%%')
       LIMIT 20",
      table, toupper(last), toupper(init1), win_start, win_end
    )
  } else {
    sprintf(
      "SELECT DISTINCT
         Covered_Recipient_First_Name  AS first_name,
         Covered_Recipient_Last_Name   AS last_name,
         Covered_Recipient_Specialty_1 AS specialty,
         Nature_of_Payment_or_Transfer_of_Value AS payment_type,
         Total_Amount_of_Payment_USDollars       AS amount,
         Date_of_Payment
       FROM %s
       WHERE UPPER(Covered_Recipient_Last_Name) = '%s'
         AND UPPER(LEFT(Covered_Recipient_First_Name, 1)) = '%s'
         AND Date_of_Payment BETWEEN DATE '%s' AND DATE '%s'
         AND (Covered_Recipient_Specialty_1 LIKE '%%Obstetrics%%'
              OR Covered_Recipient_Specialty_1 LIKE '%%Gynecology%%'
              OR Covered_Recipient_Specialty_2 LIKE '%%Obstetrics%%'
              OR Covered_Recipient_Specialty_2 LIKE '%%Gynecology%%')
       LIMIT 20",
      table, toupper(last), toupper(init1), win_start, win_end
    )
  }
}

#' Query Open Payments for an author's full first name
#'
#' Searches across multiple Open Payments tables (2013-2023) for a
#' physician matching last name + first initial + OB/GYN specialty,
#' within a date window around the congress year. Results cached to disk.
#'
#' @param last Character. Author last name.
#' @param init1 Character. First initial.
#' @param congress_year Integer. AAGL congress year.
#' @param conf_row Tibble row. Conference config with \code{date}.
#' @param con DuckDB connection.
#' @return Character scalar. Full first name or \code{NA_character_}.
#' @keywords internal
fetch_op_first_name <- function(last, init1, congress_year, conf_row, con) {
  cache_key  <- paste0(tolower(last), "_", tolower(init1), "_", congress_year)
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  win_start <- format(conf_row$conf_start - 90, "%Y-%m-%d")
  win_end   <- format(conf_row$conf_end   + 90, "%Y-%m-%d")
  sql <- make_sql(last, init1, win_start, win_end,
                  conf_row$db_table, conf_row$col_schema)

  hits <- tryCatch(dbGetQuery(con, sql), error = function(e) data.frame())

  if (nrow(hits) == 0) {
    saveRDS(list(first_name = NA_character_, hits = hits), cache_file)
    return(list(first_name = NA_character_, hits = hits))
  }

  # Normalise case for deduplication
  hits$first_name_upper <- toupper(str_squish(hits$first_name))
  unique_firsts <- unique(hits$first_name_upper[nchar(hits$first_name_upper) >= 2])

  # Ambiguous: multiple distinct first names among OB/GYN physicians → skip
  result <- if (length(unique_firsts) == 1) {
    str_to_title(unique_firsts[1])
  } else {
    NA_character_
  }

  out <- list(first_name = result, hits = hits)
  saveRDS(out, cache_file)
  out
}

# ── Load targets ───────────────────────────────────────────────────────────────
matches   <- read_csv(matches_path, show_col_types = FALSE)
abstracts <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                      show_col_types = FALSE)

gender_col <- if ("gender_unified" %in% names(matches)) "gender_unified" else "first_author_gender"
no_gender <- matches |>
  filter(is.na(.data[[gender_col]])) |>
  select(abstract_id, congress_year) |>
  left_join(abstracts |> select(abstract_id, author_name_first), by = "abstract_id") |>
  filter(!is.na(author_name_first), nchar(author_name_first) > 2) |>
  mutate(
    fa_last  = str_extract(author_name_first, "[A-Za-z]+$"),
    fa_init1 = str_extract(str_remove_all(author_name_first, "[A-Za-z]+$"), "[A-Za-z]"),
    congress_year = as.integer(congress_year)
  ) |>
  filter(!is.na(fa_last), !is.na(fa_init1)) |>
  inner_join(aagl_dates, by = "congress_year") |>
  distinct(fa_last, fa_init1, congress_year, .keep_all = TRUE)

cli_alert_info("{nrow(no_gender)} unique author-year pairs to query (2012 + 2015 excluded)")

# ── Open DuckDB connection ─────────────────────────────────────────────────────
con <- dbConnect(duckdb(), DUCKDB_PATH, read_only = TRUE)
on.exit(dbDisconnect(con), add = TRUE)

# ── Main loop ──────────────────────────────────────────────────────────────────
n       <- nrow(no_gender)
results <- vector("list", n)
cli_progress_bar("Open Payments search", total = n)

for (i in seq_len(n)) {
  row      <- no_gender[i, ]
  conf_row <- aagl_dates[aagl_dates$congress_year == row$congress_year, ]
  res <- tryCatch(
    fetch_op_first_name(row$fa_last, row$fa_init1, row$congress_year, conf_row, con),
    error = function(e) list(first_name = NA_character_, hits = data.frame())
  )
  results[[i]] <- tibble(
    abstract_id        = row$abstract_id,
    author_recorded    = row$author_name_first,
    congress_year      = row$congress_year,
    op_first_name      = res$first_name,
    op_n_hits          = nrow(res$hits),
    op_payment_types   = if (nrow(res$hits) > 0)
                           paste(unique(res$hits$payment_type), collapse = "; ")
                         else NA_character_,
    op_payer           = if (nrow(res$hits) > 0 && "Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name" %in% names(res$hits))
                           paste(unique(res$hits$Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name), collapse = "; ")
                         else NA_character_
  )
  cli_progress_update()
}
cli_progress_done()

all_results <- bind_rows(results)
name_tbl    <- all_results |> filter(!is.na(op_first_name), nchar(op_first_name) >= 2)

cli_alert_success("First names recovered: {nrow(name_tbl)} / {n} ({round(nrow(name_tbl)/n*100,1)}%)")
if (nrow(name_tbl) > 0) {
  cli_alert_info("Payment types seen:")
  print(table(unlist(str_split(na.omit(name_tbl$op_payment_types), "; "))))
}

if (nrow(name_tbl) == 0) {
  cli_alert_warning("No names recovered — skipping gender inference")
  write_csv(all_results, out_path)
  invisible(NULL)
} else {

# ── Gender inference ───────────────────────────────────────────────────────────
unique_names <- unique(name_tbl$op_first_name)

ssa_result <- tryCatch(
  gender::gender(unique_names, years = c(1930, 2012), method = "ssa") |>
    transmute(op_first_name = name, gender, proportion_male, proportion_female),
  error = function(e) { cli_alert_warning("SSA: {e$message}"); tibble() }
)
cli_alert_info("SSA resolved: {nrow(ssa_result)} / {length(unique_names)}")

ssa_resolved <- if (nrow(ssa_result) > 0) tolower(ssa_result$op_first_name) else character()
unresolved   <- unique_names[!tolower(unique_names) %in% ssa_resolved]

genderize_result <- tibble()
if (length(unresolved) > 0) {
  cli_alert_info("genderize.io for {length(unresolved)} names...")
  api_key <- Sys.getenv("GENDERIZE_API_KEY", "")

  fetch_batch <- function(batch) {
    params <- paste0("name[]=", URLencode(batch, repeated = TRUE), collapse = "&")
    if (nchar(api_key) > 0) params <- paste0(params, "&apikey=", api_key)
    resp <- tryCatch(httr::GET(paste0("https://api.genderize.io/?", params), httr::timeout(15)),
                     error = function(e) NULL)
    if (is.null(resp) || httr::status_code(resp) != 200) return(tibble())
    parsed <- tryCatch(httr::content(resp, "parsed"), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0) return(tibble())
    map(parsed, function(x) tibble(
      op_first_name     = x$name    %||% NA_character_,
      gender            = x$gender  %||% NA_character_,
      proportion_male   = if (identical(x$gender, "male"))
                            x$probability %||% NA_real_ else 1 - (x$probability %||% NA_real_),
      proportion_female = if (identical(x$gender, "female"))
                            x$probability %||% NA_real_ else 1 - (x$probability %||% NA_real_)
    )) |> list_rbind()
  }

  batches <- split(unresolved, ceiling(seq_along(unresolved) / 10))
  gz_raw  <- map(batches, ~ { Sys.sleep(0.5); tryCatch(fetch_batch(.x), error = function(e) tibble()) }) |>
    list_rbind()
  genderize_result <- if (nrow(gz_raw) > 0 && "op_first_name" %in% names(gz_raw))
    gz_raw |> filter(!is.na(op_first_name), !is.na(gender)) else tibble()
  cli_alert_info("genderize.io resolved: {nrow(genderize_result)} / {length(unresolved)}")
}

gender_lkp <- bind_rows(ssa_result, genderize_result) |>
  group_by(op_first_name) |> slice(1) |> ungroup() |>
  mutate(
    op_gender   = gender,
    op_gender_p = pmax(proportion_male, proportion_female, na.rm = TRUE)
  ) |>
  select(op_first_name, op_gender, op_gender_p)

gender_tbl <- name_tbl |>
  left_join(gender_lkp, by = "op_first_name") |>
  filter(!is.na(op_gender))

cli_alert_success("Gender resolved: {nrow(gender_tbl)} abstracts")
print(table(gender_tbl$op_gender, useNA = "ifany"))

write_csv(gender_tbl |> select(abstract_id, author_recorded, congress_year,
                                op_first_name, op_gender, op_gender_p,
                                op_n_hits, op_payment_types), out_path)
cli_alert_success("Wrote {out_path}")

# ── Show examples ──────────────────────────────────────────────────────────────
cli_alert_info("Example matches:")
print(as.data.frame(
  gender_tbl |>
    select(author_recorded, op_first_name, op_gender, op_gender_p,
           op_n_hits, op_payment_types) |>
    head(15)
), row.names = FALSE)

# NOTE: Merge into abstracts_with_matches.csv is handled by 10e_merge_demographics.R
# This script only writes its sidecar CSV (gender_from_open_payments.csv).

} # end nrow(name_tbl) > 0
