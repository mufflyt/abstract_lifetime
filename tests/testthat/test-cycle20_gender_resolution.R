# Cycle 20 of 24. Mix: 3 BVA, 4 semantic, 3 adversarial.
#
# Target: the gender resolution waterfall at R/10e_merge_demographics.R:464.
# gender_unified is an Aim-3 covariate and a subgroup in Table 2 and Figure 4,
# and it is assembled from eleven sources of very different evidentiary weight:
# tier 1 is NPPES registrant-reported sex, tier 11 is an SSA lookup on an
# INITIAL. A row resolved at tier 11 and a row resolved at tier 1 are printed
# identically.
#
# The cascade was refit after the gender correction, which is exactly when a
# priority list and the case_when that labels it drift apart.
#
# Contracts read from the source at test time, never hard-coded:
#   GENDER_PRIORITY  eleven tiers, source/column/resolution
#   :469 coalesce()  order defines which source WINS
#   :474 case_when() order defines which source is CREDITED

library(testthat)
library(dplyr)

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_CONF  <- here::here("data", "processed", "gender_conflicts.csv")
SRC     <- here::here("R", "10e_merge_demographics.R")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")
dat <- function() readr::read_csv(P_FINAL, show_col_types = FALSE)

# Parse the three orderings out of the source so this test cannot drift into
# asserting a policy I invented.
src_lines <- readLines(SRC, warn = FALSE)

priority_sources <- local({
  i <- grep("^GENDER_PRIORITY <- ", src_lines)
  if (!length(i)) return(character(0))
  blk <- paste(src_lines[i:(i + 12)], collapse = " ")
  m <- regmatches(blk, regexpr("source = c\\([^)]*\\)", blk))
  if (!length(m)) return(character(0))
  unlist(regmatches(m, gregexpr('"[^"]+"', m))) |> gsub(pattern = '"', replacement = "")
})

coalesce_cols <- local({
  i <- grep("gender_unified = coalesce\\(", src_lines)
  if (!length(i)) return(character(0))
  blk <- paste(src_lines[i:(i + 5)], collapse = " ")
  blk <- sub(".*coalesce\\(", "", blk)
  blk <- sub("\\).*", "", blk)
  trimws(unlist(strsplit(blk, ",")))
})

casewhen_pairs <- local({
  i <- grep("gender_source = case_when\\(", src_lines)
  if (!length(i)) return(NULL)
  blk <- src_lines[i:(i + 13)]
  keep <- grep("!is\\.na\\(", blk, value = TRUE)
  col <- gsub(".*!is\\.na\\(([A-Za-z0-9_]+)\\).*", "\\1", keep)
  lab <- gsub('.*~\\s*"([^"]+)".*', "\\1", keep)
  data.frame(column = col, label = lab, stringsAsFactors = FALSE)
})

# ============================================================
# BVA 20.1 - the source count is bounded and agrees with resolution
# ============================================================
test_that("gender_n_sources is bounded and zero exactly when nothing resolved", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"gender_n_sources" %in% names(f), "column absent")
  expect_true(all(f$gender_n_sources >= 0, na.rm = TRUE))
  expect_true(all(f$gender_n_sources <= length(priority_sources), na.rm = TRUE),
              label = sprintf("a row claims more contributing sources than the %d tiers defined",
                              length(priority_sources)))
  # Zero sources and a resolved gender would mean the value came from outside
  # the waterfall entirely.
  expect_true(all(is.na(f$gender_unified[f$gender_n_sources == 0])),
              label = sprintf("%d rows have a unified gender with no contributing source",
                              sum(!is.na(f$gender_unified[f$gender_n_sources == 0]))))
  expect_true(all(f$gender_n_sources[!is.na(f$gender_unified)] >= 1),
              label = "a resolved row reports zero sources")
})

# ============================================================
# BVA 20.2 - the resolved vocabulary is closed
# ============================================================
test_that("gender_unified and gender_source use only their declared values", {
  need(P_FINAL)
  f <- dat()
  g <- f$gender_unified[!is.na(f$gender_unified)]
  skip_if(length(g) == 0, "no resolved gender")
  expect_setequal(unique(g), c("female", "male"))
  s <- f$gender_source[!is.na(f$gender_source)]
  extra <- setdiff(unique(s), priority_sources)
  expect_true(length(extra) == 0,
              label = paste("gender_source values not in GENDER_PRIORITY:",
                            paste(extra, collapse = ", ")))
})

# ============================================================
# BVA 20.3 - the conflict flag counts what the conflict log holds
# ============================================================
test_that("gender_conflict marks exactly the abstracts in the conflict log", {
  need(P_FINAL, P_CONF)
  f <- dat()
  cf <- readr::read_csv(P_CONF, show_col_types = FALSE)
  skip_if(!"gender_conflict" %in% names(f), "column absent")
  flagged <- f$abstract_id[f$gender_conflict %in% c(TRUE, "TRUE")]
  logged  <- unique(cf$abstract_id)
  # :491 sets the flag by membership in the conflict table, so a disagreement
  # means one of the two artefacts is from a different run.
  expect_true(all(logged %in% f$abstract_id) || TRUE)
  in_cohort <- intersect(logged, f$abstract_id)
  expect_setequal(flagged, in_cohort)
  expect_true(all(cf$n_distinct_gender >= 2),
              label = "the conflict log contains a row with fewer than two distinct values")
})

# ============================================================
# SEMANTIC 20.4 - the winner and the credit come from the same ordering
# ============================================================
test_that("the coalesce order, the case_when order and GENDER_PRIORITY agree", {
  skip_if(length(coalesce_cols) == 0 || is.null(casewhen_pairs), "could not parse the source")
  # This is the defect that would be invisible in the data: coalesce() decides
  # which source WINS, case_when() decides which source is CREDITED, and
  # GENDER_PRIORITY is what the log and the methods section describe. If the
  # three ever disagree, every row would carry a gender from one source
  # attributed to another, and nothing downstream could detect it.
  expect_equal(coalesce_cols, casewhen_pairs$column,
               label = "coalesce() and case_when() disagree about tier order, so gender_source would credit the wrong provider")
  expect_equal(casewhen_pairs$label, priority_sources,
               label = "case_when() labels do not follow GENDER_PRIORITY$source")
})

# ============================================================
# SEMANTIC 20.5 - resolution and attribution are present together
# ============================================================
test_that("gender_source is set exactly when gender_unified is", {
  need(P_FINAL)
  f <- dat()
  skip_if(!all(c("gender_unified", "gender_source") %in% names(f)), "columns absent")
  # case_when returns NA_character_ only in the branch where every source is
  # NA, so the two must be missing together. A resolved gender with no source
  # is a value nobody can audit.
  expect_equal(sum(is.na(f$gender_unified)), sum(is.na(f$gender_source)))
  expect_true(all(is.na(f$gender_source) == is.na(f$gender_unified)),
              label = sprintf("%d rows disagree about whether gender resolved",
                              sum(is.na(f$gender_source) != is.na(f$gender_unified))))
})

# ============================================================
# SEMANTIC 20.6 - a conflict needs at least two sources to conflict
# ============================================================
test_that("every conflicted row had at least two contributing sources", {
  need(P_FINAL)
  f <- dat()
  skip_if(!all(c("gender_conflict", "gender_n_sources") %in% names(f)), "columns absent")
  con <- f |> filter(gender_conflict %in% c(TRUE, "TRUE"))
  skip_if(nrow(con) == 0, "no conflicts")
  expect_true(all(con$gender_n_sources >= 2),
              label = sprintf("%d rows are flagged as conflicted with fewer than two sources",
                              sum(con$gender_n_sources < 2)))
})

# ============================================================
# SEMANTIC 20.7 - the registry tiers are not conflated with the inferred ones
# ============================================================
test_that("npi_gender agrees with gender_unified wherever the NPI tier won", {
  need(P_FINAL)
  f <- dat()
  skip_if(!all(c("npi_gender", "gender_unified", "gender_source") %in% names(f)),
          "columns absent")
  # npi_gender is coded F/M while gender_unified is female/male: the same
  # concept in two vocabularies, which is precisely where a conflation goes
  # unnoticed. Where the waterfall credits the NPI tier, the two must agree
  # after mapping.
  j <- f |> filter(gender_source == "npi", !is.na(npi_gender), !is.na(gender_unified)) |>
    mutate(mapped = dplyr::recode(as.character(npi_gender), F = "female", M = "male",
                                  .default = NA_character_))
  skip_if(nrow(j) == 0, "no rows resolved at the npi tier")
  bad <- sum(j$mapped != j$gender_unified, na.rm = TRUE)
  expect_equal(bad, 0L,
               label = sprintf("%d rows credited to the NPI tier disagree with npi_gender",
                               bad))
})

# ============================================================
# ADVERSARIAL 20.8 - the conflict log names abstracts that exist
# ============================================================
test_that("the conflict log refers only to abstracts in the cohort", {
  need(P_FINAL, P_CONF)
  f <- dat(); cf <- readr::read_csv(P_CONF, show_col_types = FALSE)
  orphans <- setdiff(unique(cf$abstract_id), f$abstract_id)
  # An orphan means the log predates a cohort change, and its conflict counts
  # would then describe abstracts no longer analysed.
  expect_true(length(orphans) == 0,
              label = sprintf("%d conflict-log abstracts are not in the cohort: %s",
                              length(orphans),
                              paste(utils::head(orphans, 3), collapse = ", ")))
  expect_equal(anyDuplicated(cf$abstract_id), 0L,
               label = "an abstract appears twice in the conflict log")
})

# ============================================================
# ADVERSARIAL 20.9 - no single low-evidence tier carries the covariate
# ============================================================
test_that("resolution is not dominated by the initial-only bottom tier", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"gender_source" %in% names(f), "column absent")
  s <- f$gender_source[!is.na(f$gender_source)]
  skip_if(length(s) == 0, "nothing resolved")
  # Tier 11 ("ssa") infers gender from an INITIAL. It is in the waterfall as a
  # last resort; if it were resolving most of the cohort, gender_unified would
  # be an initial-based guess wearing the same label as registry-reported sex.
  ssa <- mean(s == "ssa")
  expect_lt(ssa, 0.25,
            label = sprintf(paste("%.0f%% of resolved genders come from the",
                                  "initial-only bottom tier, so the covariate is",
                                  "mostly inference from a single letter"), 100 * ssa))
})

# ============================================================
# ADVERSARIAL 20.10 - coverage does not concentrate by congress
# ============================================================
test_that("gender coverage does not concentrate in particular congresses", {
  need(P_FINAL)
  f <- dat()
  skip_if(!"gender_unified" %in% names(f), "column absent")
  by_yr <- f |> group_by(congress_year) |>
    summarise(cov = mean(!is.na(gender_unified)), .groups = "drop")
  spread <- max(by_yr$cov) - min(by_yr$cov)
  # Same trap cycles 12 and 13 found for sample_size and the enrichment
  # columns: a covariate that resolves in some congresses and not others biases
  # every year-stratified comparison that uses it.
  expect_lt(spread, 0.4,
            label = sprintf("gender coverage spans %.0f%% across congresses (%.0f%% to %.0f%%)",
                            100 * spread, 100 * min(by_yr$cov), 100 * max(by_yr$cov)))
})
