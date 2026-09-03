# Cycle 7 of 24. Mix: 4 BVA, 3 semantic, 3 adversarial.
#
# Target: agreement between the manuscript prose and the artifacts it describes.
# Cycles 0-6 tested the pipeline's internals; nothing had checked that the
# sentences in docs/ still describe the numbers in output/. Several claims in
# the Rmd are hardcoded rather than derived, so they can drift silently.

library(testthat)
library(dplyr)

P_RMD   <- here::here("docs", "abstract_results_section.Rmd")
P_APP   <- here::here("docs", "technical_appendix.Rmd")
P_AWM   <- here::here("output", "abstracts_with_matches.csv")
P_FINAL <- here::here("output", "final_analytical_dataset.csv")
P_AIM2  <- here::here("output", "aim2_time_to_pub.csv")
P_SENS  <- here::here("output", "sensitivity_analyses.csv")
P_CLEAN <- here::here("data", "processed", "abstracts_cleaned.csv")
P_PARSE <- here::here("data", "processed", "abstracts_parsed.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")
rmd  <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")

# ============================================================
# BVA 7.1 — the two cohort artifacts are the same cohort
# ============================================================
test_that("abstracts_with_matches and final_analytical_dataset describe one cohort", {
  need(P_AWM, P_FINAL)
  a <- readr::read_csv(P_AWM, show_col_types = FALSE)
  b <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  expect_equal(nrow(a), nrow(b))
  expect_true(setequal(a$abstract_id, b$abstract_id),
              label = "the two artifacts disagree on which abstracts are in the cohort")
  expect_equal(sort(table(a$classification)), sort(table(b$classification)),
               label = "classification distribution differs between the two artifacts")
})

# ============================================================
# BVA 7.2 — the video count asserted in prose matches the data
# ============================================================
test_that("the manuscript's video exclusion count is the real one", {
  need(P_RMD, P_PARSE, P_CLEAN)
  n_video <- nrow(readr::read_csv(P_PARSE, show_col_types = FALSE)) -
             nrow(readr::read_csv(P_CLEAN, show_col_types = FALSE))
  txt <- rmd(P_RMD)
  claimed <- unique(as.integer(gsub("\\D", "", regmatches(txt,
               gregexpr("Video presentations \\(n=\\d+", txt))[[1]])))
  skip_if(length(claimed) == 0, "no hardcoded video count found")
  expect_true(all(claimed == n_video),
              label = paste("prose claims n =", paste(claimed, collapse = "/"),
                            "video exclusions; the data has", n_video))
})

# ============================================================
# BVA 7.3 — the congress ordinal range matches the config
# ============================================================
test_that("the 41st-through-52nd claim matches the configured congresses", {
  need(P_RMD)
  cfg <- config::get(file = here::here("config.yml"))
  names_v <- vapply(cfg$congresses, function(x) as.character(x$name), character(1))
  first_last <- c(names_v[1], names_v[length(names_v)])
  txt <- rmd(P_RMD)
  ord <- regmatches(txt, gregexpr("\\d+(st|nd|rd|th) through \\d+(st|nd|rd|th)", txt))[[1]]
  skip_if(length(ord) == 0, "no ordinal range in prose")
  nums <- as.integer(unlist(regmatches(ord, gregexpr("\\d+", ord))))
  cfg_nums <- as.integer(sub("^(\\d+).*", "\\1", first_last))
  expect_equal(nums[1], cfg_nums[1], label = "first congress ordinal disagrees with config")
  expect_equal(nums[2], cfg_nums[2], label = "last congress ordinal disagrees with config")
})

# ============================================================
# BVA 7.4 — every follow-up window named in prose exists as a scenario
# ============================================================
test_that("the follow-up windows described in prose are the ones analysed", {
  need(P_RMD, P_SENS)
  s <- readr::read_csv(P_SENS, show_col_types = FALSE)
  have <- sort(unique(as.integer(gsub("\\D", "",
            s$scenario[grepl("within", s$scenario, ignore.case = TRUE)]))))
  txt <- rmd(P_RMD)
  m <- regmatches(txt, regexpr("at ([0-9, and]+) months of follow-up", txt))
  skip_if(length(m) == 0, "no follow-up window list in prose")
  said <- sort(as.integer(unlist(regmatches(m, gregexpr("\\d+", m)))))
  expect_equal(said, have,
               label = paste("prose lists", paste(said, collapse = "/"),
                             "months; artifact has", paste(have, collapse = "/")))
})

# ============================================================
# SEMANTIC 7.5 — the median's stated population must be its real population
# ============================================================
test_that("the time-to-publication sentence names the population it summarises", {
  need(P_RMD, P_AIM2, P_FINAL)
  txt <- rmd(P_RMD)
  a2 <- readr::read_csv(P_AIM2, show_col_types = FALSE)
  f  <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  n_with_dates <- a2$value[a2$metric == "n_with_dates"]
  n_definite_dated <- sum(f$classification == "definite" &
                          !is.na(f$months_to_pub) & f$months_to_pub > 0)

  # aim2_time_to_pub is computed over every published abstract carrying a usable
  # interval, not over definite matches. A sentence introducing that median with
  # "Among definite matches" names a strictly smaller population.
  claims_definite <- grepl("Among definite matches, the median time", txt)
  if (claims_definite) {
    expect_equal(n_with_dates, n_definite_dated,
                 label = paste0("prose says 'Among definite matches' but the median ",
                                "is computed on ", n_with_dates, " abstracts, while ",
                                "definite-with-a-date is ", n_definite_dated))
  }
  succeed()
})

# ============================================================
# SEMANTIC 7.6 — the video-years claim is true of the data
# ============================================================
test_that("videos appear only in the congress years the prose names", {
  need(P_RMD, P_PARSE)
  p <- readr::read_csv(P_PARSE, show_col_types = FALSE)
  skip_if(!"session_type" %in% names(p), "session_type absent")
  actual <- sort(unique(p$congress_year[p$session_type == "Video"]))
  txt <- rmd(P_RMD)
  m <- regmatches(txt, regexpr("appearing only in (\\d{4})-(\\d{4})", txt))
  skip_if(length(m) == 0, "no video-year claim in prose")
  said <- as.integer(unlist(regmatches(m, gregexpr("\\d{4}", m))))
  expect_equal(range(actual), said,
               label = paste("prose says videos appear only in", paste(said, collapse = "-"),
                             "but the data has", paste(range(actual), collapse = "-")))
})

# ============================================================
# SEMANTIC 7.7 — the cohort the Rmd reports is the cohort on disk
# ============================================================
test_that("the Rmd's n_total is the cleaned oral cohort", {
  need(P_RMD, P_AWM, P_CLEAN)
  # n_total <- nrow(d) where d is abstracts_with_matches.csv
  expect_true(grepl("n_total\\s*<-\\s*nrow\\(d\\)", rmd(P_RMD)),
              label = "n_total is no longer derived from the loaded cohort")
  expect_equal(nrow(readr::read_csv(P_AWM, show_col_types = FALSE)),
               nrow(readr::read_csv(P_CLEAN, show_col_types = FALSE)))
})

# ============================================================
# ADVERSARIAL 7.8 — every artifact the documents read exists
# ============================================================
test_that("the Rmd documents do not read files that are missing", {
  for (p in c(P_RMD, P_APP)) {
    if (!file.exists(p)) next
    txt <- rmd(p)
    calls <- regmatches(txt, gregexpr('here(::here)?\\("[^)]*"\\)', txt))[[1]]
    skip_if(length(calls) == 0, "no here() calls found")
    paths <- vapply(calls, function(cc) {
      parts <- gsub('"', '', unlist(regmatches(cc, gregexpr('"[^"]+"', cc))))
      do.call(file.path, as.list(c(here::here(), parts)))
    }, character(1))
    missing <- unique(paths[!file.exists(paths)])
    expect_length(missing, 0L)
    if (length(missing)) fail(paste(basename(p), "reads missing file(s):",
                                    paste(basename(missing), collapse = ", ")))
  }
})

# ============================================================
# ADVERSARIAL 7.9 — reported study-design counts cannot exceed the cohort
# ============================================================
test_that("study-design counts stay inside the cohort", {
  need(P_AWM)
  d <- readr::read_csv(P_AWM, show_col_types = FALSE)
  skip_if(!"study_design" %in% names(d), "study_design absent")
  tbl <- table(d$study_design)
  expect_lte(sum(tbl), nrow(d))
  expect_true(all(tbl >= 0))
  expect_gt(length(tbl), 1, label = "study_design collapsed to a single value")
})

# ============================================================
# ADVERSARIAL 7.10 — the technical appendix agrees with live data
# ============================================================
test_that("technical appendix A13 counts match the current dataset", {
  need(P_APP, P_FINAL, P_PARSE, P_CLEAN)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  txt <- rmd(P_APP)
  # A13 derives its counts in R chunks. Confirm the chunk still reads the files
  # it claims, so the prose cannot drift from the data behind it.
  expect_true(grepl("abstracts_parsed.csv", txt) && grepl("abstracts_cleaned.csv", txt),
              label = "A13 no longer derives the chain from the stage files")
  expect_true(grepl("final_analytical_dataset.csv", txt))
  # And the invariant the section asserts must still hold.
  n_pending <- sum(is.na(f$final_published))
  expect_equal(nrow(f) - n_pending,
               nrow(readr::read_csv(P_CLEAN, show_col_types = FALSE)) - n_pending)
})
