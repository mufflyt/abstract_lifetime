# Cycle 19 of 24. Mix: 4 BVA, 3 semantic, 3 adversarial.
#
# Target: congress date resolution, R/utils_congresses.R. Every
# time-to-publication quantity in the study is measured FROM a congress date:
# months_to_pub at 06_analyze_results.R:97, the survival censoring time at :467,
# the figures at 08_make_figures.R:156, and the date component of the match
# score at utils_scoring.R:208. If a row resolves to the wrong congress date,
# it does not error, it just measures from the wrong origin, and the abstract
# still appears in every table.
#
# Cycle 1 checked that every congress year has a config date. This cycle is
# about the resolution FUNCTION and, specifically, its fallback.
#
# Contracts read from the source:
#   utils_congresses.R:36  lookup is built from cfg$congresses year/date pairs
#   utils_congresses.R:71  an unrecognised or NA year silently receives
#                          cfg$conference$date, defaulting to 2023-11-07

library(testthat)
library(dplyr)

source(here::here("R", "utils_congresses.R"))
CFG <- config::get(file = here::here("config.yml"))
P_FINAL <- here::here("output", "final_analytical_dataset.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

# ============================================================
# BVA 19.1 - the lookup covers every congress the config declares
# ============================================================
test_that("the date lookup is one entry per configured congress, all valid", {
  lkp <- congress_date_lookup(CFG)
  expect_equal(length(lkp), length(CFG$congresses))
  expect_true(all(!is.na(lkp)), label = "a congress date failed to parse")
  expect_s3_class(lkp, "Date")
  expect_equal(anyDuplicated(names(lkp)), 0L,
               label = "two config entries claim the same congress year")
  # The date recorded for a congress must fall in that congress's own year.
  yrs <- as.integer(names(lkp))
  expect_true(all(as.integer(format(lkp, "%Y")) == yrs),
              label = paste("congress date(s) not in their own year:",
                            paste(names(lkp)[as.integer(format(lkp, "%Y")) != yrs],
                                  collapse = ", ")))
})

# ============================================================
# BVA 19.2 - resolution is exact at every configured year
# ============================================================
test_that("every configured year resolves to its own date, one row per input", {
  lkp <- congress_date_lookup(CFG)
  yrs <- as.integer(names(lkp))
  got <- conference_date_for(yrs, CFG)
  expect_equal(length(got), length(yrs), label = "the lookup is not length-preserving")
  expect_equal(unname(got), unname(lkp[as.character(yrs)]))
  # Order-invariance: a vectorised lookup that depended on input order would
  # silently mismatch rows to dates when the dataset is sorted differently.
  rev_got <- conference_date_for(rev(yrs), CFG)
  expect_equal(unname(rev_got), rev(unname(got)))
})

# ============================================================
# BVA 19.3 - the empty and single-element boundaries
# ============================================================
test_that("the lookup handles zero-length and single-element input", {
  expect_length(conference_date_for(integer(0), CFG), 0L)
  one <- conference_date_for(2015L, CFG)
  expect_length(one, 1L)
  expect_equal(as.character(one), "2015-11-15")
  # Character and integer years must agree: congress_year arrives as either
  # depending on which artefact it was read from.
  expect_equal(conference_date_for("2015", CFG), conference_date_for(2015L, CFG))
})

# ============================================================
# BVA 19.4 - an unknown year does not error, which is the hazard
# ============================================================
test_that("an unrecognised or missing year silently receives the legacy date", {
  legacy <- as.Date(CFG$conference$date %||% "2023-11-07")
  # This is the documented behaviour at utils_congresses.R:71, asserted so the
  # blast radius is written down rather than discovered. An abstract with a
  # missing or out-of-range congress year is NOT dropped and does NOT error: it
  # is measured from the legacy date, so its months_to_pub, its censoring time
  # and its date score are all computed against the wrong origin while it stays
  # in every table.
  expect_equal(conference_date_for(NA_integer_, CFG), legacy)
  expect_equal(conference_date_for(1999L, CFG), legacy)
  expect_equal(conference_date_for(2099L, CFG), legacy)
  mixed <- conference_date_for(c(2012L, NA_integer_, 2030L), CFG)
  expect_equal(mixed[1], as.Date("2012-11-06"))
  expect_equal(mixed[2], legacy)
  expect_equal(mixed[3], legacy)
})

# ============================================================
# SEMANTIC 19.5 - no row in the cohort actually hits the fallback
# ============================================================
test_that("every congress year in the data is one the config knows", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  known <- names(congress_date_lookup(CFG))
  seen <- unique(as.character(f$congress_year))
  unknown <- setdiff(seen, known)
  # 19.4 establishes that an unknown year is silently mis-dated rather than
  # rejected. This is the check that none is present, which is the only reason
  # that fallback is currently harmless.
  expect_true(length(unknown) == 0,
              label = paste("congress year(s) in the dataset with no configured",
                            "date, silently measured from the legacy date:",
                            paste(unknown, collapse = ", ")))
  expect_true(!any(is.na(f$congress_year)),
              label = sprintf("%d rows have no congress year at all",
                              sum(is.na(f$congress_year))))
})

# ============================================================
# SEMANTIC 19.6 - congress dates advance with congress year
# ============================================================
test_that("the congress dates are strictly increasing in year", {
  lkp <- congress_date_lookup(CFG)
  o <- order(as.integer(names(lkp)))
  d <- unname(lkp[o])
  expect_true(all(diff(as.numeric(d)) > 0),
              label = "congress dates are not strictly increasing with year")
  # AAGL meets annually in the autumn. A gap far from a year apart means a
  # transcription error in a date that anchors a whole congress's follow-up.
  gaps <- as.numeric(diff(d))
  expect_true(all(gaps > 300 & gaps < 430),
              label = sprintf("congress-to-congress gap(s) outside 300-430 days: %s",
                              paste(round(gaps[gaps <= 300 | gaps >= 430]), collapse = ", ")))
  expect_true(all(as.integer(format(d, "%m")) == 11),
              label = "a congress is dated outside November")
})

# ============================================================
# SEMANTIC 19.7 - the censoring horizon is after every congress
# ============================================================
test_that("the search end date lies after the last congress it censors at", {
  lkp <- congress_date_lookup(CFG)
  end <- as.Date(CFG$pubmed$date_end, "%Y/%m/%d")
  skip_if(is.na(end), "date_end not parseable with the format 06 uses")
  # 06_analyze_results.R:467 computes censor_time as date_end minus the congress
  # date. If date_end preceded a congress, that abstract's censoring time would
  # be negative and it would enter the survival model as a subject observed for
  # less than no time.
  expect_true(all(end > lkp),
              label = paste("the PubMed search end date precedes congress(es):",
                            paste(names(lkp)[end <= lkp], collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 19.8 - measured times reconcile with the resolved dates
# ============================================================
test_that("no publication event falls outside its own censoring horizon", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  skip_if(!all(c("months_to_pub", "congress_year") %in% names(f)), "columns absent")
  end <- as.Date(CFG$pubmed$date_end, "%Y/%m/%d")
  skip_if(is.na(end), "date_end not parseable with the format 06 uses")

  d <- f |> filter(!is.na(months_to_pub)) |>
    mutate(cdate  = conference_date_for(congress_year, CFG),
           censor = as.numeric(difftime(end, cdate, units = "days")) / 30.44,
           over   = months_to_pub - censor)
  skip_if(nrow(d) == 0, "no publication times")

  # 06_analyze_results.R:467 censors non-events at date_end minus the congress
  # date. An EVENT recorded after that same horizon is internally inconsistent:
  # the subject is credited with an event at a time when an identical
  # unpublished subject would already have left the risk set.
  #
  # My first version of this test used an arbitrary 45-day tolerance past
  # date_end, which is not a contract anyone stated. This is: whatever the
  # horizon is, an event cannot land beyond it.
  bad <- d |> filter(over > 0)
  expect_equal(nrow(bad), 0L,
               label = paste0(
                 nrow(bad), " of ", nrow(d), " publication events fall beyond ",
                 "their own censoring horizon: ",
                 paste(sprintf("%s (event %.1f mo, horizon %.1f mo)",
                               bad$abstract_id, bad$months_to_pub, bad$censor),
                       collapse = "; "),
                 ". The cause is that months_to_pub is measured to the PRINT ",
                 "issue date while the search matched on an earlier e-pub date, ",
                 "so an article can carry a 2026-08 issue date inside a search ",
                 "that ended 2026-04-01."))

  # Negative times are NOT an error here: 42 abstracts were published before
  # their congress, which is the pre-congress group the study exists to count.
  expect_true(any(d$months_to_pub < 0),
              label = "no pre-congress publications at all, which would itself be suspect")
})

# ============================================================
# ADVERSARIAL 19.9 - the lookup does not depend on config ordering
# ============================================================
test_that("shuffling the config's congress list does not change resolution", {
  shuffled <- CFG
  set.seed(19)
  shuffled$congresses <- CFG$congresses[sample(length(CFG$congresses))]
  yrs <- as.integer(names(congress_date_lookup(CFG)))
  # setNames over a vapply is order-sensitive by construction, so this is a
  # real risk rather than a hypothetical one: if resolution ever became
  # positional, every congress would silently take its neighbour's date.
  expect_equal(conference_date_for(yrs, shuffled),
               conference_date_for(yrs, CFG))
})

# ============================================================
# ADVERSARIAL 19.10 - config dates agree with the supplement they describe
# ============================================================
test_that("each congress entry is internally consistent about its year", {
  bad <- character(0)
  for (c in CFG$congresses) {
    y <- as.integer(c$year)
    if (!is.null(c$supplement_year) && as.integer(c$supplement_year) != y) {
      bad <- c(bad, sprintf("%d supplement_year=%s", y, c$supplement_year))
    }
    # doi_prefix embeds the year; a mismatch means the DOI chain search for that
    # congress queries the wrong volume.
    if (!is.null(c$doi_prefix) && !grepl(as.character(y), c$doi_prefix, fixed = TRUE)) {
      bad <- c(bad, sprintf("%d doi_prefix=%s", y, c$doi_prefix))
    }
  }
  expect_true(length(bad) == 0,
              label = paste("congress config entries inconsistent about their year:",
                            paste(bad, collapse = "; ")))
})
