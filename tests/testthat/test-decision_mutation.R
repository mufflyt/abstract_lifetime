# Mutation tests for adjudication precedence and denominator arithmetic.
#
# A test suite that passes tells you nothing on its own: it may assert nothing
# that matters. Each mutant below is a defect that actually reached the analysis
# outputs, or a near neighbour of one. Every mutant must be KILLED, meaning at
# least one invariant detects it. A surviving mutant is a hole in the suite and
# fails this file.
#
# To add a rule to R/utils_decisions.R, add the mutant that breaks it here.

library(testthat)
library(dplyr)
source(here::here("R", "utils_decisions.R"))

ts  <- function(s) as.POSIXct(s, tz = "UTC")
dec <- function(id, reviewer, decision, when) {
  tibble::tibble(abstract_id = id, reviewer = reviewer,
                 manual_decision = decision, manual_pmid = NA_character_,
                 review_timestamp = ts(when))
}
res <- function(id, classification) {
  tibble::tibble(abstract_id = id, classification = classification,
                 best_pmid = NA_character_)
}

# ------------------------------------------------------------
# Fixture exercising every branch and both precedence directions
# ------------------------------------------------------------
fixture <- list(
  results = bind_rows(
    res("A1", "definite"),   # human said no_match; branch 1 still wins
    res("A2", "probable"),   # human match
    res("A3", "probable"),   # human skip -> NA
    res("A4", "possible"),   # AUTO only -> keeps AUTO's no_match
    res("A5", "no_match"),   # no reviewer
    res("A6", "excluded"),   # human match (pre-congress override)
    res("A7", "probable"),   # AUTO newer than human: human must still win
    res("A8", "probable"),   # two human rows; the LATER one must win
    res("A9", "excluded")    # no reviewer at all; must resolve FALSE, not NA
  ),
  decisions = bind_rows(
    dec("A1", "GW",   "no_match", "2026-04-20 10:00:00"),
    dec("A2", "GW",   "match",    "2026-04-20 10:00:00"),
    dec("A3", "GW",   "skip",     "2026-04-20 10:00:00"),
    dec("A4", "AUTO", "no_match", "2026-04-17 12:00:00"),
    dec("A6", "JM",   "match",    "2026-04-21 10:00:00"),
    dec("A7", "GW",   "match",    "2026-04-14 09:00:00"),
    dec("A7", "AUTO", "no_match", "2099-01-01 00:00:00"),
    dec("A8", "GW",   "no_match", "2026-04-18 10:00:00"),
    dec("A8", "JM",   "match",    "2026-04-22 10:00:00")
  )
)

# The invariant battery. Returns the names of invariants that FAILED.
check_invariants <- function(dedup_fn, assign_fn, summary_fn) {
  failures <- character(0)
  note <- function(cond, nm) if (!isTRUE(cond)) failures <<- c(failures, nm)

  out <- try({
    dd <- dedup_fn(fixture$decisions)
    fp <- assign_fn(fixture$results, dd)
    s  <- summary_fn(fp)
    list(dd = dd, fp = fp, s = s)
  }, silent = TRUE)
  if (inherits(out, "try-error")) return("errored")

  dd <- out$dd; fp <- out$fp; s <- out$s
  get <- function(id) fp$final_published[fp$abstract_id == id]

  note(nrow(dd) == length(unique(dd$abstract_id)), "one_row_per_abstract")
  note(identical(dd$reviewer[dd$abstract_id == "A7"], "GW"), "human_beats_newer_auto")
  note(identical(dd$reviewer[dd$abstract_id == "A4"], "AUTO"), "auto_kept_when_alone")
  note(isTRUE(get("A1")), "definite_wins")
  note(isTRUE(get("A2")), "human_match_promotes")
  note(is.na(get("A3")), "skip_on_probable_is_na")
  note(isFALSE(get("A4")), "auto_only_resolves_false")
  note(isFALSE(get("A5")), "no_match_resolves_false")
  note(isTRUE(get("A6")), "human_match_overrides_excluded")
  note(isTRUE(get("A7")), "human_decision_survives")
  note(identical(dd$reviewer[dd$abstract_id == "A8"], "JM"), "latest_human_wins")
  note(isTRUE(get("A8")), "latest_human_decision_applied")
  note(isFALSE(get("A9")), "excluded_without_reviewer_is_false")
  note(nrow(fp) == nrow(fixture$results), "no_row_duplication_after_join")
  note(identical(s$n_evaluated, s$n_cohort - s$n_pending), "denominator_definition")
  note(identical(s$n_published + s$n_not_published, s$n_evaluated), "parts_close")
  note(isTRUE(all.equal(s$publication_rate, s$n_published / s$n_evaluated)), "rate_uses_denominator")
  failures
}

test_that("the unmutated implementation satisfies every invariant", {
  expect_equal(
    check_invariants(dedup_decisions_for_analysis, assign_final_published,
                     publication_rate_summary),
    character(0)
  )
})

# ------------------------------------------------------------
# Mutants
# ------------------------------------------------------------

# M1: the original defect. Precedence decided purely by timestamp, so AUTO
#     outranks a human whenever it is newer. Dormant until the re-run.
m1_dedup <- function(d) d |> filter(!is.na(reviewer)) |> group_by(abstract_id) |>
  arrange(desc(review_timestamp), .by_group = TRUE) |> slice(1) |> ungroup()

# M2: the over-correction. AUTO removed outright, stranding abstracts no human
#     ever saw at NA and shrinking the denominator.
m2_dedup <- function(d) d |> filter(!is.na(reviewer), reviewer != "AUTO") |>
  group_by(abstract_id) |> arrange(desc(review_timestamp), .by_group = TRUE) |>
  slice(1) |> ungroup()

# M3: ascending sort, so the OLDEST decision wins.
m3_dedup <- function(d) {
  h <- d |> filter(!is.na(reviewer), reviewer != "AUTO") |> pull(abstract_id) |> unique()
  d |> filter(!is.na(reviewer)) |> filter(!(abstract_id %in% h & reviewer == "AUTO")) |>
    group_by(abstract_id) |> arrange(review_timestamp, .by_group = TRUE) |>
    slice(1) |> ungroup()
}

# M4: no slice(), so multi-reviewer abstracts duplicate rows downstream.
m4_dedup <- function(d) {
  h <- d |> filter(!is.na(reviewer), reviewer != "AUTO") |> pull(abstract_id) |> unique()
  d |> filter(!is.na(reviewer)) |> filter(!(abstract_id %in% h & reviewer == "AUTO"))
}

# M5: `excluded` dropped from the FALSE branch, so pre-congress abstracts fall
#     through to NA and leave the denominator. This is the A12.7 defect class.
m5_assign <- function(r, dd) r |>
  left_join(select(dd, any_of(c("abstract_id","manual_decision","manual_pmid"))), by = "abstract_id") |>
  mutate(final_published = case_when(
    classification == "definite" ~ TRUE,
    manual_decision == "match" ~ TRUE,
    manual_decision == "no_match" ~ FALSE,
    classification %in% c("no_match", "no_candidates") ~ FALSE,
    TRUE ~ NA))

# M6: skip silently treated as no_match, so unresolved abstracts are counted as
#     unpublished and the denominator inflates without anyone deciding to.
m6_assign <- function(r, dd) r |>
  left_join(select(dd, any_of(c("abstract_id","manual_decision","manual_pmid"))), by = "abstract_id") |>
  mutate(final_published = case_when(
    classification == "definite" ~ TRUE,
    manual_decision == "match" ~ TRUE,
    manual_decision %in% c("no_match", "skip") ~ FALSE,
    classification %in% c("no_match", "no_candidates", "excluded") ~ FALSE,
    TRUE ~ NA))

# M7: rate divided by the cohort instead of the denominator. This is exactly the
#     discrepancy a reader hits recomputing 178/1106 rather than 178/1051.
m7_summary <- function(x) {
  n_cohort <- nrow(x); n_pending <- sum(is.na(x$final_published))
  n_pub <- sum(x$final_published, na.rm = TRUE)
  tibble::tibble(n_cohort = n_cohort, n_pending = n_pending,
                 n_evaluated = n_cohort - n_pending, n_published = n_pub,
                 n_not_published = n_cohort - n_pending - n_pub,
                 publication_rate = n_pub / n_cohort)
}

# M8: denominator counts pending as evaluated.
m8_summary <- function(x) {
  n_cohort <- nrow(x); n_pending <- sum(is.na(x$final_published))
  n_pub <- sum(x$final_published, na.rm = TRUE)
  tibble::tibble(n_cohort = n_cohort, n_pending = n_pending,
                 n_evaluated = n_cohort, n_published = n_pub,
                 n_not_published = n_cohort - n_pub,
                 publication_rate = n_pub / n_cohort)
}

mutants <- list(
  list(id = "M1 timestamp-only precedence (AUTO can outrank a human)",
       d = m1_dedup, a = assign_final_published, s = publication_rate_summary),
  list(id = "M2 AUTO excluded outright (strands AUTO-only abstracts at NA)",
       d = m2_dedup, a = assign_final_published, s = publication_rate_summary),
  list(id = "M3 oldest decision wins",
       d = m3_dedup, a = assign_final_published, s = publication_rate_summary),
  list(id = "M4 no deduplication (row duplication)",
       d = m4_dedup, a = assign_final_published, s = publication_rate_summary),
  list(id = "M5 'excluded' dropped from the FALSE branch",
       d = dedup_decisions_for_analysis, a = m5_assign, s = publication_rate_summary),
  list(id = "M6 skip treated as no_match",
       d = dedup_decisions_for_analysis, a = m6_assign, s = publication_rate_summary),
  list(id = "M7 rate divided by cohort, not denominator",
       d = dedup_decisions_for_analysis, a = assign_final_published, s = m7_summary),
  list(id = "M8 pending counted as evaluated",
       d = dedup_decisions_for_analysis, a = assign_final_published, s = m8_summary)
)

for (m in mutants) {
  local({
    mut <- m
    test_that(paste("MUTANT KILLED:", mut$id), {
      failed <- check_invariants(mut$d, mut$a, mut$s)
      expect_true(length(failed) > 0,
                  label = paste0("mutant survived undetected: ", mut$id))
      succeed()
    })
  })
}

test_that("every mutant is killed, and by a named invariant", {
  survivors <- vapply(mutants, function(m) {
    length(check_invariants(m$d, m$a, m$s)) == 0
  }, logical(1))
  expect_equal(sum(survivors), 0L,
               label = paste("surviving mutants:",
                             paste(vapply(mutants[survivors], `[[`, "", "id"),
                                   collapse = "; ")))
})
