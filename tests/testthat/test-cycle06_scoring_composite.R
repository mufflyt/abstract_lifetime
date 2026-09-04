# Cycle 6 of 24. Mix: 3 BVA, 3 semantic, 4 adversarial.
#
# Target: the composite scoring system that decides every match. Cycle 1 tested
# the classification tiers built ON TOP of the score; this cycle tests the score
# itself, its ten components, and the uniqueness of what it selects.

library(testthat)
library(dplyr)

P_FINAL <- here::here("output", "final_analytical_dataset.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("pipeline outputs not present")
COMP <- c("title_pts", "abstract_pts", "first_au_pts", "last_au_pts",
          "coauthor_pts", "team_bonus", "journal_pts", "keyword_pts",
          "date_pts", "no_text_penalty")

# ============================================================
# BVA 6.1 — the composite is exactly the sum of its parts
# ============================================================
test_that("best_score equals the sum of its ten components", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE) |> filter(!is.na(best_score))
  skip_if(!all(COMP %in% names(f)), "component columns absent")
  csum <- rowSums(f[, COMP], na.rm = TRUE)
  expect_true(all(abs(f$best_score - csum) < 1e-6),
              label = paste("best_score diverges from its components on",
                            sum(abs(f$best_score - csum) >= 1e-6), "rows"))
})

# ============================================================
# BVA 6.2 — each component stays inside its own range
# ============================================================
test_that("no component exceeds its documented contribution", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE) |> filter(!is.na(best_score))
  bounds <- list(title_pts = c(0, 3), abstract_pts = c(0, 2), first_au_pts = c(0, 2),
                 last_au_pts = c(0, 2), coauthor_pts = c(0, 1), team_bonus = c(0, 1),
                 journal_pts = c(0, 1), keyword_pts = c(0, 1),
                 date_pts = c(-3, 1), no_text_penalty = c(-2, 0))
  for (nm in names(bounds)) {
    v <- f[[nm]]; v <- v[!is.na(v)]
    expect_gte(min(v), bounds[[nm]][1], label = paste(nm, "below its floor"))
    expect_lte(max(v), bounds[[nm]][2], label = paste(nm, "above its ceiling"))
  }
})

# ============================================================
# BVA 6.3 — title similarity is bounded and consistent with its points
# ============================================================
test_that("title_sim is bounded and zero similarity earns no title points", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE) |> filter(!is.na(title_sim))
  expect_true(all(f$title_sim >= 0 & f$title_sim <= 1))
  zero <- f |> filter(title_sim == 0)
  if (nrow(zero) > 0) expect_true(all(zero$title_pts == 0),
                                  label = "points awarded for zero title similarity")
})

# ============================================================
# SEMANTIC 6.4 — the tie rule demotes, and it is actually applied
# ============================================================
test_that("a tied best candidate is never left classified definite", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  skip_if(!"has_tie" %in% names(f), "has_tie absent")
  bad <- f |> filter(classification == "definite", has_tie %in% c(TRUE, "TRUE"))
  expect_equal(nrow(bad), 0L,
               label = "utils_scoring.R:481 demotes tied definites to probable; these escaped")
})

# ============================================================
# SEMANTIC 6.5 — every component must be capable of contributing
# ============================================================
# PRESERVED FAILING TEST — see tests/loop/LEDGER.md.
# keyword_pts is 0 for all 1,106 abstracts. The scorer guards the block with
# `!is.null(abstract$keywords)`, and the cleaned abstracts carry no keywords
# column at all, so the branch is unreachable. The manuscript describes a
# "10-component composite scoring system"; one component is structurally dead.
# Removing it or sourcing keywords both change the score, so neither is done here.
test_that("no scoring component is structurally dead", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE) |> filter(!is.na(best_score))
  dead <- COMP[vapply(COMP, function(cc) all(f[[cc]] == 0, na.rm = TRUE), logical(1))]
  expect_true(length(dead) == 0,
              label = paste0("component(s) never fire on any of ", nrow(f),
                             " abstracts: ", paste(dead, collapse = ", "),
                             ". The composite is smaller than it is described to be."))
})

# ============================================================
# SEMANTIC 6.6 — the text penalty only subtracts, and blocks the top tier
# ============================================================
test_that("no_text_penalty is non-positive and bars a definite classification", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE) |> filter(!is.na(best_score))
  expect_true(all(f$no_text_penalty <= 0, na.rm = TRUE),
              label = "a penalty component added points")
  penalised <- f |> filter(no_text_penalty < 0)
  if (nrow(penalised) > 0) {
    expect_equal(sum(penalised$classification == "definite"), 0L,
                 label = "classify_match reserves 'definite' for candidates with text evidence")
  }
})

# ============================================================
# ADVERSARIAL 6.7 — one publication cannot be two abstracts' conversion
# ============================================================
# PRESERVED FAILING TEST — see tests/loop/LEDGER.md.
# Three PMIDs are each claimed by two abstracts counted as published, so the
# numerator carries three duplicate credits. Deciding which abstract owns each
# PMID is adjudication, not something to resolve in code.
test_that("no PMID is counted as the publication of more than one abstract", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE) |>
    filter(final_published %in% TRUE, !is.na(final_pmid))
  dup <- f |> count(final_pmid, name = "n") |> filter(n > 1)
  expect_true(nrow(dup) == 0,
              label = paste0(nrow(dup), " PMID(s) claimed by ", sum(dup$n),
                             " published abstracts, inflating the numerator by ",
                             sum(dup$n) - nrow(dup), ": ",
                             paste(dup$final_pmid, collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 6.8 — no candidates means no score and no match
# ============================================================
test_that("an abstract with no candidates cannot carry a match", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE)
  skip_if(!"n_candidates" %in% names(f), "n_candidates absent")
  none <- f |> filter(n_candidates == 0)
  skip_if(nrow(none) == 0, "no zero-candidate abstracts")
  expect_true(all(none$classification == "no_candidates"),
              label = "a zero-candidate abstract was given a match classification")
  expect_true(all(none$final_published %in% c(FALSE, NA)),
              label = "a zero-candidate abstract was counted as published")
})

# ============================================================
# ADVERSARIAL 6.9 — a match tier must carry the PMID it matched
# ============================================================
test_that("every abstract in a match tier has an identifier for that match", {
  need(P_FINAL)
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE) |>
    filter(classification %in% c("definite", "probable", "possible"))
  expect_true(all(!is.na(f$best_pmid)),
              label = paste(sum(is.na(f$best_pmid)),
                            "abstracts sit in a match tier with no best_pmid"))
})

# ============================================================
# ADVERSARIAL 6.10 — a missing component must not vanish silently
# ============================================================
test_that("component columns carry no NA that rowSums would swallow", {
  need(P_FINAL)
  # Scoped to abstracts that HAVE a candidate. The four no_candidates rows carry
  # NA across every component because nothing was scored, which is correct; an
  # earlier draft of this test flagged them and its premise was too broad.
  f <- readr::read_csv(P_FINAL, show_col_types = FALSE) |>
    filter(!is.na(best_score), classification != "no_candidates")
  na_counts <- vapply(COMP, function(cc) sum(is.na(f[[cc]])), integer(1))
  # rowSums(na.rm = TRUE) treats a missing component as zero, so a scoring bug
  # that produced NA would be indistinguishable from a component that scored 0.
  expect_true(all(na_counts == 0),
              label = paste("NA present in:",
                            paste(names(na_counts)[na_counts > 0], collapse = ", ")))
})
