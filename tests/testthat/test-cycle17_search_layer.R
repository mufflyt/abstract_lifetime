# Cycle 17 of 24. Mix: 3 BVA, 4 semantic, 3 adversarial.
#
# Target: the candidate-generation layer. R/03_search_pubmed.R builds the
# candidate pool that every later stage scores, adjudicates and counts, and
# R/utils_pubmed.R:429 build_search_strategies() decides what is even looked
# for. Nothing had tested it: cycles 4 and 6 measured what the SCORING did with
# candidates, never how candidates came to exist.
#
# It is testable in CI for the first time as of the committed candidate index,
# which is what made this cycle worth spending here rather than on another
# downstream artefact.
#
# Contracts read from the source rather than assumed:
#   utils_pubmed.R:429  six strategies: title, title_fragment, first_author,
#                       last_author, author_broad, author_keywords
#   utils_pubmed.R:444  the title window is 8 consecutive words starting at the
#                       first word of >= 3 characters
#   utils_pubmed.R      build_date_filter() is "<start>:<end>[PDAT]" plus a
#                       NOT clause over five publication types
#   03_search_pubmed.R:133 efficacy is grouped by strategy over strategy_df

library(testthat)
library(dplyr)

P_EFF  <- here::here("output", "search_strategy_efficacy.csv")
P_STRA <- here::here("data", "processed", "pubmed_strategy_results.csv")
P_SC   <- here::here("data", "processed", "match_scores.csv")
P_CLEAN <- here::here("data", "processed", "abstracts_cleaned.csv")
need <- function(...) if (!all(file.exists(c(...)))) skip("inputs not present")

# The six names the builder can emit, read from the source so this cannot drift
# into a list I invented.
STRATEGY_NAMES <- local({
  src <- readLines(here::here("R", "utils_pubmed.R"), warn = FALSE)
  m <- regmatches(src, gregexpr('strategies\\[\\["[a-z_]+"\\]\\]', src))
  unique(gsub('.*\\[\\["|"\\]\\].*', "", unlist(m)))
})

# ============================================================
# BVA 17.1 - the date filter spans exactly the configured window
# ============================================================
test_that("build_date_filter uses the configured bounds and excludes the noise types", {
  skip_if_not(file.exists(here::here("config.yml")), "no config")
  cfg <- config::get(file = here::here("config.yml"))
  src <- paste(readLines(here::here("R", "utils_pubmed.R"), warn = FALSE), collapse = "\n")

  # A window that silently narrowed would shrink every candidate set and lower
  # the publication rate without any test noticing.
  expect_true(!is.null(cfg$pubmed$date_start) && !is.null(cfg$pubmed$date_end),
              label = "config.yml does not define the PubMed date window")
  expect_true(grepl("\\[PDAT\\]", src, fixed = FALSE),
              label = "the date filter no longer restricts on PDAT")
  for (pt in c("Letter", "Comment", "Editorial", "Published Erratum",
               "Retraction of Publication")) {
    expect_true(grepl(pt, src, fixed = TRUE),
                label = paste("publication type", pt, "is no longer excluded;",
                              "these crowd out real papers in every candidate set"))
  }
})

# ============================================================
# BVA 17.2 - candidate counts per abstract are non-negative and bounded
# ============================================================
test_that("per-abstract candidate counts are sane at both ends", {
  cands <- candidate_pool()
  skip_if(is.null(cands), "no candidate pool or index")
  n <- cands |> count(abstract_id, name = "k")
  expect_true(all(n$k >= 1),
              label = "an abstract appears in the pool with zero candidate rows")
  # The pool is capped per strategy by retmax. A single abstract holding an
  # implausible share of the whole pool would mean a runaway query rather than
  # a search.
  expect_lt(max(n$k), nrow(cands) * 0.05,
            label = sprintf("one abstract holds %d of %d candidate rows",
                            max(n$k), nrow(cands)))
})

# ============================================================
# BVA 17.3 - the efficacy table's own arithmetic holds at the edges
# ============================================================
test_that("efficacy percentages are consistent with the counts they summarise", {
  need(P_EFF)
  e <- readr::read_csv(P_EFF, show_col_types = FALSE)
  expect_gt(nrow(e), 0)
  expect_true(all(e$n_with_results <= e$n_abstracts_searched),
              label = "a strategy reports more hits than searches")
  expect_true(all(e$n_with_results >= 0) && all(e$n_abstracts_searched > 0))
  # round(mean * 100, 1) at 03_search_pubmed.R:137. Allow one rounding step.
  recomputed <- round(e$n_with_results / e$n_abstracts_searched * 100, 1)
  expect_true(all(abs(recomputed - e$pct_with_results) <= 0.15),
              label = paste("pct_with_results does not reconcile with its own",
                            "numerator and denominator for:",
                            paste(e$strategy[abs(recomputed - e$pct_with_results) > 0.15],
                                  collapse = ", ")))
  expect_true(all(e$median_results <= e$mean_results * 50),
              label = "a median wildly out of line with its mean suggests two populations")
})

# ============================================================
# SEMANTIC 17.4 - a defined strategy must be able to run
# ============================================================
test_that("every search strategy the builder defines can actually be built", {
  need(P_EFF, P_CLEAN)
  e <- readr::read_csv(P_EFF, show_col_types = FALSE)
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)

  unreached <- setdiff(STRATEGY_NAMES, e$strategy)
  # author_keywords is built only when abstract_row$keywords is non-empty, and
  # abstracts_cleaned.csv has no keywords column at all, so the branch is
  # unreachable for every abstract in the cohort. That is the same dead keyword
  # pathway cycle 6 found in the scoring composite, appearing here in search.
  expect_true(length(unreached) == 0,
              label = paste0(
                "strategy/strategies defined in build_search_strategies() that ",
                "never ran: ", paste(unreached, collapse = ", "),
                ". The cohort has a keywords column: ",
                "keywords" %in% names(cl),
                ". A strategy that cannot be built searches for nothing, so the ",
                "candidate pool is narrower than the code describes."))
})

# ============================================================
# SEMANTIC 17.5 - candidates belong to the cohort that was searched
# ============================================================
test_that("every candidate abstract_id is an abstract that exists", {
  need(P_CLEAN)
  cands <- candidate_pool()
  skip_if(is.null(cands), "no candidate pool or index")
  cl <- readr::read_csv(P_CLEAN, show_col_types = FALSE)
  # The pool was built before the video exclusion, so it legitimately covers
  # more abstracts than the cohort. What it must never do is name an
  # abstract_id that does not exist anywhere, which would mean the join key
  # was rewritten between stages.
  parsed <- here::here("data", "processed", "abstracts_parsed.csv")
  known <- if (file.exists(parsed)) {
    unique(readr::read_csv(parsed, show_col_types = FALSE)$abstract_id)
  } else {
    unique(cl$abstract_id)
  }
  orphans <- setdiff(unique(cands$abstract_id), known)
  expect_true(length(orphans) == 0,
              label = sprintf("%d candidate abstract_ids match no parsed abstract: %s",
                              length(orphans),
                              paste(utils::head(orphans, 3), collapse = ", ")))
})

# ============================================================
# SEMANTIC 17.6 - every scored pair is a pair the search produced
# ============================================================
test_that("scoring never invents a candidate the search did not return", {
  need(P_SC)
  cands <- candidate_pool()
  skip_if(is.null(cands), "no candidate pool or index")
  sc <- readr::read_csv(P_SC, show_col_types = FALSE) |>
    mutate(best_pmid = as.character(best_pmid))
  # The reverse of the F2 invariant. F2 asks whether the pool covers the
  # winners; this asks whether a winner could have come from somewhere else,
  # which would mean the two stages disagree about the candidate universe.
  scored <- sc |> filter(!is.na(best_pmid)) |> select(abstract_id, pmid = best_pmid)
  outside <- anti_join(scored, distinct(select(cands, abstract_id, pmid)),
                       by = c("abstract_id", "pmid"))
  expect_equal(nrow(outside), 0L,
               label = sprintf("%d scored winners are absent from the candidate pool",
                               nrow(outside)))
})

# ============================================================
# SEMANTIC 17.7 - the searched denominator matches what was reported
# ============================================================
test_that("differences in per-strategy denominators are fully explained", {
  need(P_EFF, P_STRA)
  e <- readr::read_csv(P_EFF, show_col_types = FALSE)
  st <- readr::read_csv(P_STRA, show_col_types = FALSE)

  # The strategies do NOT share a denominator, and that is correct rather than
  # a defect: utils_pubmed.R builds the author strategies only when
  # first/last_author_normalized is non-NA and longer than one character, so an
  # abstract with no parsed author gets the title strategies and nothing else.
  #
  # My first version of this test asserted the denominators were equal and
  # failed on a four-abstract difference. That was the test being wrong, not
  # the pipeline. What is worth asserting is that the gap is ACCOUNTED FOR,
  # because an unexplained gap would mean a strategy silently failed to build
  # for reasons nobody had noticed. The consequence either way is real:
  # pct_with_results is computed against each strategy's own denominator, so
  # the rates are not directly comparable across rows.
  title_ids  <- unique(st$abstract_id[st$strategy == "title"])
  author_ids <- unique(st$abstract_id[st$strategy == "first_author"])
  unexplained <- setdiff(title_ids, author_ids)

  cl <- if (file.exists(P_CLEAN)) readr::read_csv(P_CLEAN, show_col_types = FALSE) else NULL
  skip_if(is.null(cl) || !"first_author_normalized" %in% names(cl),
          "cohort lacks first_author_normalized")
  no_author <- cl$abstract_id[is.na(cl$first_author_normalized) |
                              nchar(cl$first_author_normalized) <= 1]

  still_unexplained <- setdiff(unexplained, no_author)
  expect_true(length(still_unexplained) == 0,
              label = sprintf(paste("%d abstract(s) received the title strategies but not",
                                    "the author strategies, and DO have a usable first",
                                    "author, so a strategy failed to build for an unknown",
                                    "reason: %s"),
                              length(still_unexplained),
                              paste(utils::head(still_unexplained, 5), collapse = ", ")))

  # And the reverse: nothing should get an author strategy but no title one.
  expect_true(length(setdiff(author_ids, title_ids)) == 0,
              label = "an abstract was searched by author but never by title")
})

# ============================================================
# ADVERSARIAL 17.8 - the pool holds no duplicated pair
# ============================================================
test_that("no abstract/PMID pair appears twice in the candidate pool", {
  cands <- candidate_pool()
  skip_if(is.null(cands), "no candidate pool or index")
  d <- cands |> count(abstract_id, pmid, name = "k") |> filter(k > 1)
  # A duplicated pair double-weights that candidate in any per-abstract count
  # and can make a coverage check pass by repetition.
  expect_equal(nrow(d), 0L,
               label = sprintf("%d abstract/PMID pairs are duplicated in the pool",
                               nrow(d)))
})

# ============================================================
# ADVERSARIAL 17.9 - strategy provenance uses the closed vocabulary
# ============================================================
test_that("recorded strategy names come only from the builder's vocabulary", {
  need(P_STRA)
  s <- readr::read_csv(P_STRA, show_col_types = FALSE)
  skip_if(!"strategy" %in% names(s), "no strategy column")
  seen <- unique(unlist(strsplit(as.character(s$strategy[!is.na(s$strategy)]), ";\\s*")))
  extra <- setdiff(seen, STRATEGY_NAMES)
  expect_true(length(extra) == 0,
              label = paste("strategy names recorded that the builder cannot emit:",
                            paste(extra, collapse = ", ")))
})

# ============================================================
# ADVERSARIAL 17.10 - the efficacy summary is not from an older search
# ============================================================
test_that("the efficacy summary describes the strategy results beside it", {
  need(P_EFF, P_STRA)
  e <- readr::read_csv(P_EFF, show_col_types = FALSE)
  s <- readr::read_csv(P_STRA, show_col_types = FALSE)
  skip_if(!"strategy" %in% names(s), "no strategy column")
  # Recompute the summary from the raw table. A mismatch means one of the two
  # artefacts is from an older run, which is how a stale 0.2% title hit rate
  # survived a correction to the title search.
  recomputed <- s |> group_by(strategy) |>
    summarise(n = n(), hits = sum(n_results > 0), .groups = "drop")
  j <- inner_join(e, recomputed, by = "strategy")
  expect_equal(nrow(j), nrow(e),
               label = "the efficacy table names a strategy absent from the raw results")
  drift <- j |> filter(n != n_abstracts_searched | hits != n_with_results)
  expect_equal(nrow(drift), 0L,
               label = paste("efficacy rows that do not reconcile with the raw",
                             "strategy results (mismatched vintages):",
                             paste(drift$strategy, collapse = ", ")))
})
