# Pipeline Failure Modes

Places where a run can produce a **plausible but incorrect** result. Each entry
is written as **Failure → Detection → Prevention**, with a status saying whether
the failure has already occurred in the shipped data.

Severity: 🔴 affects a reported number · 🟠 affects a reported number's
interpretation · 🟡 latent.

---

## F1 🔴 The supplement listing is truncated at ~100 items per congress — **has occurred**

**Failure.** `R/01b_parse_web.R` fetches one issue-listing page per congress.
Offset pagination is attempted only when the first page returns exactly 100
items, and ScienceDirect returns the same page for every offset, so the loop
breaks immediately (`:353-364`). The result is 93–100 presentations captured per
congress against 392–852 supplement items deposited in Crossref. In 2012–2021
the captured window ends while still inside the Oral block, so an unknown number
of oral presentations were never ingested. Every downstream number is correct
*for the abstracts that were captured*; the cohort definition is what is wrong.

**Detection.** Compare each congress's captured DOI set against Crossref:
`api.crossref.org/journals/1553-4650/works?filter=from-pub-date:<Y>-11-01,until-pub-date:<Y>-11-30`,
counting records with `S`-prefixed pagination. A capture that stops at 93–100
in every year, with no `Video` or `Poster` row in ten of twelve years, is the
signature. Full evidence in [COHORT_ASSEMBLY.md](COHORT_ASSEMBLY.md) §5.

**Prevention.** Enumerate the supplement from Crossref (which is complete and
unauthenticated) rather than from the ScienceDirect listing, and use
ScienceDirect only to fetch each article page. Add a test that asserts the
captured DOI count per congress equals the Crossref supplement count, or fails
loudly with the shortfall. Failing that, log the raw listing item count per
congress so the ceiling is visible in the run log.

---

## F2 🔴 `pubmed_candidates.csv` is stale relative to `match_scores.csv` — **has occurred**

**Failure.** `R/03b_search_crossref.R:463` rewrites `pubmed_candidates.csv` in
place at the end of every supplementary-search run. On 2026-04-19,
`04_score_matches.R` ran at 09:21 and `03b` rewrote the candidate file at 09:47.
The file now on disk is a **strict subset** of the pool that was scored:

| | value |
|---|---:|
| Candidate pairs scored (`sum(n_candidates)`, and `match_scores_detailed.rds`) | 64,718 |
| Rows in `pubmed_candidates.csv` | 48,984 |
| Winning PMIDs (`best_pmid`) absent from the file | **283 of 1,102 (25.7%)** |
| `abstract_id` values in the file that are not in the cohort | 610 (from the superseded 686-row 2023 scrape, `AAGL2023_099`–`AAGL2023_686`) |

`R/05_adjudicate.R:22-29` joins publication metadata onto `best_pmid` from this
file. For the 283 unresolvable PMIDs the join yields `NA`, so `pub_title`,
`pub_journal`, `pub_year`, `pub_doi`, `pub_first_author` and `months_to_pub` are
blank **even for `definite` matches**.

Consequences in the shipped results:

- Only 137 of 1,106 rows carry publication metadata.
- **74 of the 178 published abstracts have no publication date.**
- Aim 2's median (13.8 months, IQR 6.3–25.0) is computed on 104 events.
- The Kaplan–Meier fit has n = 977 and 104 events instead of 1,051 and 178; the
  74 dateless events are dropped, not censored, so the curve understates
  cumulative publication.
- The Cox model has 104 events.
- Aim 4's strategy attribution joins the same file and is understated.

`data/processed/match_scores_detailed.rds` still holds all 64,718 scored pairs
with their PMIDs and component scores — the publication *metadata* is what was
lost, and it is recoverable by refetching those 283 PMIDs from PubMed.

**Detection.**
```r
sc   <- readr::read_csv("data/processed/match_scores.csv")
cand <- readr::read_csv("data/processed/pubmed_candidates.csv")
stopifnot(sum(sc$n_candidates) == nrow(cand))
stopifnot(all(na.omit(as.character(sc$best_pmid)) %in% cand$pmid))
```
Both assertions currently fail. `tests/testthat/test-docs_drift.R` records the
current shortfall as a pinned regression floor.

**Prevention.** Make `03b` append to a separate file rather than rewriting the
pool, or make `04` read `pubmed_candidates.csv` and record its file hash beside
`match_scores.csv` so a mismatch is detectable. Rebuilding the pool from
`pubmed_search_checkpoint.rds` plus the four supplementary candidate CSVs would
restore the 283 records.

---

## F3 🔴 Study characteristics were derived before the abstract text existed — **has occurred**

**Failure.** `R/02_clean_abstracts.R` derives ~20 predictor variables by regex
over `search_text = coalesce(abstract_full_text, abstract_text, title)`. For
congress years 2012–2018 all abstract text was paywalled at scrape time, so
`search_text` was **the title alone**. The text is recovered afterwards by
`02b_backfill_abstract_text.R` and
`scripts/backfill_sciencedirect_snippets.R`, which patch `abstract_text` **and
nothing else**. The predictors are never recomputed, and the structured section
columns (`abstract_objective`, `abstract_measurements`, `abstract_conclusion`, …)
are never filled at all.

The result is a step change at 2018/2019 in every text-derived variable:

| Variable | 2012–2018 | 2019–2023 |
|---|---:|---:|
| `has_numeric_results` TRUE | **0.0%** in all seven years | 46.9 – 88.9% |
| `is_us_based` TRUE | 31.2 – 44.8% | 96.9 – 100% |
| `is_academic` TRUE | 0.0 – 4.2% | 22.4 – 46.7% |
| `sample_size` present | 4.2 – 12.9% | 65.6 – 77.1% |
| `study_design == "other"` | 78.9 – 86.6% | 8.9 – 53.1% |
| `has_irb_statement` TRUE | 0.0% | 0.0 – 5.0% |
| `has_trial_registration` TRUE | 0.0% | 1.0 – 3.3% |
| `abstract_word_count` | **0 in every row** | 237 – 282 |

The outcome varies across the same boundary (5.6–27.4% in 2012–2018 versus
13.8–33.3% in 2019–2023), and **congress year is in neither model**. Every
coefficient in the Cox and logistic models is therefore confounded by congress
year through measurement, not only through follow-up time. `is_us_based`
(HR 1.71, p = 0.019) and `is_academic` are the most exposed; `is_rct` less so,
because randomisation is usually stated in the title.

**Detection.** Cross-tabulate every derived flag by `congress_year` and look for
a discontinuity that coincides with the text-availability boundary. A flag that
is exactly 0.0% for seven consecutive years is definitional, not empirical.

**Prevention.** Move the predictor derivation after the backfills — either into
a `02d` step or by having `02b`/`02c` call the derivation functions in
`R/utils_classify.R` on the rows they patch. Then re-run `06`–`08`. Adding
`congress_year` to both models would expose, though not remove, the confounding.

---

## F4 🟠 Two subgroup tables condition on the outcome — **has occurred**

**Failure.** `practice_type` and `subspecialty` are parsed from the *matched
publication's* affiliation, so they exist almost only for published abstracts
(145 of 178 published versus 21 of 873 unpublished have a `practice_type`).
`output/aim1_by_practice_type.csv` and `output/aim1_by_subspecialty.csv`
therefore report ~90–100% "publication rates" in every stratum, and
`figure4_subgroup_rates.png` plots them beside genuinely outcome-independent
strata.

**Detection.** For any stratifying variable, tabulate presence against the
outcome. A variable present for 81% of the published and 2% of the unpublished
cannot support a rate.

**Prevention.** Restrict subgroup rates to variables whose availability does not
depend on the outcome — `study_design`, `is_rct`, `congress_year`,
`gender_unified`, and the NPI-backed portion of `subspecialty_unified` — or
report the affiliation-derived variables as descriptors of the published set
only.

---

## F5 🟠 A failed API call is indistinguishable from a genuine zero result — **latent, may already have occurred**

**Failure.** `rate_limited_search()` catches every error, warns, and returns
`NULL` (`R/utils_pubmed.R:69-73`); `search_abstract()` treats `NULL` exactly like
an empty hit list; `03_search_pubmed.R` records `n_results = 0` and marks the
abstract complete in the checkpoint. No retry, no error column, no way to tell
afterwards. `search_semantic_scholar()` retries once on HTTP 429 and then gives
up silently. A transient NCBI outage during a run leaves permanently
under-searched abstracts that the checkpoint will never revisit.

**Detection.** None available retrospectively. Prospectively: an abstract with
zero candidates from *every* strategy including `author_broad` is suspicious —
`author_broad` yields hits for 93% of abstracts.

**Prevention.** Return a sentinel distinct from an empty result, record a
per-strategy status column, retry with exponential backoff, and do not mark an
abstract complete unless every strategy returned a definite status.

---

## F6 🟠 Checkpoints resume but never invalidate — **has occurred**

**Failure.** Each search script skips any `abstract_id` in `completed_ids`.
Editing a search strategy does not invalidate the checkpoint, so the edited
strategy never runs for an already-completed abstract. The April 2026 matching
corrections only took effect because the 35 MB
`pubmed_search_checkpoint.rds` was deleted by hand.

**Detection.** Compare the checkpoint's modification time against
`R/utils_pubmed.R` and `R/02_clean_abstracts.R`. A checkpoint older than the
code that builds the queries is stale.

**Prevention.** Store a hash of the strategy-generating code in the checkpoint
and drop `completed_ids` when it changes.

---

## F7 🟠 Search-stage short-circuits skip silently — **has occurred**

**Failure.** `R/01b_parse_web.R:331-341` skips the scrape entirely whenever the
existing CSV has ≥ 80 rows per configured congress. The scrape has not run since
2026-04-19 and will not run again while that file exists — which is how F1
persists. Likewise `00_run_all.R:24-27` runs the PDF fallback only if
`abstracts_parsed.csv` is missing, and `01c_compare_sources.R` runs only if a
file that has never existed is present.

**Detection.** The run log says "skipping scrape". Nothing downstream flags it.

**Prevention.** Make the short-circuit opt-in
(`SKIP_SCRAPE=1`) rather than automatic, and print the per-congress item counts
it is trusting.

---

## F8 🔴 `00_run_all.R` never runs the demographics merge — **has occurred**

**Failure.** `R/10e_merge_demographics.R` is not sourced by `00_run_all.R`, and
neither are `10b`, `10d`, `10f`, `10g` or `run_demographics.R`. A clean run of
the master pipeline therefore produces an `abstracts_with_matches.csv` with no
`gender_unified`, no `npi_*`, no `state_unified` and no `subspecialty_unified`.
`06_analyze_results.R` selects model terms with
`intersect(candidate_vars, names(km_data))`, so it drops those predictors
**without warning** and fits a smaller model that still writes to
`aim2b_cox_regression.csv` and `aim3_logistic_regression.csv`. The output looks
normal; the specification has silently changed.

`R/run_demographics.R` also wraps each of its twelve steps in `tryCatch()` and
only warns, so a partial merge is equally silent.

**Detection.** Assert the expected model terms after fitting, rather than
selecting them. Compare `names(read_csv("output/abstracts_with_matches.csv"))`
against the documented 86.

**Prevention.** Source `R/run_demographics.R` from `00_run_all.R` between steps
5h6 and 5i, and make the automatic variable screen fail loudly when an expected
term is absent.

---

## F9 🟡 Three copies of the decision logic

**Failure.** `07_make_tables.R` and `08_make_figures.R` each re-derive
`final_published` with an inline `case_when`, and their dedup keeps the latest
timestamp per abstract **without** the human-outranks-AUTO rule that
`dedup_decisions_for_analysis()` applies. The Shiny app has a fourth
implementation. They agree today only because the `AUTO` prefill happened before
human review, so timestamp order and precedence order coincide. Re-running
`scripts/prefill_algorithm_decisions.R` after human review would make the tables
and figures disagree with the analysis, silently.

**Detection.** Recompute both and compare — verified equal on all 1,106 rows on
2026-09-03. `tests/testthat/test-docs_drift.R` now asserts this equality.

**Prevention.** Have `07` and `08` read `output/final_analytical_dataset.csv`,
or source `R/utils_decisions.R`.

---

## F10 🟡 `05_adjudicate.R` recreates the accumulator

**Failure.** `R/05_adjudicate.R:117` writes `output/abstracts_with_matches.csv`
from scratch with 45 columns. Six pieces of code later add columns to the same
file in place. Re-running step 5 alone — the obvious thing to do after changing
a threshold — destroys every enrichment column and every demographic variable.

**Detection.** Column count. 45 means only step 5 has run; 86 is the complete
set.

**Prevention.** Have `05` write a distinct file that the enrichment stages join
onto, rather than an accumulator they mutate.

---

## F11 🟠 The Shiny deploy bundle is 135 days stale — **has occurred**

**Failure.** `shiny/adjudication_app/bundle/` holds copies of
`abstracts_cleaned.csv`, `match_scores_detailed.rds`, `pubmed_candidates.csv`,
`abstracts_with_matches.csv` and the decision files, dated 2026-04-19. The live
app at `mufflyt.shinyapps.io/aagl-adjudication` is therefore showing reviewers
data from before the denominator fix. Any decision recorded against the deployed
app is made against a superseded classification.

**Detection.** `tests/testthat/test-shiny_app.R:458` asserts the bundle is
within 24 hours of the main CSV. **It currently fails** with a 11,706,678-second
(135-day) gap — this is the single failing test in the suite, and it is
reporting a real problem rather than a fixture problem.

**Prevention.** Run `Rscript shiny/adjudication_app/deploy.R` after any
pipeline re-run, which is what `00_run_all.R:167-173` does. The gap exists
because the September re-runs were of steps 05–08 only.

---

## F12 🟡 Publication metadata is not refreshed when a reviewer supplies a PMID

**Failure.** `final_pmid = coalesce(manual_pmid, best_pmid)`, but `pub_title`,
`pub_journal`, `pub_year` and `months_to_pub` are joined on `best_pmid` in
`05_adjudicate.R`, which runs *before* the decisions are read. Where a reviewer
chose a different PMID (9 of the 178 published), the publication metadata and
`months_to_pub` describe the algorithm's candidate, not the reviewer's.

**Detection.** `sum(final_pmid != best_pmid, na.rm = TRUE)` among published rows
= 9.

**Prevention.** Re-fetch publication metadata for `final_pmid` after
adjudication, in `06` rather than `05`.

---

## F13 🟡 A missing decisions file changes the headline silently

**Failure.** `R/06_analyze_results.R:33-40`: with no
`output/manual_review_decisions.csv`, `final_published` becomes
`classification == "definite"`, nothing is `NA`, and the reported rate becomes
131/1,106 = 11.8% on a denominator of 1,106. Only a `cli` warning distinguishes
this from a normal run, and warnings do not reach the output files.

**Detection.** `aim1_publication_rate.csv` with `pending_review == 0`.

**Prevention.** Write the decision-source provenance into
`aim1_publication_rate.csv`, or `stop()` rather than warn.

---

## F14 🟡 Coarse PubMed dates are silently resolved to 1 January

**Failure.** `parse_pubmed_xml()` defaults a missing month and day to `"01"`, so
a record with a year-only `PubDate` is dated 1 January of that year. Combined
with the pre-conference short-circuit in `classify_match()`, a paper published
in, say, November 2017 but carrying only `2017` is dated 2017-01-01, falls before
the 2017-11-12 congress, and is classified `excluded` — counted unpublished.
Technical appendix A13.6 identifies at least two such cases among the 39
`excluded` abstracts, and notes that 11 of the 39 carry year-only dates.

**Detection.** `pub_month` missing in the candidate XML while `ArticleDate` is
present.

**Prevention.** Prefer `.//ArticleDate` when present, and treat a year-only date
as an interval rather than a point.

---

## F15 🟡 Stale generated artefacts that no longer have a producer

`output/aim5_publication_bias.csv` (the producing block is gated on a column
`05_adjudicate.R` no longer emits) and `output/search_strategy_efficacy.csv`
(pre-correction measurement) both look like current results and are not. Neither
is regenerated by a normal run, so neither will self-correct.
`output/excluded_pre_congress_publications.csv` has **no producer in the
repository at all**. Detection: compare file mtimes against
`output/final_analytical_dataset.csv`. Prevention: a `make clean` step, or an
assertion that every file in `output/` is newer than the dataset.

---

## F16 🟡 External absolute paths outside the repository

`R/10_npi_matching.R` hard-codes
`/Users/tylermuffly/isochrones/data/canonical_abog/canonical_abog_npi_LATEST.csv`
and `/Volumes/MufflySamsung/DuckDB/nber_my_duckdb.duckdb`. **Neither resolves on
this machine today** — the external drive mounts as `/Volumes/MufflySamsung 1 1/`
and the ABOG directory contains differently named files. The script degrades to
producing fewer matches rather than failing, so a re-run would quietly reduce
NPI coverage and change tier 1 of the gender waterfall. Prevention: move both
paths into `config.yml` and `stop()` when they are absent.
