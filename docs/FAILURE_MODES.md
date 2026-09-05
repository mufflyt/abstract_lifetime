# Pipeline Failure Modes

Places where a run can produce a **plausible but incorrect** result. Each entry
is written as **Failure → Detection → Prevention**, with a status saying whether
the failure has already occurred in the shipped data.

Severity: 🔴 affects a reported number · 🟠 affects a reported number's
interpretation · 🟡 latent.

**Status as of 2026-09-03.** F2, F3, F4, F8, F9, F10, F12, F15 and F16 have been
fixed and their entries record what changed. F1 is unfixed and cannot be fixed
without re-ingesting the congress supplements and re-adjudicating; F5, F6, F7,
F11, F13, F14 and F17 are unfixed and each says why.

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

## F2 🔴 ~~`pubmed_candidates.csv` is stale relative to `match_scores.csv`~~ — **FIXED 2026-09-03**

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

**Fix applied.** `scripts/rebuild_candidate_pool.R` takes every
(abstract_id, pmid) pair recorded in `match_scores_detailed.rds` — the surviving
record of what was scored — plus every reviewer-supplied `manual_pmid`, refetches
the missing metadata from PubMed in batches of 100, and rebuilds the file. It is
resumable and drops rows keyed to `abstract_id`s outside the 1,154 parsed
presentations. Recovered rows are tagged `strategies = "unrecovered"`, because
the strategy provenance lived only in the overwritten file and cannot be
restored; Aim 4 can therefore exclude them rather than miscount them.

**Result.**

| | before | after |
|---|---:|---:|
| Rows in the pool | 48,984 | 65,697 |
| Cohort pairs vs `sum(n_candidates)` | 48,984 vs 64,718 | **64,728 vs 64,718** |
| Winning PMIDs unresolvable | 283 | **0** |
| Published abstracts with a publication date | 104 / 178 | **178 / 178** |
| Aim 2 median time to publication | 13.8 mo (IQR 6.3–25.0) on 104 | **13.7 mo (IQR 5.7–22.6) on 171** |
| Kaplan–Meier events | 104 | **171** |
| Cox events | 104 | **171** |

`R/06_analyze_results.R` now also re-joins the publication fields on
`final_pmid` rather than inheriting `05`'s join on `best_pmid`, which is what
fixes F12 at the same time.

**Remaining prevention.** `03b` still rewrites the pool in place. Making it
append to a separate file, or recording the pool's hash beside
`match_scores.csv`, would stop the defect recurring.
`tests/testthat/test-docs_drift.R` and
`tests/testthat/test-remediation_invariants.R` both now assert that every
winning PMID resolves.

---

## F3 🔴 ~~Study characteristics were derived before the abstract text existed~~ — **FIXED 2026-09-03**

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

## F4 🟠 ~~Two subgroup tables condition on the outcome~~ — **FIXED 2026-09-03**

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

**Fix applied.** `subgroup_rate()` in `R/06_analyze_results.R` computes the
availability of the stratifier separately among the published and the
unpublished, attaches both to every row of the output, sets an
`outcome_conditional_stratifier` flag when the ratio exceeds 3, and warns at run
time. `aim1_by_practice_type.csv` now carries
`availability_among_published = 81.5` against
`availability_among_unpublished = 2.4`, so the artefact is unmissable in the
file itself. The rate is still emitted, because it is the correct conditional
quantity and the manuscript reads these files.

**Still outstanding.** `figure4_subgroup_rates.png` still plots these panels
beside outcome-independent strata. Whether to drop them is a presentation
decision for the author.

---

## F17 🟠 One publication credited to two abstracts — **surfaced 2026-09-03, not resolved**

**Failure.** Three PMIDs are each credited to two abstracts counted as
published, so six of the 178 numerator rows rest on three publications. Found by
`tests/testthat/test-cycle06_scoring_composite.R:116`.

| PMID | abstracts | what it is |
|---|---|---|
| 32604198 | `AAGL2019_036`, `AAGL2019_081` | Two companion systematic reviews of occult uterine malignancy presented at the same congress, both reviewer-confirmed to one paper. Plausibly a genuine merge. |
| 38906210 | `AAGL2021_030`, `AAGL2023_023` | Two relugolix SPIRIT analyses. **The 2021 abstract carries `manual_decision == "no_match"`** and is counted published only because branch 1 of `assign_final_published()` puts `classification == "definite"` ahead of every reviewer branch. |
| 39490893 | `AAGL2022_081`, `AAGL2023_027` | `AAGL2022_081` is about intrauterine anaesthesia and is matched to a paper on retained products of conception, which `AAGL2023_027` matches exactly. It looks like a reviewer error on a `possible` candidate. |

**Detection.** `R/06_analyze_results.R` now computes `final_pmid_shared` for
every published abstract whose credited publication is also credited to another,
warns at run time, and writes the evidence to
`output/shared_publication_matches.csv`.

**Why the numerator was not deduplicated.** The publication rate is a
per-abstract quantity, and two abstracts from one group can legitimately resolve
to one paper — companion analyses merged before submission. Cochrane MR000005
counts these per abstract. Deciding which abstract owns each PMID is
adjudication, not code, so 178 stands and the flag makes the exposure visible.

**What it demonstrates.** The 38906210 case is a concrete instance of the
branch-order asymmetry documented in
[ADJUDICATION.md](ADJUDICATION.md) §6: a reviewer explicitly said `no_match` and
the abstract is counted published anyway. Four abstracts are in that position
overall. Reordering the cascade is a methodological decision that moves 48
abstracts and has not been taken.

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

## F8 🔴 ~~`00_run_all.R` never runs the demographics merge~~ — **FIXED 2026-09-03**

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

**Fix applied.** `00_run_all.R` now sources `10b`, `10d`, `10f`, `10g` and
`10e_merge_demographics.R` as step 5h8, before the fidelity checks. A clean run
of the master pipeline now produces the full demographics block.

**Still outstanding.** The automatic variable screen in `06` still drops an
absent term silently rather than failing; and `run_demographics.R` still wraps
each step in `tryCatch()` and only warns.

---

## F9 🟡 ~~Three copies of the decision logic~~ — **FIXED 2026-09-03**

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

**Fix applied.** `R/07_make_tables.R` and `R/08_make_figures.R` now source
`R/utils_decisions.R` and call `dedup_decisions_for_analysis()` and
`assign_final_published()`; their inline copies are gone. The Shiny app keeps
its own `dedup_decisions()` because it deduplicates for display per
(abstract, reviewer) rather than for analysis, which is a different operation.
`tests/testthat/test-docs_drift.R` asserts the two agree on every row.

---

## F10 🟡 ~~`05_adjudicate.R` recreates the accumulator~~ — **FIXED 2026-09-03**

**Failure.** `R/05_adjudicate.R:117` writes `output/abstracts_with_matches.csv`
from scratch with 45 columns. Six pieces of code later add columns to the same
file in place. Re-running step 5 alone — the obvious thing to do after changing
a threshold — destroys every enrichment column and every demographic variable.

**Detection.** Column count. 45 means only step 5 has run; 86 is the complete
set.

**Fix applied.** `R/05_adjudicate.R` now reads any existing
`output/abstracts_with_matches.csv`, and re-joins on `abstract_id` every column
it does not itself produce, reporting how many it carried forward. Re-running
step 5 alone preserved all 41 enrichment columns in test.
`tests/testthat/test-remediation_invariants.R` asserts the enrichment block
survives.

**Still outstanding.** The file remains an accumulator six writers mutate.
Splitting it into a `05` output plus a separate enrichment table would be the
structural fix.

---

## F11 🟠 ~~The Shiny deploy bundle is 135 days stale~~ — **FIXED 2026-09-03 (bundle), live app awaiting a deploy**

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

**Fix applied.** Three changes:

1. The bundle is refreshed and now byte-identical to every source it copies.
   Verified by content hash, not modification time.
2. `deploy.R` gained a **verification step that runs before anything can be
   published** and `stop()`s the script if the bundle is not the data the
   analysis was run on. It checks md5 equality for the five verbatim files, that
   every winning PMID is present in the slimmed candidate pool, and that no
   abstract has fewer candidates in the bundle than were scored. Both failure
   paths were exercised: removing a source file and removing 40 winning
   candidates each abort the deploy with exit code 1.
   Steps 2 and 3 previously only emitted `cli_alert_warning()` and carried on,
   which is how the staleness survived unnoticed.
3. Deployment is now opt-in behind `SHINY_DEPLOY=true`, so refreshing the bundle
   in CI or a test run cannot publish to a live application reviewers are using.

**Verified end to end.** The app was started locally, served HTTP 200, and
rendered all twelve congress years. `tests/testthat/test-shiny_bundle_currency.R`
(48 assertions) drives the real server through `shiny::testServer()` and asserts
that the loaded cohort equals `abstracts_cleaned.csv`, that no abstract is served
fewer candidates than were scored, that every winning PMID is displayable, and
that the candidate-to-score join resolves. The last of those matters: before the
pool was rebuilt the app showed 26 candidates for `AAGL2012_001` where 35 had
been scored.

**CI coverage added 2026-09-04.** `bundle/` is gitignored, so the tests above
skip on a fresh checkout and this failure mode had no CI protection.
`deploy.R` now also writes `shiny/adjudication_app/bundle_manifest.csv`, which
is tracked and records each source's checksum at build time. Three tests compare
it against the current tracked sources and fail with "the deployed app is
serving older data than the analysis", naming the files that moved. Verified
both ways: appending one byte to a source makes them fail, and hiding `bundle/`
leaves them running (9 assertions) while the rest skip.

**Still outstanding — needs the author.** A verified bundle is not a deployed
one. Until `SHINY_DEPLOY=true Rscript shiny/adjudication_app/deploy.R` is run,
reviewers on shinyapps.io continue to see the April data. That is an
outward-facing publish to a shared application and has deliberately not been
done automatically.

---

## F12 🟡 ~~Publication metadata is not refreshed when a reviewer supplies a PMID~~ — **FIXED 2026-09-03**

**Failure.** `final_pmid = coalesce(manual_pmid, best_pmid)`, but `pub_title`,
`pub_journal`, `pub_year` and `months_to_pub` are joined on `best_pmid` in
`05_adjudicate.R`, which runs *before* the decisions are read. Where a reviewer
chose a different PMID (9 of the 178 published), the publication metadata and
`months_to_pub` describe the algorithm's candidate, not the reviewer's.

**Detection.** `sum(final_pmid != best_pmid, na.rm = TRUE)` among published rows
= 9.

**Fix applied.** `R/06_analyze_results.R` re-joins the publication fields on
`final_pmid` after `assign_final_published()` has run, filling them only where
the outcome is TRUE so a rejected candidate's metadata never appears on an
abstract counted unpublished. `scripts/rebuild_candidate_pool.R` was extended to
carry reviewer-supplied `manual_pmid` values so the join can resolve them — six
2018 abstracts had a reviewer PMID the search never returned and consequently no
date at all.

This also surfaced a related point: **seven confirmed publications appeared
before their congress** (four are `excluded` candidates a reviewer confirmed
anyway, one is a `definite` online-first paper two weeks ahead of the 2015
meeting, two are newly resolved 2018 reviewer PMIDs). They belong in the
numerator, because a reviewer ruled they are the abstract's publication, but a
negative interval is not a time to publication. Aim 2 now reports
`n_published`, `n_with_dates`, `n_pre_congress` and `n_undated` and computes the
median on post-congress publications only.

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

## F15 🟡 ~~Stale generated artefacts that no longer have a producer~~ — **PARTLY FIXED 2026-09-03**

**Fixed.** `result_positivity` was silently missing from `R/05_adjudicate.R`'s
`select()`, which gated off the Aim 5 publication-bias block from 2026-04-17
onward. It is restored, the block runs again, and
`output/aim5_publication_bias.csv` is current: 31.2% of negative-result
abstracts published against 20.2% of positive-result ones (OR 0.56 positive vs
negative), on 415 classifiable abstracts.

**Still outstanding.** `output/search_strategy_efficacy.csv` remains the
pre-correction measurement and will stay stale until the search layer is re-run;
`tests/testthat/test-cycle04_validation_sensitivity.R:179` holds it visible.
`output/excluded_pre_congress_publications.csv` still has **no producer in the
repository**. Three other orphans were closed on 2026-09-04:
`docs/data_inventory.csv`, `docs/data_dictionary.csv` and
`docs/DATA_DICTIONARY.md` now come from `scripts/build_docs_metadata.R`, and the
cohort-truncation evidence behind appendix A14 from
`scripts/audit_cohort_completeness.R`.

---

## F16 🟡 ~~External absolute paths outside the repository~~ — **FIXED 2026-09-03**

`R/10_npi_matching.R` hard-coded two absolute paths outside the repository.
Both now come from `config.yml: external_data` and are overridable with
`ABOG_NPI_PATH` and `NPPES_DUCKDB_PATH`.

Three further problems surfaced while fixing this, and the earlier audit note
that "neither path exists" was **wrong** — both files exist:

1. The missing-file guard called `invisible(NULL)`, which does **not** stop a
   sourced script. Execution fell straight through to `read_csv()` and the whole
   pipeline aborted at this step. The remainder of the script is now wrapped in
   `if (npi_pool_ok)`.
2. The ABOG `LATEST` symlink has been repointed upstream since the shipped
   `npi_matches.csv` was built. It now targets an ABOG *workforce* export:
   79,400 rows using `first`/`last`/`middle`/`subspecialty_name` instead of
   `first_name`/`last_name`/`middle_name`/`subspecialty`, **no gender column at
   all**, and an NPI on only **411 of 79,400 rows**. The loader now maps the
   renamed columns and names any it cannot find, so an upstream rename degrades
   rather than halting.
3. Running against that export yields 1 NPI and no gender, against 265 NPIs and
   256 genders in the shipped sidecar. The script now **refuses to overwrite a
   richer sidecar with a poorer one**, writing `npi_matches_nogender.csv` and
   explaining why.

**Gender no longer depends on that file (2026-09-04).** The ABOG gender column
was tier 1 of the waterfall, so its disappearance made a quarter of the gender
variable unreproducible. `R/09k_gender_from_nppes.R` now reads registrant-
reported sex from the NPPES registry keyed on the NPI that `10_npi_matching.R`
resolved, and it is the new tier 1; ABOG is tier 2 and still covers the four
abstracts NPPES leaves blank. NPPES resolves 263 of the 265 high-confidence
NPIs and agrees with ABOG on 251 of 252 (99.6%). The remaining ABOG dependency
is for `npi_state` and `npi_subspecialty`, neither of which is a model term.
See [AUTHOR_ENRICHMENT.md](AUTHOR_ENRICHMENT.md) §4.

The NPPES mirror is present at `$NPPES_DUCKDB_PATH` (84 GB); the
configured path lacks the volume suffix macOS assigns when more than one copy of
the drive is mounted, so the taxonomy fallback is skipped with a warning rather
than failing.
