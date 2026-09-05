# Changelog

All notable changes to this project are documented in this file.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
This project does not publish versioned releases; entries are dated by the work
they describe. `NEWS.md` carries the same history with fuller narrative, and
`docs/technical_appendix.Rmd` carries the methodological detail.

## [Unreleased]

## [2026-09-05] - Affiliation covariates derived from affiliations

### Changed

- **`is_academic` and `is_us_based` are derived from the author affiliation
  instead of abstract body text.** Neither had ever read an affiliation: the
  affiliation branch of each rule never fired because of the parser defect, so
  both fell through to the abstract body, where the academic pattern matched
  "residency", "fellowship" and "tertiary center" and the US pattern matched
  any state name anywhere in the text. Against real affiliations the two
  proxies agreed 54.1% and 52.7% of the time.
- **The academic finding does not survive.** HR 0.649 (p = 0.013) becomes 1.097
  (p = 0.587); bootstrap retention falls from 47.4% to 5.6%. Reported for three
  refits as "academic affiliation is associated with slower publication", it
  was an artefact of a proxy that agreed with its construct about as often as a
  coin. It should now be reported as no evidence of an association.
- `is_us_based` reverses direction, 1.354 to 0.787, and remains non-significant
  either way. `is_rct` and `n_authors` were never affiliation-derived and
  survive at 93.2% and 88.6% retention.
- The publication rate, denominator and time to publication do not change.

### Added

- `data/processed/abstract_affiliations.csv`, the union of institution strings
  per abstract. Per-author linkage needs a refid the 2017-2018 ScienceDirect
  format does not emit and reaches 55% of the cohort; abstract-level covariates
  do not need that link and this reaches **1,092 of 1,106 (98.7%)**, including
  2017 and 2018 at 100%.
- `n_affiliations`, the count of distinct institutions per abstract: mean 2.37,
  679 abstracts naming more than one.

### Fixed

- Absence of an affiliation is now `NA`, not `FALSE`. Coding it as a negative
  filled the comparison group with unknowns and produced an era gradient that
  made an earlier attempt at this change unusable.
- `scripts/ph_diagnostics_report.R` assumed a single numeric violator. With two
  violators it selected the categorical one and silently produced no AIC or
  ceiling metrics. It now picks the first numeric violator for those, reports
  how many terms violate, and drops every violator for the "without violator"
  refit rather than one.

### Note

- **A second proportional-hazards violator.** Global p moves from 0.043 to
  0.010; `is_us_based` now violates at p = 0.005 alongside `n_authors` at
  p = 0.003. The remediation handled it unchanged, choosing by type: a log-time
  interaction for the numeric term, `strata()` for the logical one. Stratifying
  on both restores the global test to 0.607.
- `is_multicenter` is deliberately unchanged at 65. More than one affiliation
  is not a multicentre study, and redefining it as `n_affiliations > 1` would
  take it from 65 to 679 and change what the variable means. That is an
  estimand decision, not a measurement repair.

## [2026-09-05] - A human no_match supersedes the algorithm

### Changed

- **PI decision: a human `no_match` overrides a `definite` classification.**
  The branch now sits above `classification == "definite"`, extending the
  principle already used in `dedup_decisions_for_analysis()`, that a person
  outranks a prefill, one level further so that a person also outranks the
  score.
- **The numerator moves from 171 to 170** and the rate from 16.3% to 16.2%
  (95% CI 14.1-18.5). Unlike the pre-congress decision this one also moves the
  median time to publication, 13.7 to 13.6 months (IQR 5.7-22.4), because the
  abstract removed had an interval of 34.6 months.
- The rule is restricted to human decisions. Of the four abstracts counted
  published against a `no_match`, only one is human: AAGL2021_030, where R01
  recorded `no_match`, R02 recorded `match` and then `no_match` five days
  later. The other three carry AUTO rows whose notes record
  `classification=reject`, a vocabulary this pipeline no longer uses, against a
  current classification of `definite`. They are fossils of a superseded
  scoring run, and letting them override would give an old run authority over
  the current one.

### Fixed

- `assign_final_published()` needs `reviewer` to tell a human decision from a
  prefill. Joined and left in place it widened the analytical dataset from 93
  columns to 94 and tripped the documentation-drift test; it is now dropped
  again unless the caller already had it.

### Changed (tests)

- `test-cycle22_decision_log.R` required the outcome column to agree with the
  surviving decision for every abstract. Two decisions now break that on
  purpose, so it asserts the narrower contract: every divergence must be
  explained either by the pre-congress rule or by the AUTO exemption, and a
  human `no_match` counted published is still a failure. A companion test
  counts the stale AUTO rows and fails if more than the three known appear.
- The BVA gate's "definite overrides a human no_match" case is inverted, and
  two cases added: an AUTO `no_match` must NOT override, and a human
  `no_match` must not override the pre-congress rule, which is an eligibility
  test rather than a judgment about the match.

### Note

- `is_academic` bootstrap retention fell again, 50.4% to 47.4%. It now survives
  fewer than half of resamples.
- Manifest down to 21; the cycle22 decision-log entry is resolved and removed.

## [2026-09-05] - The pre-congress exclusion is absolute

### Changed

- **PI decision: a reviewer's `match` does not override the pre-congress
  exclusion.** A publication that appeared before the congress cannot be a
  conference-to-publication conversion. The test now runs as the first branch
  of the outcome cascade, ahead of both `classification == "definite"` and the
  reviewer verdict, and is applied to the print issue date of the publication
  actually credited to the abstract.
- **The numerator moves from 178 to 171** and the rate from 16.9% to 16.3%
  (95% CI 14.2-18.6). The denominator is unchanged at 1,051: an excluded
  abstract stays in the cohort counted unpublished. The median time to
  publication is unchanged at 13.7 months, because these abstracts had negative
  intervals and were already outside the time-to-event analysis. The Cox model
  gains seven observations as they become censored.
- `output/aim2_time_to_pub.csv` now reports `n_pre_congress = 0`, down from 7.
  That is the rule verifying itself.
- `R/07_make_tables.R` and `R/08_make_figures.R` adopt the settled outcome from
  the analytical dataset instead of recomputing it. They recomputed through the
  same cascade but without 06's date refresh, so after this change they would
  have disagreed with the analysis on two abstracts.

### Added

- `scripts/pre_congress_exclusions.R`. The evidence file had no producer and
  had been built by hand from electronic publication dates while the analysis
  measured from print issue dates. It now regenerates on the decided basis: 42
  abstracts, against 39 in the hand-built file, and fails if any is still
  counted as published.
- `apply_pre_congress_exclusion()` and `adopt_analysis_outcome()` in
  `R/utils_decisions.R`, and eight boundary cases in the BVA gate covering the
  congress date itself, a publication one day before it, and the two overrides
  the decision removes.
- Appendix A19.

### Fixed

- Testing `classification == "excluded"` would have missed three abstracts. Two
  carry a reviewer PMID other than the scored best candidate, so the
  pre-conference penalty had been computed against a paper that was not the one
  counted; one was scored `definite` despite its credited paper predating the
  congress by two weeks.

### Note

- `is_academic` bootstrap retention fell from 62.0% to 50.4%. Already described
  as not sampling-robust, it now survives about half of resamples and should be
  read as suggestive at most.

## [2026-09-05] - The publication date is the print issue date

### Changed

- **PI decision: the publication date is the print issue date**
  (`JournalIssue/PubDate`), for the time-to-publication interval and for
  whether a paper preceded its congress. Articles released online ahead of
  print are dated to their print issue; month-only issue dates resolve to the
  first of the month. The Methods in `docs/abstract_results_section.Rmd`
  previously said only "the publication date", which was ambiguous between two
  PubMed fields that differ by months.
- **No number changes.** The analysis already measured from the print issue,
  so the rate, the median time to publication and every model are unaffected.
  What changes is that the definition is now stated and enforced.

### Added

- `tests/testthat/test-publication_date_basis.R`. It fails if the parser stops
  reading `JournalIssue/PubDate` or assigns the date from `ArticleDate`, if the
  month-only default changes, if the Methods stop defining the date, or if any
  of the four contested pre-congress intervals stops matching issue-date
  arithmetic. The two bases differ by 1.5 to 4.9 months on those abstracts, so
  a recorded interval identifies its own provenance.
- Appendix A18.

### Known issue, not fixed here

- `output/excluded_pre_congress_publications.csv` was built from `ArticleDate`
  and is on the wrong basis. It is evidence rather than an input to any
  reported figure, so nothing downstream is wrong, but it must be regenerated
  before the branch-order question is settled: the two bases identify different
  sets, and whether that open decision concerns four abstracts or seven depends
  on it.
- Whether a reviewer `match` should override a pre-congress exclusion remains
  open. That is what moves the numerator between 178 and 174, and it is not
  decided here.

## [2026-09-05] - Single-sourced registry, and a producer for the PH figures

### Fixed

- **`docs/VALIDATION.md` still carried a hand-maintained table of the registered
  failures, stopped at three rows while the manifest holds twenty-three.** The
  README's copy went in the same-day test-governance work; this removes the
  other one. Both now link to the generated
  [`docs/DECISIONS_PENDING.md`](docs/DECISIONS_PENDING.md).
- Counts stated in prose are now written as markers
  (`<!--manifest-count-->23<!--/manifest-count-->`) and checked against the
  manifest, so they cannot silently go stale the way the tables did.

### Added

- `scripts/ph_diagnostics_report.R` and `output/cox_ph_support.csv`. The numbers
  justifying the proportional-hazards remediation (Cox frame size, the global
  Schoenfeld test with the violating term removed, the AIC of both fits, the
  Schoenfeld residual correlation, the share of the frame at the author-count
  ceiling) were computed ad hoc and typed into prose. A number with no producer
  cannot be re-derived, so nothing noticed when one stopped being true, and all
  of them had shifted at the previous refit. Seven are now registered in
  `docs/manuscript_claims.csv`.
- `tests/testthat/test-decision_registry.R`: marked counts must match their
  manifest, the generated registry must hold one entry per manifest entry and
  keep its do-not-edit banner, and a table of manifest rows reappearing in
  either document fails the suite. The guard requires a line number in the row,
  so the legitimate test inventory in VALIDATION.md is not caught by it.
- Figure S4, match classification by congress year, added to the README beside
  S2 and S3. It shows the classification mix does not drift across the twelve
  years the single search pass covered.
- Appendix A17, including the distinction the count guard exposed on its first
  run: the skip manifest holds 13 registrations while 12 skips were observed,
  and those are different quantities.

### Changed

- `docs/_meta/data_inventory_meta.csv` gained entries for `cox_ph_support.csv`
  and `output/candidate_pool_index.csv`, the latter committed earlier without
  metadata. The builder's own guard caught both.

## [2026-09-05]: CI stops counting skipped tests as coverage; cycles 17-24

### Fixed

- **A skipped test is no longer read as a passing one.** The suite gate
  classified results as `failed == 0 & error == 0`, which is true of a skipped
  test. `test-cycle15_backfill_contract.R` read the gitignored PubMed XML cache,
  so in CI it skipped, the gate counted the skip as a pass, and then reported the
  test's expected-failure entry as stale. `main` was red for two days with no
  correct fix available. `tests/gate_rules.R` now classifies skips separately,
  and `R/02b_backfill_abstract_text.R` writes `output/backfill_coverage.csv` so
  the assertion reads a committed artefact and fails identically everywhere.
- **`test-utils_classify.R` had two `test_that` blocks with the same name.** Both
  manifests key on `file :: test`, so that key was ambiguous and an entry could
  have excused a different assertion than the one it was written for.

### Added

- **`tests/expected_skips.yaml` and a skip guard.** The gate now fails when a
  test skips without an approved entry, and prints every skip with its reason.
  Enforced in one direction only: an unapproved skip fails, an approved skip
  that runs anyway does not, because the skip set is legitimately
  environment-dependent. Each entry records why it cannot run and what would
  make it run.
- **`output/candidate_pool_index.csv`**, a 1.4 MB `abstract_id`/`pmid`
  projection of the gitignored 130 MB candidate pool, covering 65,697 pairs.
  This activates seven previously inert checks, including
  `F2: every winning PMID resolves in the candidate pool`, a central pipeline
  invariant that had never run in CI.
- **`docs/TEST_GOVERNANCE.md`**, documenting the gate, both manifests, and the
  rule that authoritative gate results come from a clean git worktree.
- **`docs/DECISIONS_PENDING.md`**, generated from the expected-failure manifest
  by `R/generate_decisions_pending.R`, with a currency test.
- **Cycles 17-24 of the test-generation loop**, 165 assertions across eight
  files, completing the 24-cycle protocol. Targets: the candidate-generation
  layer, the missingness and model-stability diagnostics, congress date
  resolution, the gender resolution waterfall, `abstract_id` integrity across 36
  artefacts, the reviewer decision log, validation and match fidelity, and the
  coherence of the governance layer itself.

### Changed

- **The Shiny bundle is verified from its committed manifest.** 45 assertions
  read the gitignored 47 MB bundle and skipped in CI. `bundle_manifest.csv`
  records each source's md5 and byte count at deploy time, so a source that
  still matches it is byte-identical to its copy. Drift now produces three CI
  failures where CI previously reported nothing.
- **`test-shiny_e2e.R` is opt-in.** It contributed zero assertions everywhere
  because `shinytest2` is installed nowhere, while still appearing as a test
  file. It now requires `RUN_SHINY_E2E=true`, and a floor assertion runs in every
  environment and fails if the exclusion is unrecorded.
- **`config/ci_contract.yml` declares all three workflows and five gates.**
  `manuscript.yaml` had been undeclared since it was added. The contract test now
  also fails on a workflow that exists but is not declared, and on a declared
  gate that no workflow invokes.
- **`manifest.max_entries` raised from 20 to 24**, deliberately and with the
  reason recorded. A ratchet to bring back down as decisions close, not headroom.

### Measured

- Assertions that ran only on a developer machine: **75 to 23**. Fifty-two are
  now enforced in CI.
- Approved skips: **20 to 13**.
- Suite in a clean worktree: 56 files, 1,723 passing, 23 registered failures,
  12 approved skips.

### Open

Eight decisions were surfaced and left to the author, taking the
expected-failure manifest from 16 to 23 entries. Two touch a reported number:
four abstracts with a pre-congress publication and four with a human `no_match`
are both counted as published, in the numerator of 178. See
`docs/DECISIONS_PENDING.md`.


## [2026-09-04] — Proportional hazards resolved

### Fixed

- **The Cox proportional-hazards violation is diagnosed and remediated.** The
  global Schoenfeld test (p = 0.043) had been recorded as an open methodological
  decision since the previous day. Per-term tests, now written to
  `output/cox_ph_terms.csv`, show the violation is confined to a single
  covariate — `n_authors`, p = 0.002, with every other term between p = 0.13 and
  p = 0.84 — and that refitting without it returns the global test to p = 0.497.
  `n_authors` is fitted with a log-time interaction rather than stratified away,
  because it is one of only two predictors that survive bootstrap resampling and
  stratifying would have discarded the effect to fix the diagnostic. AIC 2276.2
  against 2284.1 for the proportional fit.
- **The results Rmd no longer asserts what it has not checked.** The Results
  paragraph in `docs/abstract_results_section.Rmd` hard-coded "proportional
  hazards assumption met" and named US-based affiliation as the only significant
  predictor. Both had gone stale: the assumption is violated, and the current
  significant predictors are randomized design, academic affiliation and author
  count. The paragraph is now derived from the fitted model, including the
  significance wording, so it cannot drift again. A p-value of 0.083 no longer
  renders as "was associated with".
- `docs/STATISTICAL_ANALYSIS.md` listed `has_funding` in the Cox formula while
  the screen table two sections above recorded it as removed. The formula shown
  is now the one read back from the fitted model.

### Added

- `output/cox_ph_terms.csv` — per-term Schoenfeld tests, so a global violation
  can be attributed rather than just reported.
- `output/aim2b_cox_regression_timevarying.csv`,
  `data/processed/cox_model_timevarying.rds` — the log-time fit.
- `output/cox_time_varying_hr.csv` — the hazard ratio at 3, 6, 12, 24, 36 and 48
  months. Team size has no detectable effect on early publication (HR 1.01 at
  3 months) and a substantial one later (1.51 at 24 months, 1.74 at 48).
- `output/aim2b_cox_regression_stratified.csv`,
  `data/processed/cox_model_stratified.rds` — stratifying on the violator
  instead, as the sensitivity analysis for the five covariates that did not
  violate. Global p = 0.688 and no hazard ratio moves more than 2%.
- Figure 8, `output/figures/figure8_timevarying_n_authors.png`, plotting the
  time-varying hazard ratio against the constant the main model reports.
- `cox_ph_assumption.csv` gains `violating_terms`, `remediation` and
  `remediated_global_p`. Row 1 and the `p_value` column are unchanged, because
  downstream readers index them positionally.

### Changed

- `test-pipeline_semantics.R::PH assumption holds` is retired and replaced by
  two tests that are strictly stronger: any PH violation must be attributed per
  term and named in a remedy, the remedy must restore the assumption, and no
  non-violating hazard ratio may move more than 15% or change direction when the
  violator is stratified out.
- `tests/run_suite_gate.R` now also fails when a manifest entry names a test that
  never ran. The gate already caught entries that start passing; a renamed test
  would have left its entry behind describing a decision with no assertion
  attached to it.
- The expected-failure manifest is down to three entries.

## [2026-09-04]

Documentation audit, then remediation. Full narrative in
`docs/technical_appendix.Rmd` section A15; mechanism for each entry in
`docs/FAILURE_MODES.md`.

**The headline did not move.** 178 / 1,051 = 16.9% (95% CI 14.8–19.3) before and
after. What moved is the evidence beneath the time-to-event and regression
results, and how much of the pipeline can be rebuilt on another machine.

### Added

- `docs/` reference set: `COHORT_ASSEMBLY`, `PIPELINE`, `PUBLICATION_SEARCH`,
  `MATCHING_ALGORITHM`, `OUTCOME_DEFINITION`, `ADJUDICATION`, `DATA_DICTIONARY`,
  `DATA_INVENTORY`, `AUTHOR_ENRICHMENT`, `STATISTICAL_ANALYSIS`,
  `RESULTS_PROVENANCE`, `SOURCE_OF_TRUTH`, `REPRODUCIBILITY`, `FAILURE_MODES`,
  `VALIDATION`, `METHODOLOGICAL_HISTORY`, plus `data_inventory.csv` (83 files),
  `data_dictionary.csv` (92 variables) and `pipeline_manifest.yml` (44 stages).
- `scripts/build_docs_metadata.R` — the producer for `data_inventory.csv`,
  `data_dictionary.csv` and `DATA_DICTIONARY.md`, which were previously
  committed with no producer at all. Joins hand-authored prose in `docs/_meta/`
  to counts recomputed from the live tree, and fails if the two disagree about
  which files or columns exist.
- `scripts/rebuild_candidate_pool.R` — repairs `pubmed_candidates.csv` from
  `match_scores_detailed.rds` plus reviewer-supplied PMIDs. Resumable.
- `scripts/audit_cohort_completeness.R` — measures ingestion against the
  Crossref deposit. The evidence behind appendix A14.
- `R/02d_rederive_predictors.R` — recomputes the text-derived covariates after
  the backfills.
- `R/06b_missingness.R` — item-level missingness, Little's MCAR test, and a
  comparison of the 55 unresolved abstracts against the evaluated set.
- `R/06c_session_snapshot.R` — records R version, platform, seed and package
  versions.
- `R/06d_model_stability.R` — bootstrap predictor retention and
  leave-one-congress-out refits.
- `R/09k_gender_from_nppes.R` — registrant-reported sex from NPPES, the new
  tier 1 of the gender waterfall.
- `output/model_variable_screen.csv` — a decision and a reason for every model
  candidate, so the specification is reconstructible from the outputs.
- `output/shared_publication_matches.csv`, `unresolved_vs_evaluated.csv`,
  `missingness_*`, `model_predictor_stability.csv`,
  `model_leave_one_congress_out.csv`, `session_snapshot.txt`,
  `cohort_completeness_audit.csv`.
- Figure 7, predictor stability; figure 1 STROBE flow chart and supplementary
  figure S1 now tracked and embedded in the README.
- Six test files: `test-docs_drift`, `test-remediation_invariants`,
  `test-shiny_bundle_currency`, `test-gender_nppes_tier`,
  `test-mysterycall_integrations`, `test-model_stability`.
- `mysterycall` as an optional dependency, pinned at `42d66d92`. Every use
  degrades to the previous behaviour without it.

### Fixed

- **Stale candidate pool.** `03b` rewrote `pubmed_candidates.csv` after `04` had
  scored, leaving 283 of 1,102 winning PMIDs unresolvable. Published abstracts
  with a publication date **104/178 → 178/178**; Kaplan–Meier and Cox events
  **104 → 171**; median time to publication 13.8 mo (IQR 6.3–25.0) →
  **13.7 mo (IQR 5.7–22.6)**.
- **Covariates derived before the abstract text existed.** `02` ran before the
  backfills, so 2012–2018 predictors came from the title alone. Cohort counts
  TRUE: `is_academic` 148 → 371, `is_us_based` 689 → 907, `is_rct` 71 → 98,
  `is_multicenter` 38 → 65, `has_numeric_results` 276 → 622. The residual
  gradient is now confined to 2017 and 2018, which have no recoverable text.
- **96 degenerate `abstract_text` values** cleared — the footnote
  `"*: Corresponding author."` written into all 95 abstracts of the 2018
  congress, which passed the `nchar >= 10` gate that decides a row needs no
  backfill. Score-neutral: all 96 already scored `abstract_pts == 0`.
- **`05_adjudicate.R` no longer deletes 41 enrichment columns** when re-run.
- **`00_run_all.R` now runs the demographics merge**, which was reachable only
  through `run_demographics.R`.
- **`07` and `08` source `utils_decisions.R`** rather than each carrying an
  inline outcome cascade missing the human-over-AUTO rule.
- **`result_positivity` restored** to `05`'s select; the Aim 5 block had been
  silently gated off since 2026-04-17.
- **`subspecialty_unified` harmonised** from 13 levels to 8.
- **NPI paths moved to `config.yml: external_data`**; the missing-file guard
  called `invisible(NULL)`, which does not stop a sourced script, so a missing
  pool aborted the pipeline.
- **Shiny deploy verifies before publishing** and is opt-in behind
  `SHINY_DEPLOY`. The bundle was 135 days stale.
- **`technical_appendix.Rmd` knits again** after `sensitivity_analyses.csv`
  gained a column that broke a hard-coded `kable()` header.

### Changed

- **Gender waterfall is 11 tiers led by a registry.** NPPES registrant-reported
  sex is tier 1; ABOG is tier 2. They agree on 251 of 252. Coverage 1,065 →
  1,066, but **11 abstracts moved from a name-inferred tier to a registry
  tier** and four values changed, three replacing a name inference and two of
  those resting on a single first initial.
- **Model variable screen is recorded, not inferred.** A near-zero-variance
  criterion was added; `has_funding` (TRUE for 7 of 1,051) is now excluded by
  rule. `log_sample_size` is recorded as **absent** from the Cox model frame —
  it has always been a listed candidate and has never once entered the model.
- **Table 1** is now stratified with p-values: 35 rows over 10 variables,
  against two rows over five with no test.
- **Sensitivity scenarios name their denominator**; rows 1–2 divide by the
  cohort and 3 onward by the evaluated set.
- Subgroup tables carry `availability_among_published` /
  `availability_among_unpublished` and an `outcome_conditional_stratifier` flag.

### Known issues

- **Cohort truncation.** ScienceDirect ingestion captured 1,154 of 7,711
  supplement items (15%); every congress is at or below 100. Ten congresses
  captured no video presentations at all, meaning the window closed inside the
  oral block and an unknown number of oral presentations were never ingested.
  Verified against Crossref. Not remediated: re-ingesting requires redoing the
  search and the human adjudication.
- **The 55 unresolved abstracts are not missing completely at random** — they
  differ on `study_design` (p = 0.0004) and `n_authors` (p = 0.013). Bounds
  16.1%–21.1%.
- **Only two predictors are robust to resampling** — `n_authors` (97.2%) and
  `is_rct` (93.6%). `is_academic` survives 67.4%.
- **Three PMIDs are credited to two abstracts each.** One is counted published
  against an explicit reviewer `no_match`.
- `keyword_pts` is 0 for every abstract, so the "10-component" score has nine
  live components. Fixing it would invalidate the adjudication.
- The live Shiny app still serves April data until someone deploys.


### Added

- `pub_issue` extracted from `JournalIssue/Issue` in PubMed XML, required by the
  corrected supplement test.
- `gender_conflict` and `gender_n_sources` columns, populated for every row,
  recording cross-source agreement rather than only the winning value.
- `data/processed/gender_conflicts.csv` — 228 cross-source gender disagreements.
- `data/processed/gender_from_openalex.csv` — 157 resolutions.
- `data/processed/gender_from_open_payments.csv` — 16 resolutions.
- `output/final_analytical_dataset.csv` — unified 1,106 x 90 analytical dataset
  exported by `06_analyze_results.R`.
- `docs/aagl_abstract_programmatic.Rmd` / `.docx` — programmatic abstract draft.
- Technical appendix section A12, documenting the matching corrections below.
- `CHANGELOG.md` (this file).

### Fixed

Four matching defects, all biasing in the same direction — each suppressed true
abstract-to-publication matches. Detail in appendix A12.

- **Session-number title prefixes** (A12.1). The 2013, 2017, 2018, and 2021
  congress programs prefix titles with a session number, which entered the
  PubMed title phrase and matched nothing. Because the affected congresses are a
  non-random subset of years, this confounded the reported time trend.
- **Non-article publication types** (A12.2). Letters, comments, editorials,
  errata, and retractions were eligible candidates; they carry the title and
  authors of the paper they discuss and can outrank the genuine publication.
- **Over-broad JMIG supplement exclusion** (A12.3). The test matched on journal +
  volume + year, excluding every regular JMIG article sharing a volume with the
  congress supplement — precisely where AAGL abstracts are likeliest to publish.
  The most consequential of the four.
- **Title phrase search destroyed by stopword removal** (A12.4). Dropping tokens
  under three characters does not shorten a phrase; it produces a word sequence
  that appears in no title, breaking the pipeline's highest-precision strategy.

### Changed

- Processed data, Cox/KM/logistic models, and result tables regenerated against
  the corrected pipeline.
- Figure set renamed to `figure2_km_curve`, `figure3_km_by_year`,
  `figure4_subgroup_rates`, `figure5_cox_forest`, `figure6_time_to_pub`, and
  `figureS1`–`figureS4`.
- Main figures are now tracked and embedded in `README.md`.

### Removed

- Stale `figure2_time_to_pub`, `figure3_km_curve`, `figure4_strategy_perf`, and
  `figure5_score_dist` files, superseded by the rename above.

### Fixed (analysis)

- **Denominator defect ([#2](https://github.com/mufflyt/abstract_lifetime/issues/2)).**
  `R/05_adjudicate.R` dropped abstracts whose best candidate predates the
  conference (`classification == "excluded"`) out of the cohort. That
  classification describes the *candidate*, not the *abstract*. The 39 affected
  abstracts are now retained and counted as unpublished; no downstream change
  was needed, since `06`, `07`, `08` and the Shiny app already map `excluded` to
  `published = FALSE`.

  Publication rate **17.2% → 16.9%** (95% CI 14.8–19.3); cohort 1,067 → 1,106;
  published 174 → 178. Of the 39, 35 are unpublished and **4 carry an explicit
  reviewer `manual_decision == "match"`** — the filter was discarding confirmed
  publications from the numerator as well as non-events from the denominator,
  which is why the rate moves by 0.3 points rather than the 0.6 the denominator
  change alone predicts. Tables, figures, and the STROBE diagram regenerated.

### Fixed (CI)

- `test-shiny_app.R` read gitignored artefacts (`pubmed_candidates.csv`, the
  deploy bundle) with no existence guard, so it could never pass in a fresh
  `actions/checkout`. Guarded with `skip_if_no_file()`.
- `practice_type` coverage asserted >= 80% against ~18% achieved, and citation
  coverage asserted >= 90% across all abstracts when a citation count only
  exists for matched publications. Both were unsatisfiable rather than unmet;
  re-pointed at regression floors.

### Known issues

- Three defects found during the 2026-09-03 documentation audit are recorded in
  `docs/FAILURE_MODES.md` and have **not** been fixed: the supplement listing is
  truncated at ~100 items per congress (F1); `pubmed_candidates.csv` is a stale
  subset of the pool that was scored, so 74 of 178 published abstracts have no
  publication date (F2); and the text-derived predictors were computed before
  the abstract-text backfill, producing a step change at 2018/2019 in five model
  variables (F3).
- The four matching corrections shipped in a single re-run; their individual
  contributions are not separately identified and no ablation was performed.
- Supplement detection still falls back to a November-month heuristic where
  PubMed omits the issue field.
- `10g_second_author_triangulation.R` returns zero rows and contributes nothing
  to the gender waterfall.
- ~~Three `test-pipeline_semantics.R` failures predate this work and remain.~~
  Resolved as of 2026-09-03: the two coverage thresholds were re-pointed at
  regression floors and the row mismatch was the denominator defect, now fixed.
  `test-pipeline_semantics.R` passes in full. The suite's single remaining
  failure is `test-shiny_app.R:458` — the deploy bundle is stale.

## [2026-04-19]

### Changed

- `10e_merge_demographics.R` became the writer of the unified demographics
  block in `abstracts_with_matches.csv`; the 12 demographic producer scripts
  write sidecar CSVs only. (Correction, 2026-09-03: the file has six writers in
  total — `01d`, `05`, `09b`, `09d`, `09e`, `10e`, plus an inline block in
  `00_run_all.R`. See `docs/SOURCE_OF_TRUTH.md`.)
- Single `gender_unified` column replaces the dual
  `first_author_gender`/`gender_unified` pair, resolved by a 10-tier priority
  waterfall. Coverage 73.9% → 99%.
- PubMed-derived demographics for non-confirmed matches are blanked reversibly
  via a `demographics_from_matched_pub` flag rather than destructive `NA`.

### Added

- `R/09i_gender_from_openalex.R`, `R/09j_gender_from_open_payments.R`,
  `R/10g_second_author_triangulation.R`, `R/run_demographics.R`.
- `shiny/adjudication_app/deploy.R` — bundle preparation and shinyapps.io deploy.
- NPPES taxonomy fallback (`207V%`), middle-initial scoring, city matching, and
  temporal scoring in NPI resolution. High-confidence NPIs 248 → 278; state
  coverage 10% → 31%; subspecialty 17% → 36%.
- Appendix A10.11, coauthor triangulation for name disambiguation.

## [2026-04-18]

### Added

- `R/09f_enrich_gender_from_pubmed.R`, `R/09g_gender_from_orcid.R`,
  `R/09h_gender_from_obgyn_pubs.R`.

## [2026-04-17]

### Added

- Initial 12-congress pipeline (2012–2023), 1,070 oral abstracts.
- Six-strategy PubMed search, four supplementary databases, and DOI-chain
  reverse citation search.
- Ten-component composite scoring with Cochrane MR000005 five-tier
  classification.
- NPI matching against the ABOG pool (60,846 board-certified OB/GYNs).
- Shiny adjudication app with Google Sheets backend.
- Manuscript and technical appendix with inline R.
