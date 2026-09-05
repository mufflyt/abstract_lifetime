# Tests and Validation

Inventory of the test suite, plus an audit of which scientific claims are and
are not protected by a test.

**Current status** (`testthat::test_dir("tests/testthat")`, 2026-09-04,
27 test files):

```
[ FAIL 4 | WARN 18 | SKIP 1 | PASS 900 ]
```

All remaining failures are **deliberately left red**: each marks a decision that
belongs to the author rather than to code.

`test-cycle03_model_contracts.R:57` — which asserts that no reported odds ratio
spans a 100-fold interval — now passes, because the `has_funding` interval
narrowed from 0.117–29.04 to 0.293–11.345 when the event count rose from 104 to
171. The underlying concern has not gone away: `has_funding` is TRUE for **7 of
1,106** abstracts (it was 3 before `02d` re-derived the predictors from the
backfilled text), and an estimate from seven events is not interpretable
whatever its interval width. It should be reported as not estimable, or
dropped — `mysterycall_remove_near_zero()` identifies it automatically at a
frequency ratio of 1044:7, above the conventional 19:1 cutoff.

| Failing test | Reports | Why it is not fixed |
|---|---|---|
| `test-cycle04_validation_sensitivity.R:179` | `search_strategy_efficacy.csv` still carries the pre-correction 0.2% `title` yield | Regenerating it means re-running the whole search layer, which would change candidate sets and invalidate the human adjudication |
| `test-cycle06_scoring_composite.R:83` | `keyword_pts` fires on 0 of 1,106 abstracts | Fixing the component changes every composite score and therefore every classification, invalidating the adjudication |
| `test-cycle06_scoring_composite.R:116` | 3 PMIDs credited to 6 published abstracts | Deciding which abstract owns each PMID is adjudication. Surfaced as `final_pmid_shared` and `output/shared_publication_matches.csv`; see FAILURE_MODES.md F17 |

**Resolved and removed from the manifest, 2026-09-04.** The Cox
proportional-hazards entry (`test-pipeline_semantics.R::PH assumption holds`,
global p = 0.043) is gone. Per-term Schoenfeld tests attributed the whole
violation to `n_authors` (p = 0.002; every other term p ≥ 0.13), which is now
fitted with a log-time interaction; stratifying on it instead restores the
global test to p = 0.688 without moving any other hazard ratio more than 2%.
The assertion that replaced it is stronger, not weaker: a violation must be
attributed per term, named in a remedy, and the remedy must demonstrably restore
the assumption. See appendix A16 and
[STATISTICAL_ANALYSIS.md](STATISTICAL_ANALYSIS.md#proportional-hazards).

`tests/run_suite_gate.R` also now fails on a manifest entry naming a test that
never ran, which is the failure this rename would otherwise have introduced.

Fixed during this pass, each having been a genuine failure: the stale deploy
bundle (F11 — bundle refreshed; **the live shinyapps.io app is still stale and
needs `SHINY_DEPLOY=true Rscript shiny/adjudication_app/deploy.R`, which is an
outward-facing publish and is left to the author**), the gold-standard confusion
cells summing to 49 against a stated n = 50, `cohens_kappa` silently `NA`
because `irr` was not installed (now 0.994), `technical_appendix.Rmd` failing to
knit after `sensitivity_analyses.csv` gained a column, and an outdated
assertion in `test-cycle02_survival_estimand.R` that no confirmed publication
may carry a negative interval.

Before this work the suite stood at
`[ FAIL 1 | WARN 17 | SKIP 1 | PASS 519 ]` across 16 files.

**Note on the deploy-bundle tests.** They measure whether `bundle/` matches the
analysis, not whether the *deployed* application does. A green result means the
next deploy will ship current data; it does not mean reviewers are seeing it.

**And most of them do not run in CI.** `bundle/` is gitignored, so eight of the
eleven tests in `test-shiny_bundle_currency.R` skip on a fresh checkout — the
guard against the defect that actually happened had no CI protection at all.
`shiny/adjudication_app/bundle_manifest.csv` closes that: `deploy.R` writes the
checksum of every source at build time, the file is tracked, and three tests
compare the recorded checksums against the current tracked sources. Those three
(9 assertions) run everywhere. They answer the question that matters — *have the
sources moved since the last deploy?* — without needing the bundle itself.
`pubmed_candidates.csv` is ~130 MB and gitignored, so its row is recorded but
unverifiable in CI; a third test asserts that it is the *only* such row, so a
new unverifiable source cannot slip in unnoticed.
The pre-existing check in `test-shiny_app.R:458` compared modification times,
which a `touch` satisfies; `test-shiny_bundle_currency.R` compares content and
adds functional completeness checks. Nothing in the suite can reach
shinyapps.io.

---

## 1. Test-file inventory

| File | `test_that` blocks | Kind | Runs in CI | Can skip | Requires | Scientific invariant protected |
|---|---:|---|---|---|---|---|
| `test-decision_precedence_bva.R` | 23 | unit + contract, boundary-value | **CI gate 1** | Only the last two blocks, on missing output files | `R/utils_decisions.R`; `output/final_analytical_dataset.csv` and `aim1_publication_rate.csv` for the last two | AUTO-vs-human precedence; the four-branch outcome cascade; denominator arithmetic; that the exported rate is reconstructible |
| `test-decision_mutation.R` | 3 (drives 10+ planted mutants) | mutation | **CI gate 2** | No | `R/utils_decisions.R` | That the BVA battery still *detects* each defect it was written for. A surviving mutant fails the build. |
| `test-cycle01_thresholds_contracts.R` | 10 | unit + contract | Gate 3 | No | `utils_scoring.R`, `utils_congresses.R`, `utils_decisions.R`, `config.yml` | Threshold inclusivity at the cutoff; `Inf`/`NA` handling; cohort sizes 0 and 1; pre-conference short-circuit; date-vector length; order-invariance of dedup; every congress year has a config date |
| `test-pipeline_semantics.R` | 24 | semantic / integration | Gate 3 | Yes — every block guards on file existence | `abstracts_cleaned.csv`, `abstracts_with_matches.csv`, model RDS, `validation_metrics.csv`, `sensitivity_analyses.csv` | Row counts and years; no videos downstream; unique IDs; ID naming; classification vocabulary; score-to-class consistency; plausibility bounds against Cochrane MR000005; coverage floors; Cox validity; PH assumption; gold-standard sensitivity and NPV; cohort ID equality; pre-conference retention; sensitivity monotonicity |
| `test-shiny_app.R` | 27 | integration | Gate 3 | Yes — gitignored artefacts guarded by `skip_if_no_file()` | `shiny/adjudication_app/app.R`, the deploy bundle, `pubmed_candidates.csv` | App parses; required files exist; column contracts; abstract-text coverage by era; reactive behaviour; bundle freshness |
| `test-shiny_e2e.R` | 17, plus 1 that always runs | end-to-end (browser) | Gate 5 | **Opt-in: off unless `RUN_SHINY_E2E=true`** | `shinytest2`, Chrome, `pubmed_candidates.csv` | Full reviewer workflow. Excluded from CI by decision, not by accident: installing `shinytest2` on the runner would launch a browser against an app with no candidate pool to render, producing failures rather than coverage. The exclusion is registered in `tests/expected_skips.yaml`, and a floor assertion inside the file runs in every environment and fails if that entry is removed, so the gap cannot go unrecorded. |
| `test-utils_classify.R` | 32 | unit | Gate 3 | No | `R/utils_classify.R` | Study-design, research-category and procedure classifier behaviour |
| `test-utils_affiliation.R` | 26 | unit | Gate 3 | No | `R/utils_affiliation.R`, `teaching_hospital_names.txt` | Practice type, subspecialty, career stage, teaching-hospital match |
| `test-utils_states.R` | 10 | unit | Gate 3 | No | `R/utils_states.R` | US state and country parsing from affiliations |
| `test-utils_pub_types.R` | 8 | unit | Gate 3 | No | `R/utils_pub_types.R` | Publication-type canonicalisation priority |
| `test-utils_acog.R` | 7 | unit | Gate 3 | No | `R/utils_acog.R` | State → ACOG district mapping |
| `test-utils_scoring.R` | 6 | unit | Gate 3 | No | `R/utils_scoring.R` | Component scoring and `classify_match()` |
| `test-utils_congresses.R` | 6 | unit | Gate 3 | No | `R/utils_congresses.R`, `config.yml` | Congress date lookup |
| `test-utils_text.R` | 5 | unit | Gate 3 | No | `R/utils_text.R` | Title/author normalisation, Jaccard, keyword extraction |
| `test-utils_positivity.R` | 5 | unit | Gate 3 | No | `R/utils_positivity.R` | Result-direction classification |
| `test-utils_crossref.R` | 2 | unit | Gate 3 | No | `R/utils_crossref.R` | Query construction only — no network |
| `test-cycle02_survival_estimand.R` | 10 | BVA + semantic + adversarial | Gate 3 | Partially | `final_analytical_dataset.csv`, `km_fit.rds` | Kaplan–Meier set construction; the time-to-publication estimand and its population; per-year denominators; the join that builds the analytical dataset (found and fixed a missing duplicate-`abstract_id` guard in `assign_final_published()`) |
| `test-cycle03_model_contracts.R` | 10 | BVA + semantic + adversarial | Gate 3 | Partially | model RDS, `aim2b`/`aim3` CSVs | Logistic and Cox output contracts; the ≥50%-missing exclusion rule; complete-case attrition; determinism; artefact vintage. Added `n_obs` to `aim3_logistic_regression.csv` (1,010 against a denominator of 1,051 — 41 abstracts leave through complete-case deletion, previously invisible). |
| `test-cycle04_validation_sensitivity.R` | 10 | semantic + adversarial | Gate 3 | Partially | `validation_metrics.csv`, `sensitivity_analyses.csv`, `interrater_agreement.csv`, `search_strategy_efficacy.csv` | Gold-standard confusion-cell partition; sensitivity-scenario denominator consistency; interrater completeness; search-efficacy vintage |
| `test-docs_drift.R` *(added by this pass)* | 12 | contract | Gate 3 | Partially | `docs/*.csv`, the analytical outputs | Documentation-to-data agreement; see §4 |
| `test-model_stability.R` *(added 2026-09-04)* | 7 | contract | Gate 3 | Partially | `model_variable_screen.csv`, the stability outputs, the fitted models | That the screen records a reason for every candidate; that its kept set is exactly what the models were fitted on; that a near-zero-variance term is excluded by that named rule; that a never-present candidate is recorded rather than vanishing; that stability is reported for every fitted term and its label follows its number; that every congress is dropped for every term; and that `is_rct` and `n_authors` survive dropping any single congress |
| `test-mysterycall_integrations.R` *(added 2026-09-04)* | 9 | contract | Gate 3 | Partially | Table 1 outputs, `10e` helpers, the missingness outputs, the session snapshot | The four borrowed functions: Table 1 is stratified and its stratum sizes reconcile with the cohort; `safe_join()` blocks a duplicated sidecar key, preserves row count, and still creates columns from an empty-but-typed sidecar; missingness counts match the dataset; the MCAR row records what it did not test; `best_score` is labelled definitional; the snapshot records version, platform and seed |
| `test-gender_nppes_tier.R` *(added 2026-09-04)* | 6 | contract | Gate 3 | Partially | `gender_from_nppes.csv`, `npi_matches.csv`, `gender_resolution_policy.csv`, `abstracts_with_matches.csv` | The NPPES registry tier: population identity with the high-confidence NPI set, vocabulary, resolution rate, NPPES/ABOG agreement floor, that the policy file puts NPPES first and matches the coalesce order in the code, and that every row labelled `nppes` carries the sidecar's value |
| `test-shiny_bundle_currency.R` *(added by this pass)* | 9 | contract + end-to-end | Gate 3 | Yes — the bundle is gitignored | `shiny/adjudication_app/bundle/**`, `abstracts_cleaned.csv`, `match_scores.csv`, `pubmed_candidates.csv` | Whether the adjudication app serves the data the analysis was run on: md5 equality for the five verbatim files, cohort identity, no abstract under-served candidates, every winning PMID displayable, the candidate-to-score join resolves, and `deploy.R` gates publication behind `SHINY_DEPLOY`. Two tests drive the real server through `shiny::testServer()`. |
| `test-remediation_invariants.R` *(added by this pass)* | 8 | contract | Gate 3 | Partially | `abstracts_cleaned.csv`, `final_analytical_dataset.csv`, `pubmed_candidates.csv`, the aim CSVs | The defects fixed on 2026-09-03: no scraper footnote in `abstract_text`; no covariate structurally zero across a congress outside 2017–2018; `abstract_word_count` nonzero wherever text exists; the enrichment block survives a step-5 re-run; one subspecialty vocabulary; subgroup tables carry their availability split; sensitivity scenarios name their denominator; every winning PMID resolves; every published abstract carries a date; pre-congress publications are confined and excluded from Aim 2 |

CI (`.github/workflows/tests.yaml`) runs on every push and pull request, plus a
nightly cron, in three gates: the BVA contracts, then the mutation tests, then
the full suite. Gate 2 is unusual and worth keeping — it fails the build when
the suite *stops detecting* a defect, which a passing run alone cannot reveal.
`.github/workflows/R-CMD-check.yaml` runs the same suite again on `main`/`master`
without the gates.

**No network test exists.** Every API wrapper is tested on query construction
only. A live-API contract test would have caught the Europe PMC over-exclusion
described in [PUBLICATION_SEARCH.md](PUBLICATION_SEARCH.md) §1.

---

## 2. Scientific invariants

| Scientific invariant | Test exists? | Test file | Strength |
|---|---|---|---|
| No unexplained abstract loss between cleaned and analytical | **Yes** | `test-pipeline_semantics.R:276` — `expect_setequal(ids1, ids2)` | **Strong.** Full set equality, the assertion that was weakened while the denominator defect was live. |
| Unique abstract IDs | Yes | `test-pipeline_semantics.R:39` | Strong |
| ID naming convention | Yes | `test-pipeline_semantics.R:47` | Moderate — regex only |
| No videos downstream | Yes | `test-pipeline_semantics.R:29` | Strong |
| Publication cannot precede the congress unless explicitly classified | Yes | `test-pipeline_semantics.R:90, 139, 292` | **Moderate.** Asserts `excluded` rows have negative `date_pts` and that no *published* abstract has a negative `months_to_pub`. Does not catch the coarse-date misdating in F14. |
| One authoritative classification per abstract | Yes | `test-pipeline_semantics.R:59` (vocabulary) + `:39` (uniqueness) | Strong |
| Denominator reconciliation (cohort − pending = evaluated; published + not = evaluated) | **Yes** | `test-decision_precedence_bva.R:164, 208` | **Strong.** Asserted on synthetic boundary cases *and* on the shipped outputs. |
| Exported rate is reconstructible from exported counts | Yes | `test-decision_precedence_bva.R:223` | Strong — this is the test that forced `n_evaluated` into `aim1_publication_rate.csv` |
| Candidate score in range | Partial | `test-pipeline_semantics.R:68, 79` | Moderate — asserts definite ≥ 7 and no_match < 3; no explicit `[-5, 14]` bound |
| Publication-date validity | Partial | `test-pipeline_semantics.R:129, 139` | Moderate — bounds the median at 6–36 months and forbids negative times among the published |
| Missing-data mechanism | **Yes** | `test-mysterycall_integrations.R`, `R/06b_missingness.R` | Moderate. Little's MCAR is run and recorded, and the 55 unresolved are compared against the evaluated on every model covariate. Two substantive differences found. |
| Human-adjudication completeness | **No** | — | **GAP.** Nothing asserts that every cohort abstract has a decision, nor that decisions with no matching abstract are accounted for (47 video orphans). |
| One publication per abstract | **Yes** | `test-cycle06_scoring_composite.R:116` | **Strong, and currently violated by design** — 3 PMIDs are credited to 6 abstracts. Surfaced in `final_pmid_shared`. F17. |
| Final dataset grain | Partial | `test-pipeline_semantics.R:18` | Moderate — row count and year set, on `abstracts_with_matches.csv` |
| Model specification is recorded, not inferred | **Yes** | `test-model_stability.R` | **Strong.** `output/model_variable_screen.csv` gives a decision and a reason for every candidate, and the test asserts the kept set equals the fitted terms. |
| Regression findings are sample- and congress-robust | **Yes** | `test-model_stability.R` | Moderate. 500 bootstrap refits and 84 leave-one-congress-out refits; the test pins the two robust terms. |
| Model cohort reconciliation | **Yes** | `test-cycle03_model_contracts.R`, `test-remediation_invariants.R` | Moderate. `aim3_logistic_regression.csv` now exports `n_obs` (1,010 against a denominator of 1,051), and publication-date coverage among the published is asserted at ≥ 95% — it is now 178/178, was 104/178. |
| Manuscript numbers match generated output | Partial → **now yes** | `test-docs_drift.R` | Moderate — see §4 |
| Every congress year has a config date | Yes | `test-cycle01_thresholds_contracts.R:175` | Strong |
| Classifier boundary behaviour | Yes | `test-cycle01_thresholds_contracts.R:24, 41, 90` | Strong |
| Dedup is order-invariant | Yes | `test-cycle01_thresholds_contracts.R:151` | Strong |
| **Candidate pool matches what was scored** | **Yes** | `test-remediation_invariants.R`, `test-docs_drift.R` | **Strong, and now satisfied.** Every winning PMID resolves; cohort pairs 64,728 against 64,718 scored. Fixed by `scripts/rebuild_candidate_pool.R`. |
| **Cohort completeness against the source supplement** | **No** | — | **GAP — and it is currently violated.** Nothing compares the captured DOI set against Crossref. F1. |
| **Predictor derivation postdates the text backfill** | **Yes** | `test-remediation_invariants.R` | **Strong.** Asserts no text-derived covariate is 0% across a whole congress outside 2017–2018, and that no scraper footnote sits in `abstract_text`. Fixed by `R/02d_rederive_predictors.R`. |
| **Subgroup variables are not outcome-conditional** | **Partly** | `test-remediation_invariants.R` | Moderate. The rate is still emitted, but every row now carries the availability split and an `outcome_conditional_stratifier` flag, and the test asserts those columns exist. |
| **The demographics merge ran** | **Partly** | `test-remediation_invariants.R` | Moderate. The enrichment block's presence in `abstracts_with_matches.csv` is asserted, and `00_run_all.R` now calls `10e`. The automatic variable screen in `06` still drops an absent term silently. |
| Search failure is distinguishable from a zero result | **No** | — | **GAP.** No status is recorded, so no test is possible without a code change. F5. |
| Gender inference quality | Partial | `test-pipeline_semantics.R:156` (coverage ≥ 60%) | **Weak.** Coverage is not accuracy. Nothing tests the 228 conflicts or the 292 initial-only calls. |

---

## 3. Two coverage thresholds that were re-pointed

`test-pipeline_semantics.R:168` (`practice_type`) and `:184` (citation counts)
previously asserted ≥ 80% and ≥ 90%. Both were unsatisfiable rather than unmet:
`practice_type` can only exist for abstracts with a matched publication (17.5%
achieved), and a citation count only exists for a matched publication. They are
now regression floors against the achieved level. That is the right call, but it
means neither test can detect a *degradation* larger than the floor's slack.

---

## 4. The documentation-drift test added by this pass

`tests/testthat/test-docs_drift.R` asserts, against the live files:

1. Every script path named in `docs/pipeline_manifest.yml` exists.
2. Every file marked `authoritative` in `docs/data_inventory.csv` exists.
3. `output/final_analytical_dataset.csv` has exactly the documented dimensions
   (1,106 × 90).
4. The `classification` levels in the data are exactly the six documented.
5. `manual_decision` levels are exactly `match`/`no_match`/`skip`.
6. Every column of the final dataset appears in `docs/data_dictionary.csv`
   (no undocumented column).
7. Every variable in `docs/data_dictionary.csv` appears in the final dataset
   (no documented column has disappeared).
8. Every variable used in the fitted Cox and logistic models appears in the
   data dictionary.
9. The documented numerator and denominator (178 / 1,051) agree with
   `output/final_analytical_dataset.csv` and with
   `output/aim1_publication_rate.csv`.
10. The README headline agrees with the generated result.
11. `07`/`08`'s inline decision logic still agrees with
    `R/utils_decisions.R` on every row (guards F9).
12. The two known-violated invariants (F2's candidate-pool shortfall and F1's
    per-congress capture ceiling) are pinned at their **current** values, so
    they fail if they get worse *and* fail if they are fixed without updating
    the documentation.

Point 12 is deliberate. A test that simply asserted the correct invariant would
fail today and be muted; a test pinned to the current value keeps the defect
visible in every run and forces the documentation to be updated when it is
fixed.


---

## 5. The gold-standard confusion matrix

`output/validation_metrics.csv` reported `n = 50` while
`true_positives (13) + false_positives (13) + false_negatives (0) +
true_negatives (23) = 49`, and `accuracy` divided a numerator measured on the 49
classified rows by a denominator of 50. The 50th abstract carries `NA` in
`truth` or `predicted` and is dropped from all four cells by `na.rm`.

Fixed on 2026-09-03 in the parallel test workstream:
`R/validation_gold_standard.R` now exports `n_classified` alongside `n`, and
accuracy divides by `n_classified` (0.720 → **0.735**).

The remaining point to carry into the manuscript is **PPV = 0.50**: before human
adjudication, half the algorithm's positive calls are wrong. That is the
justification for the review step, and it belongs alongside the sensitivity
figure of 1.00 rather than behind it. `n = 50` is also small enough that every
metric has a wide interval; no interval is currently reported.
