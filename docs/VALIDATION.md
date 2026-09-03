# Tests and Validation

Inventory of the test suite, plus an audit of which scientific claims are and
are not protected by a test.

**Current status** (`testthat::test_dir("tests/testthat")`, 2026-09-03 16:50,
full working tree, 21 test files):

```
[ FAIL 3 | WARN 17 | SKIP 1 | PASS 627 ]
```

All three failures report real problems; two are deliberately left red.

| Failing test | Reports | Cross-reference |
|---|---|---|
| `test-shiny_app.R:458` | The deploy bundle is 135 days behind `data/processed/` | [FAILURE_MODES.md](FAILURE_MODES.md) F11 |
| `test-cycle03_model_contracts.R:57` | `has_funding` OR spans 0.12–29.04 from 3 TRUE abstracts — not estimable rather than not significant. **Left red** pending a reporting decision. | [STATISTICAL_ANALYSIS.md](STATISTICAL_ANALYSIS.md) §Diagnostics |
| `test-cycle04_validation_sensitivity.R:179` | `search_strategy_efficacy.csv` still carries the pre-correction 0.2% `title` yield. **Left red** until the search layer is re-run. | [PUBLICATION_SEARCH.md](PUBLICATION_SEARCH.md) §8 |

The single skip is the browser end-to-end suite, which needs `shinytest2`.

Two further failures existed earlier on 2026-09-03 and were fixed during this
pass by a parallel workstream: the gold-standard confusion cells summing to 49
against a stated n = 50 (now exported as `n_classified`; accuracy 0.720 →
0.735), and `cohens_kappa` silently `NA` because `irr` was not installed
(κ = 0.994).

Before this documentation pass the suite stood at
`[ FAIL 1 | WARN 17 | SKIP 1 | PASS 519 ]` across 16 files. The test suite is
under active development, so counts move; the invariant table in §2 is the
durable part of this document.

---

## 1. Test-file inventory

| File | `test_that` blocks | Kind | Runs in CI | Can skip | Requires | Scientific invariant protected |
|---|---:|---|---|---|---|---|
| `test-decision_precedence_bva.R` | 23 | unit + contract, boundary-value | **CI gate 1** | Only the last two blocks, on missing output files | `R/utils_decisions.R`; `output/final_analytical_dataset.csv` and `aim1_publication_rate.csv` for the last two | AUTO-vs-human precedence; the four-branch outcome cascade; denominator arithmetic; that the exported rate is reconstructible |
| `test-decision_mutation.R` | 3 (drives 10+ planted mutants) | mutation | **CI gate 2** | No | `R/utils_decisions.R` | That the BVA battery still *detects* each defect it was written for. A surviving mutant fails the build. |
| `test-cycle01_thresholds_contracts.R` | 10 | unit + contract | Gate 3 | No | `utils_scoring.R`, `utils_congresses.R`, `utils_decisions.R`, `config.yml` | Threshold inclusivity at the cutoff; `Inf`/`NA` handling; cohort sizes 0 and 1; pre-conference short-circuit; date-vector length; order-invariance of dedup; every congress year has a config date |
| `test-pipeline_semantics.R` | 24 | semantic / integration | Gate 3 | Yes — every block guards on file existence | `abstracts_cleaned.csv`, `abstracts_with_matches.csv`, model RDS, `validation_metrics.csv`, `sensitivity_analyses.csv` | Row counts and years; no videos downstream; unique IDs; ID naming; classification vocabulary; score-to-class consistency; plausibility bounds against Cochrane MR000005; coverage floors; Cox validity; PH assumption; gold-standard sensitivity and NPV; cohort ID equality; pre-conference retention; sensitivity monotonicity |
| `test-shiny_app.R` | 27 | integration | Gate 3 | Yes — gitignored artefacts guarded by `skip_if_no_file()` | `shiny/adjudication_app/app.R`, the deploy bundle, `pubmed_candidates.csv` | App parses; required files exist; column contracts; abstract-text coverage by era; reactive behaviour; bundle freshness |
| `test-shiny_e2e.R` | 17 | end-to-end (browser) | Gate 3 | **Always skips** without `shinytest2` | `shinytest2`, Chrome | Full reviewer workflow |
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
| Human-adjudication completeness | **No** | — | **GAP.** Nothing asserts that every cohort abstract has a decision, nor that decisions with no matching abstract are accounted for (47 video orphans). |
| Final dataset grain | Partial | `test-pipeline_semantics.R:18` | Moderate — row count and year set, on `abstracts_with_matches.csv` |
| Model cohort reconciliation | **No** | — | **GAP.** Nothing asserts `nrow(model_data)` against `n_evaluated`, or that the Cox event count equals the number published. The 104-vs-178 gap would have surfaced immediately. |
| Manuscript numbers match generated output | Partial → **now yes** | `test-docs_drift.R` | Moderate — see §4 |
| Every congress year has a config date | Yes | `test-cycle01_thresholds_contracts.R:175` | Strong |
| Classifier boundary behaviour | Yes | `test-cycle01_thresholds_contracts.R:24, 41, 90` | Strong |
| Dedup is order-invariant | Yes | `test-cycle01_thresholds_contracts.R:151` | Strong |
| **Candidate pool matches what was scored** | **No** | — | **GAP — and it is currently violated.** `sum(match_scores$n_candidates)` (64,718) ≠ `nrow(pubmed_candidates.csv)` (48,984), and 283 winning PMIDs are unresolvable. F2. |
| **Cohort completeness against the source supplement** | **No** | — | **GAP — and it is currently violated.** Nothing compares the captured DOI set against Crossref. F1. |
| **Predictor derivation postdates the text backfill** | **No** | — | **GAP — and it is currently violated.** Nothing detects that `has_numeric_results` is 0.0% for seven consecutive congress years. F3. |
| **Subgroup variables are not outcome-conditional** | **No** | — | **GAP — and it is currently violated.** F4. |
| **The demographics merge ran** | **No** | — | **GAP.** `06` silently drops model terms that are absent. F8. |
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
