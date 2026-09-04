# Results Provenance

Every number that appears in the README, the manuscript drafts, the technical
appendix, the generated tables or the figures, mapped to the file and the code
that produced it, with a verification status.

Verification was performed on 2026-09-03. The tables below reflect the state
**after** the remediation pass described in
[FAILURE_MODES.md](FAILURE_MODES.md); values that moved during that pass are
shown with their previous value so the change is auditable. **Status key**: ✅ current and correct · ⚠️ correct but easy to
misread · ❌ stale or wrong.

---

## 1. Headline results

| Claim | Current value | Source dataset | Code | Verification |
|---|---:|---|---|---|
| Total presentations parsed | 1,154 | `data/processed/abstracts_parsed_web.csv` | `R/01b_parse_web.R` | ✅ `nrow()` |
| Video presentations excluded | 48 | same | `R/02_clean_abstracts.R:31-36` | ✅ |
| **Analytical cohort** | **1,106** | `output/final_analytical_dataset.csv` | `R/02_clean_abstracts.R` | ✅ `nrow()` |
| Unresolved adjudication | 55 | same | `R/utils_decisions.R:79-85` | ✅ `sum(is.na(final_published))` |
| **Publication-rate denominator** | **1,051** | same | `R/06_analyze_results.R:56` | ✅ 1,106 − 55 |
| **Publications** | **178** | same | `R/utils_decisions.R:78-85` | ✅ `sum(final_published, na.rm=TRUE)` |
| Not published | 873 | same | | ✅ 1,051 − 178 |
| **Publication rate** | **16.9%** (16.94%) | `output/aim1_publication_rate.csv` | `R/06_analyze_results.R:62-63` | ✅ 178/1,051 |
| 95% CI | 14.8% – 19.3% | same | `prop.test(178, 1051, correct = FALSE)` | ✅ reproduced |
| Median months to publication | **13.7** (was 13.8) | `output/aim2_time_to_pub.csv` | `R/06_analyze_results.R` | ✅ computed on **171 of 178** published; the other 7 published before their congress and are excluded |
| p25 | **5.7** (was 6.3) | same | | ✅ |
| p75 | **22.6** (was 25.0) | same | | ✅ |
| RCT → time to publication | HR **2.212** (1.473–3.323), p < 0.001 | `output/aim2b_cox_regression.csv` | `coxph` | ✅ 170 events, was 104 |
| RCT → publication | OR **2.556** (1.551–4.156), p < 0.001 | `output/aim3_logistic_regression.csv` | `glm` | ✅ n = 1,010 |
| Multicenter → time to publication | HR **1.387** (0.809–2.377), p = 0.234 | `output/aim2b_cox_regression.csv` | | ✅ not significant; the README claim of HR ≈ 2.3 has been removed |
| Multicenter → publication | OR **1.482** (0.769–2.718), p = 0.218 | `output/aim3_logistic_regression.csv` | | ✅ the programmatic abstract now derives its significance wording from the p-value |
| Author count → time to publication | HR **1.257** (1.093–1.445), p = 0.001 | `output/aim2b_cox_regression.csv` | | ⚠️ `n_authors` is censored at 5 |
| Author count → publication | OR **1.336** (1.154–1.560), p < 0.001 | `output/aim3_logistic_regression.csv` | | ⚠️ same |
| Male first author → time to publication | HR **0.811** (0.596–1.103), p = 0.181 | `output/aim2b_cox_regression.csv` | | ⚠️ not significant. Moved again on 2026-09-04 when NPPES registry gender became tier 1; 231 abstracts carry a cross-source conflict |
| US-based → time to publication | HR **1.419** (0.887–2.270), p = 0.145 | same | | ⚠️ **no longer significant** after `is_us_based` was re-derived from the backfilled text (689 → 907 TRUE) |
| Academic → time to publication | HR **0.621** (0.440–0.876), p = **0.007** | `output/aim2b_cox_regression.csv` | | ⚠️ **newly significant and negative**; `is_academic` went from 148 to 371 TRUE when re-derived. Provisional. |
| PH assumption, global | p = **0.056** (was 0.32) | `output/cox_ph_assumption.csv` | `cox.zph()` | ⚠️ only marginally supported now that the test has 170 events |
| Gold-standard sensitivity | 1.00 | `output/validation_metrics.csv` | `R/validation_gold_standard.R` | ⚠️ n = 50, of which **49 classified** (`n_classified`); PPV is **0.50** and accuracy 0.735 |
| Interrater agreement | 98.1% raw, κ = 0.994, over 519 abstracts | `output/interrater_agreement.csv` | `R/10_interrater.R` | ⚠️ reviewers were not blinded to the algorithm's answer |
| Logistic-model N | 1,010 | `output/aim3_logistic_regression.csv` (`n_obs`) | `R/06_analyze_results.R:285` | ✅ 41 abstracts leave the model through complete-case deletion |
| Cox-model N / events | 938 / 104 | `data/processed/cox_model.rds` | `R/06_analyze_results.R:217` | ⚠️ 104 events, not 178 |

---

## 2. Numbers in `README.md`

| Line | Claim | Status | Current value |
|---|---|---|---|
| 5 | badge "Tests: 392 passing" | ❌ | 519 passing, **1 failing**, 1 skipped |
| 18–19 | headline | ✅ fixed — now states 178 of 1,051 evaluated, cohort 1,106 |
| 19 | "A further 55 remain pending" | ✅ | 55 |
| 21–22 | "Cochrane … pooled rate near 45%" | — | external claim, not verifiable here |
| 24–27 | "Supersedes 17.2% … 39 abstracts — 35 unpublished, plus 4 with a confirmed match" | ✅ | 39 = 35 + 4 |
| 33 | figure 1 alt text | ✅ fixed — rewritten to the current flow |
| 60–62 | Cox narrative | ✅ fixed — the README now reports the fitted values and names which terms are not distinguishable from no effect |
| 58 | figure 5 alt text | ✅ fixed |
| 65–66 | gender conflicts | ✅ fixed — 228 |
| 109, 250, 258 | test counts | ✅ fixed — the README now states the current counts and links to VALIDATION.md |
| 219 | variable count | ✅ fixed — 91 in the final dataset |
| 225 | gender coverage | ✅ fixed — 96.4% (1,066 of 1,106) |
| 225 | "practice_type (18%)" | ✅ | 17.5% |
| 225 | "subspecialty_unified (36%)" | ✅ | 35.0% |
| 225 | "state_unified (31%)" | ✅ | 30.4% |
| 226 | NPI high-confidence matches | ✅ fixed — 276 |
| 295 | dataset dimensions | ✅ fixed — **1,106 × 91** |
| 174–183 | "Six PubMed strategies … plus four supplementary databases and a DOI search" | ✅ | verified against `build_search_strategies()` |
| 187 | "10-component composite score" | ⚠️ **unfixed** | ten components exist; `keyword_pts` is 0 for every abstract, so nine are live. Fixing it would change every score and invalidate the adjudication. |
| 189–193 | classification thresholds | ✅ | match `config.yml` and `classify_match()` |
| 317–318 | DuckDB and ABOG file paths | ✅ fixed — both moved to `config.yml: external_data`, overridable by env var. The earlier claim that neither file exists was **wrong**: both exist; the ABOG symlink points at a schema-drifted export and the DuckDB volume name has a suffix. See REPRODUCIBILITY.md. |

---

## 3. Numbers in `CHANGELOG.md` and `NEWS.md`

| Claim | Status | Current value |
|---|---|---|
| "`output/final_analytical_dataset.csv` — unified **1,067 × 90**" | ❌ | 1,106 × 90 |
| "`gender_conflicts.csv` — **277** cross-source gender disagreements" | ❌ | 228 |
| "`gender_from_openalex.csv` — 157 resolutions" | ✅ | 157 rows |
| "`gender_from_open_payments.csv` — 16 resolutions" | ✅ | 16 rows |
| "Publication rate 17.2% → 16.9%; cohort 1,067 → 1,106; published 174 → 178" | ✅ | end state verified |
| "`10e_merge_demographics.R` is the **sole writer** to `abstracts_with_matches.csv`" | ✅ corrected in CHANGELOG | **six** writers: `01d`, `05`, `09b`, `09d`, `09e`, `10e`, plus an inline block in `00_run_all.R`. `05` no longer destroys the others' columns (F10). |
| "`10g_second_author_triangulation.R` returns zero rows" | ✅ | 0 rows |
| "Three `test-pipeline_semantics.R` failures … remain" | ❌ | `test-pipeline_semantics.R` now passes in full; the single remaining failure is `test-shiny_app.R:458` (stale deploy bundle) |
| "1,106 vs 1,067 row mismatch between `abstracts_cleaned.csv` and `abstracts_with_matches.csv`" | ❌ | both are 1,106; the mismatch was resolved by the denominator fix |
| "NPI … 60,846 board-certified OB/GYNs" | — | unverifiable — the ABOG file is absent |

---

## 4. Numbers in the manuscript drafts

`docs/abstract_results_section.Rmd` and `docs/aagl_abstract_programmatic.Rmd`
are **fully programmatic** — every figure is an inline `r` expression reading
the pipeline CSVs. There are therefore no stale *numbers* in them. There are
stale *sentences*:

| Location | Problem | Status |
|---|---|---|
| `aagl_abstract_programmatic.Rmd`, Results | "Multicenter studies were **also significantly more likely** to reach full publication (OR `r multi_or` …)" — the inline values it prints are OR 1.88, CI 0.86–3.88, p = 0.096 | ❌ the prose asserts significance that the numbers it prints contradict |
| `aagl_abstract_programmatic.Rmd`, Interventions | "All probable algorithmic matches were confirmed via **blinded** human adjudication" | ❌ review is not blinded: the app displays the classification tier and every score component and pre-selects the algorithm's answer (`shiny/adjudication_app/app.R:1058-1065`) |
| `aagl_abstract_programmatic.Rmd`, Results | "RCTs … more likely to be published than **observational studies**" | ❌ the reference category is `is_rct == FALSE`, i.e. every non-RCT design |
| `aagl_abstract_programmatic.Rmd`, Interventions | "6-strategy search across … (PubMed, CrossRef, Europe PMC, OpenAlex)" | ⚠️ omits Semantic Scholar and the DOI chain; "6-strategy" describes PubMed alone |
| `abstract_results_section.Rmd:46-47` | `n_community <- prac$n[prac$practice_type == "community"]` | ❌ **renders empty.** `classify_practice_type()` no longer emits a `community` level; `aim1_by_practice_type.csv` has `academic`, `military_va`, `private_practice`, `research_institute`. The inline `r n_community` on line 184 produces `numeric(0)`. |
| `abstract_results_section.Rmd:130` | describes practice type as "academic, community, military/VA, research institute, or private practice" | ❌ five levels described, four produced |
| `technical_appendix.Rmd` A8.3 | static table giving 97 abstracts per year and 2017 coverage 0% | ❌ current values are 93–98 per year and 2017 = 1/97 = 1.0%. The **adjacent code chunk computes the correct table**, so the rendered document contradicts itself. |
| `technical_appendix.Rmd` A8.1 | "667 of 1,106 abstracts (60%) had no recoverable abstract text" | ⚠️ 667 is the count *before* recovery; after recovery 184 have no `abstract_text`. The sentence is about the initial state and is correct as written but reads as a final figure. |

---

## 5. Numbers in tables and figures

| Artefact | Generated | Consistent with the current analysis? |
|---|---|---|
| `output/figures/figure1_flow_data.csv` | 2026-09-01 22:16 | ✅ 1,154 / 48 / 1,106 / 131 / 81 / 142 / 39 / 713 |
| `figure1_flow_diagram.png` | 2026-09-01 22:16 | ✅ |
| `figure1_strobe_flowchart.{png,pdf}` | 2026-09-03 14:56 | ✅ asserts its own arithmetic with `stopifnot()` |
| `figure2`–`figure6`, `figureS1`–`figureS4` | 2026-09-01 22:16 | ⚠️ one analysis run behind. `06_analyze_results.R` was re-run 2026-09-03 15:27; the model outputs are **byte-identical** to the 2026-09-01 versions (verified against commit `e288259`), so the figures are numerically current, but a future re-run will silently desynchronise them. |
| `output/tables/table1`–`table4` | 2026-09-01 22:16 | ⚠️ same |
| `output/aim1_by_practice_type.csv` | 2026-09-03 | ⚠️ now carries `availability_among_published` (81.5%) vs `availability_among_unpublished` (2.4%) and an `outcome_conditional_stratifier` flag. Still not a publication rate — see §6. |
| `output/aim1_by_subspecialty.csv` | 2026-09-03 | ⚠️ same (77.5% vs 1.9%) |
| `output/aim5_publication_bias.csv` | 2026-09-03 | ✅ regenerated; `result_positivity` restored to `05`'s select |
| `output/search_strategy_efficacy.csv` | 2026-04-19 | ❌ **still stale** — a pre-correction measurement. Regenerating it requires re-running the search layer. |
| `output/aim4_strategy_performance.csv` / `table4` | 2026-09-03 | ⚠️ the pool is repaired, but rows recovered by the rebuild carry `strategies = "unrecovered"`, so attribution covers only the pairs whose provenance survived |
| `output/figures/strobe_flow.{png,pdf,svg}` | 2026-09-01 21:30 | ❌ superseded by `figure1_strobe_flowchart` |

---

## 6. Two subgroup tables that must not be read as publication rates

`output/aim1_by_practice_type.csv` reports 87.3% for academic centres, 100% for
military/VA and 100% for research institutes.
`output/aim1_by_subspecialty.csv` reports 90.4% for general OB/GYN and 100% for
FPMRS, REI, obstetrics and surgery_other. These are **not** publication rates.

`practice_type` and `subspecialty` are parsed from the **matched publication's**
PubMed affiliation string. They can only exist for an abstract that has a
matched publication:

| | published | not published |
|---|---:|---:|
| `practice_type` present | 145 | 21 |
| `practice_type` absent | 33 | 852 |

Conditioning the rate on a variable that is itself a near-deterministic function
of the outcome produces ~90% in every stratum by construction. The same holds
for `subspecialty` (138 published vs 17 unpublished have a value).

`figure4_subgroup_rates.png` plots these panels alongside genuinely
outcome-independent strata (study design, gender), which invites the reader to
compare them.

`subspecialty_unified` and `state_unified` are partly rescued by their NPI
component, which does not depend on a match (206 unpublished abstracts have a
`subspecialty_unified`), but they are still strongly outcome-associated and
carry the mixed-vocabulary problem described in
[AUTHOR_ENRICHMENT.md](AUTHOR_ENRICHMENT.md) §5.

**The tables now flag themselves.** `subgroup_rate()` in
`R/06_analyze_results.R` attaches the availability split and an
`outcome_conditional_stratifier` column to every row, and warns at run time. The
rate is still emitted because it is the correct conditional quantity and the
manuscript reads these files. `figure4_subgroup_rates.png` still plots these
panels beside outcome-independent strata; whether to drop them is a
presentation decision.

---

## 7. Which conflicting file wins

Where two files disagree, the authoritative one is named in
[SOURCE_OF_TRUTH.md](SOURCE_OF_TRUTH.md). In every case above the generated
output is right and the prose is stale, with two exceptions: the two subgroup
CSVs in §6, where the generated output is arithmetically correct and
scientifically misleading, and `aim5_publication_bias.csv` /
`search_strategy_efficacy.csv`, where the generated output is simply old.
