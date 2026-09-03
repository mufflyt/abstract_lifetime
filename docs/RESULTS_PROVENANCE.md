# Results Provenance

Every number that appears in the README, the manuscript drafts, the technical
appendix, the generated tables or the figures, mapped to the file and the code
that produced it, with a verification status.

Verification was performed on 2026-09-03 against commit `665c551` with a clean
working tree. **Status key**: ✅ current and correct · ⚠️ correct but easy to
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
| Median months to publication | 13.8 | `output/aim2_time_to_pub.csv` | `R/06_analyze_results.R:155-162` | ⚠️ computed on **104 of 178** published |
| p25 | 6.3 | same | | ⚠️ same caveat |
| p75 | 25.0 | same | | ⚠️ same caveat |
| RCT → time to publication | HR 2.295 (1.294–4.071), p = 0.005 | `output/aim2b_cox_regression.csv` | `coxph`, `R/06_analyze_results.R:217` | ✅ |
| RCT → publication | OR 2.244 (1.273–3.856), p = 0.004 | `output/aim3_logistic_regression.csv` | `glm`, `:285` | ✅ |
| Multicenter → time to publication | HR **1.132** (0.413–3.105), p = **0.809** | `output/aim2b_cox_regression.csv` | | ❌ README says "HR ≈ 2.3 … p < 0.05" |
| Multicenter → publication | OR 1.884 (0.861–3.881), p = **0.096** | `output/aim3_logistic_regression.csv` | | ❌ the programmatic abstract calls this "significantly more likely" |
| Author count → time to publication | HR 1.240 (1.042–1.475), p = 0.015 | `output/aim2b_cox_regression.csv` | | ⚠️ `n_authors` is censored at 5 |
| Author count → publication | OR 1.325 (1.147–1.543), p < 0.001 | `output/aim3_logistic_regression.csv` | | ⚠️ same |
| Male first author → time to publication | HR 0.590 (0.390–0.892), p = 0.012 | `output/aim2b_cox_regression.csv` | | ⚠️ gender inferred; 228 conflicts |
| US-based → time to publication | HR 1.712 (1.091–2.687), p = 0.019 | same | | ⚠️ `is_us_based` has a severe year gradient |
| PH assumption, global | p = 0.32 | `output/cox_ph_assumption.csv` | `cox.zph()` | ✅ |
| Gold-standard sensitivity | 1.00 | `output/validation_metrics.csv` | `R/validation_gold_standard.R` | ⚠️ n = 50, of which **49 classified** (`n_classified`); PPV is **0.50** and accuracy 0.735 |
| Interrater agreement | 98.1% raw, κ = 0.994, over 519 abstracts | `output/interrater_agreement.csv` | `R/10_interrater.R` | ⚠️ reviewers were not blinded to the algorithm's answer |
| Logistic-model N | 1,010 | `output/aim3_logistic_regression.csv` (`n_obs`) | `R/06_analyze_results.R:285` | ✅ 41 abstracts leave the model through complete-case deletion |
| Cox-model N / events | 938 / 104 | `data/processed/cox_model.rds` | `R/06_analyze_results.R:217` | ⚠️ 104 events, not 178 |

---

## 2. Numbers in `README.md`

| Line | Claim | Status | Current value |
|---|---|---|---|
| 5 | badge "Tests: 392 passing" | ❌ | 519 passing, **1 failing**, 1 skipped |
| 18–19 | "**16.9%** … (178 of 1,106; 95% CI 14.8–19.3)" | ❌ internally inconsistent | The rate and CI are for 178/**1,051**. 178/1,106 = 16.1%. |
| 19 | "A further 55 remain pending" | ✅ | 55 |
| 21–22 | "Cochrane … pooled rate near 45%" | — | external claim, not verifiable here |
| 24–27 | "Supersedes 17.2% … 39 abstracts — 35 unpublished, plus 4 with a confirmed match" | ✅ | 39 = 35 + 4 |
| 33 | figure 1 alt text: "1,067 AAGL oral presentations" | ❌ | 1,106. The **figure itself is correct** (`figure1_flow_data.csv` says 1,154 / 48 / 1,106); only the alt text is stale. |
| 60–62 | "Randomized design (HR ≈ 2.2), **multicenter conduct (HR ≈ 2.3)**, and author count are each associated with faster publication" | ❌ | Multicenter HR is 1.13, p = 0.81. Only RCT, author count, US-based and gender reach p < 0.05. |
| 58 | figure 5 alt text: "RCT design, multicenter studies, and number of authors show hazard ratios above 1 at p < 0.05" | ❌ | same |
| 65–66 | "277 authors carry a cross-source disagreement in `gender_conflict`" | ❌ | **228** (`data/processed/gender_conflicts.csv`, 228 rows; `sum(gender_conflict)` = 228) |
| 109, 250, 258 | "391 tests" / "[ FAIL 0 … PASS 392 ]" | ❌ | 519 passing, 1 failing |
| 219 | "59 columns per abstract" | ❌ | 90 in the final dataset, 86 in `abstracts_with_matches.csv` |
| 225 | "gender_unified (99% coverage)" | ❌ | 96.3% |
| 225 | "practice_type (18%)" | ✅ | 17.5% |
| 225 | "subspecialty_unified (36%)" | ✅ | 35.0% |
| 225 | "state_unified (31%)" | ✅ | 30.4% |
| 226 | "278 high-confidence [NPI] matches, 40% of US authors" | ❌ | **276** high-confidence |
| 295 | "`final_analytical_dataset.csv` Unified **1,067 × 90**" | ❌ | **1,106 × 90** |
| 174–183 | "Six PubMed strategies … plus four supplementary databases and a DOI search" | ✅ | verified against `build_search_strategies()` |
| 187 | "10-component composite score" | ⚠️ | ten components exist; `keyword_pts` is 0 for every abstract, so nine are live |
| 189–193 | classification thresholds | ✅ | match `config.yml` and `classify_match()` |
| 317–318 | DuckDB and ABOG file paths | ❌ | neither path exists on this machine |

---

## 3. Numbers in `CHANGELOG.md` and `NEWS.md`

| Claim | Status | Current value |
|---|---|---|
| "`output/final_analytical_dataset.csv` — unified **1,067 × 90**" | ❌ | 1,106 × 90 |
| "`gender_conflicts.csv` — **277** cross-source gender disagreements" | ❌ | 228 |
| "`gender_from_openalex.csv` — 157 resolutions" | ✅ | 157 rows |
| "`gender_from_open_payments.csv` — 16 resolutions" | ✅ | 16 rows |
| "Publication rate 17.2% → 16.9%; cohort 1,067 → 1,106; published 174 → 178" | ✅ | end state verified |
| "`10e_merge_demographics.R` is the **sole writer** to `abstracts_with_matches.csv`" | ❌ | **six** writers: `01d`, `05`, `09b`, `09d`, `09e`, `10e`, plus an inline block in `00_run_all.R:127-146` |
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
| `output/aim1_by_practice_type.csv` | 2026-09-03 15:27 | ❌ **do not report as publication rates.** See §6. |
| `output/aim1_by_subspecialty.csv` | 2026-09-03 15:27 | ❌ same |
| `output/aim5_publication_bias.csv` | 2026-04-17 | ❌ orphaned; the producing block no longer runs |
| `output/search_strategy_efficacy.csv` | 2026-04-19 | ❌ pre-correction measurement |
| `output/aim4_strategy_performance.csv` / `table4` | 2026-09-03 | ❌ attribution understated — joins the stale candidate file |
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

**Nothing here has been changed.** These files are flagged, not corrected.

---

## 7. Which conflicting file wins

Where two files disagree, the authoritative one is named in
[SOURCE_OF_TRUTH.md](SOURCE_OF_TRUTH.md). In every case above the generated
output is right and the prose is stale, with two exceptions: the two subgroup
CSVs in §6, where the generated output is arithmetically correct and
scientifically misleading, and `aim5_publication_bias.csv` /
`search_strategy_efficacy.csv`, where the generated output is simply old.
