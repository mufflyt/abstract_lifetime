# Abstract Lifetime

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.4](https://img.shields.io/badge/R-%3E%3D%204.4-blue.svg)](https://www.r-project.org/)
[![Tests](https://img.shields.io/badge/tests-900%20passing%2C%204%20failing-yellow.svg)](docs/VALIDATION.md)
[![Shiny App](https://img.shields.io/badge/Shiny-Live%20App-orange.svg)](https://mufflyt.shinyapps.io/aagl-adjudication/)

**Publication Rate, Time to Publication, and Predictors of Full Publication
Among Oral Presentations at the AAGL Global Congress, 2012–2023**

A reproducible pipeline that tracks whether abstracts presented orally at the
AAGL Global Congress on Minimally Invasive Gynecology go on to full peer-reviewed
publication. Abstracts are ingested from the *Journal of Minimally Invasive
Gynecology* congress supplements, searched against six bibliographic sources,
scored by a composite matching algorithm, adjudicated by human reviewers, and
analysed against the methodological standards of the
[Cochrane review on full publication of results initially presented in abstracts](https://doi.org/10.1002/14651858.MR000005.pub4)
(Scherer et al., 2018).

---

## Headline result

**16.9%** of AAGL oral presentations reached full peer-reviewed publication —
**178 of 1,051 evaluated** (95% CI 14.8–19.3). The cohort is **1,106** oral
presentations; a further **55** remain unresolved in adjudication and are
excluded from the denominator. For context, the Cochrane review of this
literature reports a pooled rate near 45% across specialties.

Median time from congress to publication was **13.7 months** (IQR 5.7–22.6),
computed on the 171 published abstracts whose publication follows their
congress. Seven confirmed publications appeared shortly *before* their meeting
and are excluded from that summary but counted in the numerator.

> Cohort 1,106 and denominator 1,051 are different quantities. See
> [docs/COHORT_ASSEMBLY.md](docs/COHORT_ASSEMBLY.md).

### Cohort assembly

![STROBE flow diagram: 1,154 presentations parsed from the AAGL congress supplements, 48 video presentations excluded, 1,106 oral presentations forming the cohort, 55 excluded for unresolved adjudication, 1,051 evaluated, of which 178 published and 873 not published.](output/figures/figure1_flow_diagram.png)

### Cumulative publication over time

![Kaplan-Meier curve of cumulative publication against months since conference, rising steeply through the first 24 months and flattening thereafter.](output/figures/figure2_km_curve.png)

Publication is overwhelmingly an early event. Abstracts that have not appeared
within roughly three years of the congress rarely publish at all.

### Publication trajectory by congress year

![Kaplan-Meier curves stratified by congress year, 2012 through 2023.](output/figures/figure3_km_by_year.png)

Recent congresses carry less follow-up and are right-censored. Note that the
crude rate *rises* with congress recency (5.6% in 2017 to 33.3% in 2023), the
opposite of what censoring alone predicts — see
[Known limitations](#known-limitations).

### Predictors of time to publication

![Cox proportional hazards forest plot showing hazard ratios with 95% confidence intervals for randomized design, academic affiliation, US location, author count, inferred male first author, multicenter conduct and reported funding.](output/figures/figure5_cox_forest.png)

Randomized design (HR 2.21, 95% CI 1.47–3.32) and author count (HR 1.26 per
author, 1.09–1.45) are associated with faster publication. Academic affiliation
is associated with *slower* publication (HR 0.62, 0.44–0.88). US location
(HR 1.42, 0.89–2.27), male first authorship (HR 0.81, 0.60–1.10), multicenter
conduct (HR 1.39, 0.81–2.38) and reported funding (HR 1.76, 0.43–7.17) are not
statistically distinguishable from no effect.

Three cautions. The proportional-hazards assumption is now **violated**
(global p = 0.043 on 171 events). Only two terms are robust to resampling:
across 500 bootstrap refits, `n_authors` is retained 97.2% of the time and
`is_rct` 93.6%, while `is_academic` survives only 67.4% — it is significant in
all twelve leave-one-congress-out refits, so it is not driven by any single
congress, but it is not sampling-robust either. And `has_funding` is now
excluded from both models by a near-zero-variance rule (TRUE for 7 of 1,051).
See `output/model_predictor_stability.csv` and
`output/model_variable_screen.csv`.
Author gender is **registry-reported for 267 of 1,066 abstracts and inferred
from a name for the rest**, 287 of those from a single first initial; 231 carry
a cross-source disagreement recorded in `gender_conflict`.

### Other figures

![Publication rate by subgroup, with confidence intervals.](output/figures/figure4_subgroup_rates.png)

![Histogram of months from conference to publication.](output/figures/figure6_time_to_pub.png)

Four supplementary figures are generated into `output/figures/` but not tracked.

---

## Pipeline

```
ScienceDirect JMIG supplements (12 congress issues)
        │  01b_parse_web.R + 01d_tag_session_type.R
        ▼
   1,154 presentations
        │  02_clean_abstracts.R  (−48 video)
        ▼
   1,106 oral presentations  ── the COHORT
        │  03 / 03b / 03c   PubMed · CrossRef · Europe PMC ·
        │                   OpenAlex · Semantic Scholar · DOI chain
        ▼
   64,718 scored (abstract × candidate) pairs
        │  04_score_matches.R   10-component composite score
        ▼
   definite 131 · probable 81 · possible 142 · no_match 709 ·
   excluded 39 · no_candidates 4
        │  05_adjudicate.R → Shiny app → Google Sheets
        ▼
   1,153 adjudicated decisions (3 human reviewers + an AUTO pass)
        │  06_analyze_results.R   utils_decisions.R
        ▼
   1,051 evaluated → 178 published · 873 not · 55 unresolved
        │  07_make_tables.R · 08_make_figures.R · strobe_flowchart.R
        ▼
   final_analytical_dataset.csv (1,106 × 92), tables, figures
```

Full stage-by-stage detail, including a Mermaid DAG:
[docs/PIPELINE.md](docs/PIPELINE.md) ·
machine-readable: [docs/pipeline_manifest.yml](docs/pipeline_manifest.yml)

---

## Quick start

```bash
git clone https://github.com/mufflyt/abstract_lifetime.git
cd abstract_lifetime
```

```r
install.packages(c(
  "tidyverse", "here", "config", "cli", "rentrez", "xml2", "httr",
  "jsonlite", "survival", "broom", "gender", "stringdist", "digest",
  "rvest", "purrr", "yaml", "flowchart", "DiagrammeR", "htmlwidgets",
  "webshot2", "scales", "npi", "naniar"
))

# Optional but recommended: supplies NPPES registry gender (waterfall tier 1),
# Table 1 with p-values, join safety in the demographics merge, Little's MCAR
# test, and the session snapshot. Every use degrades gracefully without it.
remotes::install_github("mufflyt/mysterycall@42d66d92ef52a0f85d1f7c61208c2ddd79d9c06e")
```

**Reproduce the reported numbers from tracked files** (works from a clean clone,
takes about a minute):

```r
source("R/06_analyze_results.R")   # outcome, all five aims, models
source("R/07_make_tables.R")
source("R/08_make_figures.R")
source("R/strobe_flowchart.R")
testthat::test_dir("tests/testthat")
```

**Re-run the whole pipeline** — see the caveats below first:

```r
Rscript 00_run_all.R   # 3-4 h cold, ~30 min warm; now includes the
                       # demographics merge and the STROBE flow chart
```

If `data/processed/pubmed_candidates.csv` is missing or predates
`match_scores.csv`, repair it before running steps 4-6:

```r
Rscript scripts/rebuild_candidate_pool.R
```

---

## Documentation

| Document | Answers |
|---|---|
| [COHORT_ASSEMBLY.md](docs/COHORT_ASSEMBLY.md) | What exactly is the denominator, and what happened to every parsed abstract? |
| [PIPELINE.md](docs/PIPELINE.md) | Every stage: input → transformation → output, with a DAG |
| [PUBLICATION_SEARCH.md](docs/PUBLICATION_SEARCH.md) | Every search query, verbatim, per source |
| [MATCHING_ALGORITHM.md](docs/MATCHING_ALGORITHM.md) | The composite score, component by component, with thresholds |
| [OUTCOME_DEFINITION.md](docs/OUTCOME_DEFINITION.md) | When does an abstract count as published? (Methods-ready) |
| [ADJUDICATION.md](docs/ADJUDICATION.md) | Human review: schema, precedence, and the decision accounting |
| [DATA_DICTIONARY.md](docs/DATA_DICTIONARY.md) · [CSV](docs/data_dictionary.csv) | All 92 variables with derivation and coverage |
| [DATA_INVENTORY.md](docs/DATA_INVENTORY.md) · [CSV](docs/data_inventory.csv) | All 74 data files with producer, consumers and grain |
| [AUTHOR_ENRICHMENT.md](docs/AUTHOR_ENRICHMENT.md) | Identity resolution and the ten-tier gender waterfall |
| [STATISTICAL_ANALYSIS.md](docs/STATISTICAL_ANALYSIS.md) | Every model as fitted, with diagnostics |
| [RESULTS_PROVENANCE.md](docs/RESULTS_PROVENANCE.md) | Every reported number → the file and code that produced it |
| [SOURCE_OF_TRUTH.md](docs/SOURCE_OF_TRUTH.md) | Which file wins when two disagree |
| [REPRODUCIBILITY.md](docs/REPRODUCIBILITY.md) | What a clean clone gets, and what it does not |
| [FAILURE_MODES.md](docs/FAILURE_MODES.md) | Seventeen ways this pipeline can be plausibly wrong, and which nine are now fixed |
| [VALIDATION.md](docs/VALIDATION.md) | The test suite and the invariants that have no test |
| [METHODOLOGICAL_HISTORY.md](docs/METHODOLOGICAL_HISTORY.md) | Corrections, with their effect on the numbers |
| [technical_appendix.Rmd](docs/technical_appendix.Rmd) | Extended appendices A1–A13 |

---

## What requires manual or external data

| Component | Status |
|---|---|
| Human adjudication (1,263 decisions, 3 reviewers) | **Manual.** Tracked in `output/manual_review_decisions.csv`; the live store is a Google Sheet requiring a service-account key. |
| Gold standard (50 abstracts) | **Manual.** Tracked. Cannot be regenerated by code. |
| International gender lookup (300 names) | **Manual.** Tracked. |
| ACGME teaching-hospital list (2,754 names) | External snapshot, tracked, no retrieval script. |
| `data/cache/sd_html/` (1,154 files) | **Gitignored and unrefetchable** — ScienceDirect returns HTTP 403. This cache is currently the only copy of the source documents. |
| Shiny adjudication app | The bundle is verified current and `deploy.R` refuses to publish a stale one, but **the live app at shinyapps.io still serves April data** until someone runs `SHINY_DEPLOY=true Rscript shiny/adjudication_app/deploy.R`. |
| `data/processed/pubmed_candidates.csv` (≈130 MB) | **Gitignored.** A clean clone cannot run steps 4–5 without re-running the search, or `scripts/rebuild_candidate_pool.R` against `match_scores_detailed.rds`. |
| NPI matching | **Not currently regenerable.** Both inputs now come from `config.yml: external_data` (`ABOG_NPI_PATH`, `NPPES_DUCKDB_PATH`). Both files exist on this machine, but the ABOG `LATEST` symlink now targets a workforce export with no gender column and NPIs on 411 of 79,400 rows, so `R/10_npi_matching.R` refuses to overwrite the richer shipped sidecar. |
| 2017 abstract text | **Irrecoverable** without institutional PDF access (CORS blocks the jmig.org scraper; no Wayback snapshots). 96 of 97 abstracts have no text. |

---

## Test status

27 test files. As of 2026-09-04: **900 passing, 4 failing, 1 skipped**
(from 519 passing / 1 failing at the start of the day). Every failure is
deliberately left red, each marking a decision that belongs to the author rather
than to code:

| Failing test | What it reports | Why it stays red |
|---|---|---|
| `test-pipeline_semantics.R:247` | Cox proportional hazards is violated (global p = 0.043) | The fix is a modelling decision — stratify, use time-varying coefficients, or report the HRs as averages over follow-up |
| `test-cycle04_validation_sensitivity.R:179` | `search_strategy_efficacy.csv` still carries the pre-correction `title`-strategy yield of 0.2% | Regenerating it means re-running the whole search layer, which would change candidate sets and invalidate the human adjudication |
| `test-cycle06_scoring_composite.R:83` | `keyword_pts` fires on 0 of 1,106 abstracts, so the "10-component" score has nine live components | Fixing the component changes every score and therefore every classification |
| `test-cycle06_scoring_composite.R:116` | 3 PMIDs are credited to 6 published abstracts | Deciding which abstract owns each PMID is adjudication; surfaced in `final_pmid_shared` |

CI runs three gates: decision-logic boundary contracts, then mutation tests
(every planted defect must still be killed), then the full suite.
Full inventory and the list of invariants that have **no** test:
[docs/VALIDATION.md](docs/VALIDATION.md).

## Known limitations

1. **The cohort is truncated.** The ScienceDirect listing scraper captures only
   the first ~100 items per congress supplement, against 392–852 items deposited
   in Crossref. For congress years 2012–2021 the captured window ends while
   still inside the oral-presentation block, so an unknown number of oral
   presentations were never ingested. The cohort is best described as *the first
   ~95–100 presentations listed in each supplement*, not *all oral
   presentations*. [FAILURE_MODES.md F1](docs/FAILURE_MODES.md)
2. **2017 and 2018 have no recoverable abstract text at all** (1 of 97 and 0 of
   95). Every text-derived covariate is near zero for those two congresses, and
   congress year is in neither model, so they act as a measurement-driven
   stratum. The wider 2012–2018 version of this problem was a derivation-ordering
   bug and is fixed. [F3](docs/FAILURE_MODES.md)
3. **A failed API call is indistinguishable from a genuine zero result.** No
   retry, no error column, and the checkpoint marks the abstract complete, so a
   transient NCBI outage leaves a permanently under-searched abstract. [F5](docs/FAILURE_MODES.md)
4. **Search checkpoints resume but never invalidate.** Editing a query does not
   re-run it for an already-completed abstract. [F6](docs/FAILURE_MODES.md)
5. **`aim1_by_practice_type.csv` and `aim1_by_subspecialty.csv` are rates
   conditional on a match having been found**, not publication rates — the
   stratifiers come from the matched publication's affiliation. The files now
   carry the availability split that proves it. [F4](docs/FAILURE_MODES.md)
6. Author gender is registry-reported for 267 of 1,066 abstracts (NPPES, then
   ABOG) and name-inferred for the rest; 287 rest on a single first initial and
   231 carry a cross-source disagreement.
7. `n_authors` is censored at 5 by ScienceDirect's author-list truncation, and
   is a significant term in both models.
8. Publication dates are print/issue dates; `ArticleDate` is not read. Eleven
   pre-congress exclusions rest on year-only dates resolved to 1 January.
   Changing this would re-score every candidate and invalidate the human
   adjudication, so it has not been changed. [F14](docs/FAILURE_MODES.md)
9. The composite score is described as ten-component but `keyword_pts` is 0 for
   every abstract, so nine are live. Fixing it has the same re-scoring
   consequence as above.
10. The 55 unresolved abstracts are removed from the denominator, and they are
    **not** missing completely at random — they differ from the evaluated set on
    `study_design` (p = 0.0004) and `n_authors` (p = 0.013), the latter a
    significant predictor in both models. Bounds: 16.1% if all unpublished,
    21.1% if all published. See `output/unresolved_vs_evaluated.csv`.
11. The proportional-hazards assumption is **violated** (global p = 0.043); the
    constant-hazard-ratio reading of the Cox table needs a stratified or
    time-varying check.
12. **Three publications are each credited to two abstracts**, so six of the 178
    numerator rows rest on three papers. The numerator is not deduplicated —
    two abstracts can legitimately merge into one paper — but the affected rows
    are flagged in `final_pmid_shared` and listed in
    `output/shared_publication_matches.csv`. One of the three is counted
    published against an explicit reviewer `no_match`.
    [F17](docs/FAILURE_MODES.md)

---

## Citation

See [CITATION.cff](CITATION.cff). Change history:
[CHANGELOG.md](CHANGELOG.md) (dated) and [NEWS.md](NEWS.md) (narrative);
methodological history with quantified effects in
[docs/METHODOLOGICAL_HISTORY.md](docs/METHODOLOGICAL_HISTORY.md).

## Contributing

Fork, branch, add tests, ensure `testthat::test_dir("tests/testthat")` does not
regress, open a pull request.

## License

MIT. See [LICENSE](LICENSE).
