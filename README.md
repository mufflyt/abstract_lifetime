# Abstract Lifetime

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.4](https://img.shields.io/badge/R-%3E%3D%204.4-blue.svg)](https://www.r-project.org/)
[![Tests](https://img.shields.io/badge/tests-619%20passing%2C%206%20failing-yellow.svg)](docs/VALIDATION.md)
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

Median time from congress to publication was **13.8 months** (IQR 6.3–25.0),
computed on the 104 published abstracts with a recoverable publication date.

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

Randomized design (HR 2.30, 95% CI 1.29–4.07), author count (HR 1.24 per author,
1.04–1.48), and US location (HR 1.71, 1.09–2.69) are associated with faster
publication. Inferred male first authorship is associated with slower
publication (HR 0.59, 0.39–0.89). Multicenter conduct (HR 1.13, 0.41–3.11) and
reported funding (HR 3.52, 0.48–25.74) are not statistically distinguishable
from no effect; only 38 and 3 abstracts respectively carry those flags.

Author gender is **inferred from names, not self-reported**, and 228 abstracts
carry a cross-source disagreement recorded in `gender_conflict`. Treat that
estimate as provisional.

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
   final_analytical_dataset.csv (1,106 × 90), tables, figures
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
  "webshot2", "scales"
))
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
Rscript 00_run_all.R          # 3-4 h cold, ~30 min warm
Rscript R/run_demographics.R  # REQUIRED: 00_run_all.R does not call it
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
| [DATA_DICTIONARY.md](docs/DATA_DICTIONARY.md) · [CSV](docs/data_dictionary.csv) | All 90 variables with derivation and coverage |
| [DATA_INVENTORY.md](docs/DATA_INVENTORY.md) · [CSV](docs/data_inventory.csv) | All 70 data files with producer, consumers and grain |
| [AUTHOR_ENRICHMENT.md](docs/AUTHOR_ENRICHMENT.md) | Identity resolution and the ten-tier gender waterfall |
| [STATISTICAL_ANALYSIS.md](docs/STATISTICAL_ANALYSIS.md) | Every model as fitted, with diagnostics |
| [RESULTS_PROVENANCE.md](docs/RESULTS_PROVENANCE.md) | Every reported number → the file and code that produced it |
| [SOURCE_OF_TRUTH.md](docs/SOURCE_OF_TRUTH.md) | Which file wins when two disagree |
| [REPRODUCIBILITY.md](docs/REPRODUCIBILITY.md) | What a clean clone gets, and what it does not |
| [FAILURE_MODES.md](docs/FAILURE_MODES.md) | Sixteen ways this pipeline can be plausibly wrong |
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
| `data/processed/pubmed_candidates.csv` (98 MB) | **Gitignored.** A clean clone cannot run steps 4–5 without re-running the search. |
| NPI matching | **Irreproducible.** Both inputs are hard-coded absolute paths outside the repository, and neither exists on the current machine. |
| 2017 abstract text | **Irrecoverable** without institutional PDF access (CORS blocks the jmig.org scraper; no Wayback snapshots). 96 of 97 abstracts have no text. |

---

## Test status

21 test files. As of 2026-09-03 16:50: **627 passing, 3 failing, 1 skipped.**
The suite is under active development in a parallel workstream (the `cycle0*`
files), so these counts move — [docs/VALIDATION.md](docs/VALIDATION.md) carries
the current inventory and, more usefully, the list of scientific invariants that
have **no** test.

Every failure reports a real problem rather than a broken fixture, and two are
deliberately left red pending a decision:

| Failing test | What it reports |
|---|---|
| `test-shiny_app.R:458` | The deployed Shiny bundle is 135 days behind `data/processed/`, so reviewers on the live app see pre-denominator-fix data. |
| `test-cycle03_model_contracts.R:57` | **Left red by design.** `has_funding` is TRUE for 3 of 1,051 abstracts and its odds ratio spans 0.12–29.04. "Not significant" and "not estimable" are different claims; which to report has not been decided. |
| `test-cycle04_validation_sensitivity.R:179` | **Left red by design.** `search_strategy_efficacy.csv` still carries the pre-correction `title`-strategy yield of 0.2% (3 hits in 1,742 queries) and has not been regenerated since the April 2026 title-phrase fix. |

CI runs three gates: decision-logic boundary contracts, then mutation tests
(every planted defect must still be killed), then the full suite.

## Known limitations

1. **The cohort is truncated.** The ScienceDirect listing scraper captures only
   the first ~100 items per congress supplement, against 392–852 items deposited
   in Crossref. For congress years 2012–2021 the captured window ends while
   still inside the oral-presentation block, so an unknown number of oral
   presentations were never ingested. The cohort is best described as *the first
   ~95–100 presentations listed in each supplement*, not *all oral
   presentations*. [FAILURE_MODES.md F1](docs/FAILURE_MODES.md)
2. **74 of the 178 published abstracts have no publication date**, because the
   candidate file on disk is a stale subset of the pool the scores were computed
   against. Time-to-publication, the Kaplan–Meier curve and the Cox model all
   run on 104 events. [F2](docs/FAILURE_MODES.md)
3. **Study characteristics were derived before the abstract text was
   recovered.** For 2012–2018 the regex classifiers saw the title alone, giving
   a step change at 2018/2019 in `is_us_based` (31–45% → 97–100%),
   `is_academic` (0–4% → 22–47%), `sample_size` availability (4–13% → 66–77%)
   and `has_numeric_results` (0% → 47–89%). Congress year is in neither model,
   so every coefficient is confounded by year through measurement. [F3](docs/FAILURE_MODES.md)
4. **`aim1_by_practice_type.csv` and `aim1_by_subspecialty.csv` are not
   publication rates.** Both stratifiers are parsed from the matched
   publication's affiliation and therefore exist almost only for published
   abstracts. [F4](docs/FAILURE_MODES.md)
5. **A failed API call is indistinguishable from a genuine zero result.** No
   retry, no error column, and the checkpoint marks the abstract complete. [F5](docs/FAILURE_MODES.md)
6. **`00_run_all.R` does not run the demographics merge.** Run
   `R/run_demographics.R` separately or the models silently lose their
   demographic terms. [F8](docs/FAILURE_MODES.md)
7. Author gender is inferred; 27% of resolved values come from a single first
   initial and 228 carry a cross-source disagreement.
8. `n_authors` is censored at 5 by ScienceDirect's author-list truncation, and
   is a significant term in both models.
9. Publication dates are print/issue dates; `ArticleDate` is not read. Eleven
   pre-congress exclusions rest on year-only dates resolved to 1 January.
10. The 55 unresolved abstracts are removed from the denominator. Bounds:
    16.1% if all unpublished, 21.1% if all published.

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
