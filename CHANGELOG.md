# Changelog

All notable changes to this project are documented in this file.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
This project does not publish versioned releases; entries are dated by the work
they describe. `NEWS.md` carries the same history with fuller narrative, and
`docs/technical_appendix.Rmd` carries the methodological detail.

## [Unreleased]

### Added

- `pub_issue` extracted from `JournalIssue/Issue` in PubMed XML, required by the
  corrected supplement test.
- `gender_conflict` and `gender_n_sources` columns, populated for every row,
  recording cross-source agreement rather than only the winning value.
- `data/processed/gender_conflicts.csv` — 277 cross-source gender disagreements.
- `data/processed/gender_from_openalex.csv` — 157 resolutions.
- `data/processed/gender_from_open_payments.csv` — 16 resolutions.
- `output/final_analytical_dataset.csv` — unified 1,067 x 90 analytical dataset
  exported by `06_analyze_results.R`.
- `data/AAGL Adjudication Decisions - decisions.csv` — 2,372 human decisions.
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

### Known issues

- The four matching corrections shipped in a single re-run; their individual
  contributions are not separately identified and no ablation was performed.
- Supplement detection still falls back to a November-month heuristic where
  PubMed omits the issue field.
- `10g_second_author_triangulation.R` returns zero rows and contributes nothing
  to the gender waterfall.
- Three `test-pipeline_semantics.R` failures predate this work and remain:
  practice_type coverage, citation coverage, and a 1,106 vs 1,067 row mismatch
  between `abstracts_cleaned.csv` and `abstracts_with_matches.csv`.

## [2026-04-19]

### Changed

- `10e_merge_demographics.R` is the sole writer to `abstracts_with_matches.csv`;
  all 12 producer scripts write sidecar CSVs only.
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
