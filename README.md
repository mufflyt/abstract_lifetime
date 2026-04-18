# Abstract Lifetime

**Publication Rate, Time to Publication, and Predictors of Full Publication Among Oral Presentations at the AAGL Global Congress, 2012-2023**

A fully automated, reproducible pipeline that tracks whether conference abstracts presented at the AAGL Global Congress on Minimally Invasive Gynecology progress to full peer-reviewed publication. Designed to meet the methodological standards recommended by the Cochrane review on abstract-to-publication conversion (Scherer et al., MR000005).

## Key Results

| Metric | Value |
|--------|-------|
| Abstracts analyzed | 1,106 oral presentations |
| Congresses covered | 12 (2012-2023) |
| Definite matches | 75 (6.8%) |
| Probable matches (pending review) | 229 (20.7%) |
| Definite + probable rate | 27.7% |
| Median time to publication | 10.8 months (IQR 3.8-16.7) |
| Only significant predictor | US-based affiliation (HR 2.65, p=0.004) |
| Algorithm sensitivity | 100% |
| Algorithm specificity | 69.4% |

## Pipeline Architecture

```
00_run_all.R                    # Master pipeline (runs all steps)
R/
  01b_parse_web.R               # Scrape JMIG supplements from ScienceDirect
  01d_tag_session_type.R        # Classify Oral vs Video from TOC
  02_clean_abstracts.R          # Normalize + extract 20+ predictor variables
  03_search_pubmed.R            # 6-strategy PubMed search per abstract
  03b_search_crossref.R         # CrossRef + Europe PMC + OpenAlex + Semantic Scholar
  03c_doi_chain_search.R        # Reverse citation search via OpenAlex
  04_score_matches.R            # 10-component composite scoring
  05_adjudicate.R               # Cochrane-aligned 5-tier classification
  09b_enrich_pub_types.R        # PubMed publication type extraction
  09_enrich_authors.R           # Author names + affiliations from PubMed XML
  09c_author_characteristics.R  # Gender, ACOG district, practice type, subspecialty
  09d_enrich_metrics.R          # Citation counts + journal impact from OpenAlex
  06_analyze_results.R          # KM, Cox PH, logistic regression, sensitivity
  07_make_tables.R              # 4 publication-quality tables
  08_make_figures.R             # 6 main + 4 supplementary figures
  utils_*.R                     # Reusable utilities (scoring, text, states, etc.)
```

## Quick Start

```r
# Full pipeline (3-4 hours cold, ~30 min with cache)
Rscript 00_run_all.R

# Render manuscript
rmarkdown::render("docs/abstract_results_section.Rmd")
rmarkdown::render("docs/technical_appendix.Rmd")

# Run tests (391 tests)
testthat::test_dir("tests/testthat")

# Deploy Shiny adjudication app
Rscript deploy_shiny.R aagl-adjudication
```

## Search Strategy

Six PubMed strategies per abstract, plus four supplementary databases and a novel DOI-based reverse citation search:

| Source | Method |
|--------|--------|
| PubMed | Title phrase, first/last author, author+keywords, distinctive phrase, author broad |
| CrossRef | Title-based (low-hit abstracts) |
| Europe PMC | Multi-strategy title + author |
| OpenAlex | Keyword search with PMID resolution |
| Semantic Scholar | Title-based |
| DOI-chain | Reverse citations via OpenAlex (papers citing the abstract DOI) |

## Matching Algorithm

10-component composite score (title similarity, abstract semantic, first/last/coauthor match, journal relevance, keywords, publication date) with Cochrane-aligned classification:

- **Definite** (score >= 7 + text evidence): auto-accepted
- **Probable** (score 3-7 + text evidence): human review required
- **Possible** (weak evidence or ties): human review required
- **No match** (score < 3): no viable publication found
- **Excluded**: candidate published before the conference

## Shiny Adjudication App

Live at **https://mufflyt.shinyapps.io/aagl-adjudication/**

Web-based tool for blinded manual review of probable/possible matches. Features:
- Side-by-side abstract vs candidate comparison
- Per-component score breakdowns
- Google Sheets backend for multi-reviewer collaboration
- Keyboard shortcuts (m/n/s/Enter/arrows)
- Filters by congress year, classification tier
- Conflict detection between reviewers

## Variable Extraction

59 columns per abstract, extracted via NLP and API enrichment:

| Category | Variables |
|----------|-----------|
| Study characteristics | study_design (12 categories), research_category, primary_procedure, is_rct, sample_size, is_multicenter |
| Cochrane variables | has_funding, has_industry, has_trial_registration, has_irb_statement, has_numeric_results, is_database_study |
| Author demographics | first_author_gender (75% coverage), practice_type (87%), subspecialty (54%), ACOG district (92% of US), n_authors |
| Publication outcomes | pub_type_canonical, cited_by_count, journal_impact_proxy, months_to_pub |
| Match quality | classification, best_score, 10 score components, has_tie |

## Figures

**Main manuscript** (6 figures):
1. STROBE flow diagram
2. Kaplan-Meier cumulative publication curve (pooled)
3. KM curves stratified by congress year
4. Publication rate by subgroup (study design, practice type, subspecialty, geography, gender)
5. Cox PH forest plot (hazard ratios with 95% CI)
6. Time to publication histogram

**Supplementary** (4 figures):
S1. Publication rate by congress year
S2. Search strategy comparison
S3. Score distribution by classification
S4. Classification breakdown by year

## Testing

391 tests across 13 test files:
- Unit tests for all utility functions (text normalization, scoring, state parsing, ACOG mapping, gender, affiliation classification, study design, procedures)
- Semantic tests validating pipeline output plausibility against Cochrane benchmarks
- Integration tests for Shiny app data integrity

```r
testthat::test_dir("tests/testthat")
# [ FAIL 0 | WARN 13 | SKIP 18 | PASS 391 ]
```

## Reproducibility

- All API responses cached to disk (PubMed XML, ScienceDirect HTML)
- RDS checkpoints at each search stage (resume after interruption)
- `set.seed(42)` for reproducible sampling
- Manuscript Rmd files use inline R code pulling from pipeline CSVs
- Full re-run with populated cache: ~30 minutes

## Data

- `data/processed/`: Intermediate pipeline outputs (abstracts, candidates, scores, author characteristics)
- `output/`: Final analysis results (CSVs, figures, tables)
- `data/cache/`: API response cache (gitignored for large files)
- `data/validation/`: Gold standard + ACGME teaching hospital names

## Requirements

- R >= 4.4
- Key packages: tidyverse, rentrez, xml2, httr, jsonlite, survival, gender, stringdist, DiagrammeR
- Optional: googlesheets4 (Shiny backend), rsconnect (deployment), webshot2 (flow diagram PNG)

## Citation

If you use this pipeline or methodology, please cite:

> Muffly T. Publication Rate, Time to Publication, and Predictors of Full Publication Among Oral Presentations at the AAGL Global Congress, 2012-2023: A Retrospective Cohort Study. *Journal of Minimally Invasive Gynecology*. 2026.

## License

This project is for academic research purposes.
