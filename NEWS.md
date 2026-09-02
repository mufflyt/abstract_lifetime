# NEWS

## 2026-04-28

Recovered from an external drive on 2026-09-01; this work was completed on
2026-04-28 but never committed. See `docs/technical_appendix.Rmd` section A12
for the full write-up.

### Matching corrections

Four defects in the matching layer, all biasing in the same direction — each
suppressed true abstract-to-publication matches.

- **Session-number prefixes stripped** (`utils_text.R`, `02_clean_abstracts.R`).
  The 2013, 2017, 2018, and 2021 congress programs prefix titles with a session
  number (`"4 - Laparoscopic..."`). The prefix entered PubMed Strategy 1 as part
  of the title phrase, which then matched nothing. Because the affected
  congresses are a non-random subset of years, this confounded the time trend.
  Stripped via `^[0-9]+\s+[-–]\s*`; the required space before the dash
  protects titles like `"5-year outcomes..."`.
- **Non-article publication types excluded** (`build_date_filter()`). Letters,
  comments, editorials, errata, and retractions were eligible candidates. They
  carry the title and authors of the paper they discuss, so they score highly on
  the two heaviest components and can outrank the genuine publication. Now
  excluded at the search layer rather than post-hoc.
- **JMIG supplement detection narrowed** (`is_supplement_article()`). The test
  matched on journal + volume + year, but a supplement shares its volume with
  that volume's regular issues — so every regular JMIG article in the matching
  volume and year was excluded. This was the most consequential defect: AAGL
  abstracts are likeliest to publish in JMIG within about a year, exactly the
  window being blanked. Now requires the Issue field to contain `"Suppl"`, or
  November publication where PubMed omits the issue.
- **Title phrase search fixed** (`build_search_strategies()`). Strategy 1
  dropped tokens under three characters, which does not shorten the phrase — it
  produces a word sequence that appears in no title. Now takes a consecutive
  8-word window preserving stopwords, anchored at the first token of 3+
  characters.

### Gender resolution transparency

- **`gender_conflict` and `gender_n_sources`** populated for every row, recording
  the shape of the evidence rather than only the winner of the priority
  waterfall. Enables sensitivity analysis restricted to uncontested assignments.
- **`data/processed/gender_conflicts.csv`** — 277 cross-source disagreements
  with competing values.
- **Two new waterfall sources**: OpenAlex author search (157 resolutions),
  CMS Open Payments (16). `gender_unified` coverage 98.8%.
- **Second-author triangulation returns zero rows.** Script retained (the
  senior-author equivalent does resolve names) but contributes nothing; it
  should not be counted as an active source.

### PubMed metadata

- `parse_pubmed_xml()` extracts `JournalIssue/Issue` as `pub_issue`, required by
  the corrected supplement test.

### New outputs

- `output/final_analytical_dataset.csv` — unified dataset (1,067 rows x 90
  columns) with demographics and human decisions merged, exported by
  `06_analyze_results.R` for external analysis.
- `docs/aagl_abstract_programmatic.Rmd` / `.docx` — programmatic abstract draft.

### Regenerated

Processed data, Cox/KM/logistic models, and result tables re-run against the
corrected pipeline. Figure set renamed (`figure2_km_curve`, `figure3_km_by_year`,
`figure4_subgroup_rates`, `figure5_cox_forest`, `figure6_time_to_pub`,
`figureS1`-`figureS4`); stale `figure2_time_to_pub`, `figure3_km_curve`,
`figure4_strategy_perf`, and `figure5_score_dist` files removed.

### Denominator defect fixed (issue #2)

`R/05_adjudicate.R` dropped abstracts whose best candidate predates the
conference out of the cohort. `excluded` describes the candidate, not the
abstract, and the Cochrane MR000005 denominator is abstracts presented. The 39
are retained and counted as unpublished. Nothing downstream needed changing —
`06`, `07`, `08` and the Shiny app already treated `excluded` as unpublished.

Publication rate **17.2% → 16.9%** (95% CI 14.8–19.3), cohort 1,067 → 1,106,
published 174 → 178. Four of the 39 carry a reviewer's `manual_decision ==
"match"`, so the filter was also discarding confirmed publications; the two
effects partly offset. Details and the four PMIDs in appendix A12.7.

### CI restored

CI had failed on `main` since at least 2026-04-19. Two causes, neither a real
regression: `test-shiny_app.R` read gitignored artefacts with no existence
guard (impossible to pass in a fresh checkout), and two coverage thresholds
were unsatisfiable by construction rather than merely unmet. Suite is now
392 passing / 0 failing locally, and green in a tracked-files-only checkout.

### Known gaps

- The four matching corrections shipped in one re-run, so their individual
  contributions are not separately identified. No ablation was performed.
- Supplement detection still falls back to a November-month heuristic where
  PubMed omits the issue field.
- Three pre-existing `test-pipeline_semantics.R` failures remain (practice_type
  coverage, citation coverage, and a 1,106 vs 1,067 row mismatch between
  `abstracts_cleaned.csv` and `abstracts_with_matches.csv`). All three predate
  this work; the gender-coverage failure that also predated it now passes.

## 2026-04-19

### Demographics pipeline hardening

- **Single merge point**: `10e_merge_demographics.R` is now the sole writer to `abstracts_with_matches.csv`. All 12 producer scripts write sidecar CSVs only.
- **Unified gender column**: Replaced dual `first_author_gender`/`gender_unified` with a single `gender_unified` column using a 10-tier priority waterfall (NPI > OpenAlex > PubMed > OB/GYN pubs > OpenAlex search > ORCID > Open Payments > senior triangulation > second triangulation > SSA).
- **Gender coverage**: 73.9% -> 99% (1056/1067).
- **Reversible blanking**: PubMed-derived demographics for non-confirmed matches are blanked via a `demographics_from_matched_pub` flag rather than destructive NA assignment.
- **Orchestrator script**: `R/run_demographics.R` runs all demographic producers in dependency order.

### NPI matching enhancements (isochrones-inspired)

- **Additional name sources**: 5 gender enrichment sidecars (PubMed, OB/GYN pubs, OpenAlex search, Open Payments, ORCID) now feed full names into NPI matching.
- **NPPES taxonomy fallback**: Queries `temporal_all_years_fixed` with OB/GYN taxonomy filtering (`207V%`) for authors not in the ABOG pool.
- **Middle initial scoring**: +5 pts for middle initial agreement (from ABOG pool and NPPES).
- **City matching**: +10 pts when PubMed affiliation city matches NPPES practice city.
- **Temporal scoring**: +5 pts when NPI was enumerated before congress year.
- **Initial-only authors**: NPPES fallback now includes authors without full names.
- **NPI high-confidence**: 248 -> 278 (40.3% of US authors).
- **State coverage**: 10% -> 31%. Subspecialty coverage: 17% -> 36%.

### Shiny adjudication app

- Google Sheet link always visible in sidebar (hardcoded URL).
- Auto-advance to next abstract + scroll to top after saving a decision.
- Decision form resets (radio, PMID, notes) after save.
- Removed conflict confirmation modal — all reviewer decisions saved directly.
- "Show unreviewed only" filter now excludes AUTO (algorithm) rows.
- `deploy.R` script auto-slims `pubmed_candidates.csv` for bundle.
- Auto-deploy added as final step in `00_run_all.R`.

### New scripts

- `R/09i_gender_from_openalex.R` — Gender from OpenAlex author search.
- `R/09j_gender_from_open_payments.R` — Gender from CMS Open Payments database.
- `R/10g_second_author_triangulation.R` — Name resolution via second coauthor PubMed search.
- `R/run_demographics.R` — Demographics pipeline orchestrator.
- `shiny/adjudication_app/deploy.R` — Bundle preparation + shinyapps.io deployment.

### Technical appendix

- Added section A10.11: Coauthor Triangulation for Name Disambiguation (senior + second author PubMed co-publication search, results and limitations).

## 2026-04-18 (earlier)

### Gender enrichment

- `R/09f_enrich_gender_from_pubmed.R` — PubMed full-name search for gender resolution.
- `R/09g_gender_from_orcid.R` — Gender from ORCID person records.
- `R/09h_gender_from_obgyn_pubs.R` — Gender from OB/GYN publication author search.

## 2026-04-17

### Initial pipeline

- 12-congress pipeline (2012-2023), 1070 oral abstracts.
- 6-strategy PubMed search + 4 supplementary databases + DOI-chain reverse citations.
- 10-component composite scoring with Cochrane MR000005 5-tier classification.
- NPI matching via ABOG pool (60,846 board-certified OB/GYNs).
- Shiny adjudication app with Google Sheets backend.
- Full manuscript + technical appendix with inline R code.
