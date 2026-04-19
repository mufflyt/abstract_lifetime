# NEWS

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
