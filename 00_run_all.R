#!/usr/bin/env Rscript
# 00_run_all.R — Master pipeline for AAGL 2023 abstract-to-publication tracking
#
# Usage: source("00_run_all.R") or Rscript 00_run_all.R

library(here)
library(config)
library(cli)

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")
cfg <- config::get(file = here("config.yml"))
set.seed(cfg$pipeline$seed)

cli_h1("AAGL Abstract-to-Publication Pipeline")

# Step 1: Parse abstracts (web first, PDF fallback)
cli_h2("Step 1: Data Ingestion")
source(here("R", "01b_parse_web.R"))
if (!file.exists(here("data", "processed", "abstracts_parsed.csv"))) {
  cli_alert_warning("Web parsing failed or incomplete; falling back to PDF")
  source(here("R", "01_parse_pdf.R"))
}

# Step 1c: Compare sources (if both exist)
if (file.exists(here("data", "processed", "abstracts_parsed_web.csv")) &&
    file.exists(here("data", "processed", "abstracts_parsed_pdf.csv"))) {
  cli_alert_info("Comparing web vs PDF sources")
  source(here("R", "01c_compare_sources.R"))
}

# Step 1d: Tag session type (Oral vs Video) from JMIG TOC
cli_h2("Step 1d: Session Type Tagging")
source(here("R", "01d_tag_session_type.R"))

# Step 2: Clean
cli_h2("Step 2: Cleaning Abstracts")
source(here("R", "02_clean_abstracts.R"))

# Step 2b: Backfill abstract text from PubMed XML (2012-2018)
cli_h2("Step 2b: Abstract Text Backfill (PubMed XML)")
source(here("R", "02b_backfill_abstract_text.R"))

# Step 2c: Backfill abstract text from ScienceDirect snippets (2012-2018)
cli_h2("Step 2c: Abstract Text Backfill (ScienceDirect snippets)")
source(here("scripts", "backfill_sciencedirect_snippets.R"))

# Step 3: Search PubMed
cli_h2("Step 3: PubMed Search")
source(here("R", "03_search_pubmed.R"))

# Step 3b: Supplementary search (CrossRef + Europe PMC)
cli_h2("Step 3b: Supplementary Search")
source(here("R", "03b_search_crossref.R"))

# Step 3c: DOI-chain search (reverse citations via OpenAlex)
cli_h2("Step 3c: DOI-chain Search")
source(here("R", "03c_doi_chain_search.R"))

# Step 4: Score matches
cli_h2("Step 4: Scoring Matches")
source(here("R", "04_score_matches.R"))

# Step 5: Adjudicate
cli_h2("Step 5: Adjudication")
source(here("R", "05_adjudicate.R"))

# Step 5b: Publication-type enrichment for matched PMIDs
cli_h2("Step 5b: Publication Type Enrichment")
source(here("R", "09b_enrich_pub_types.R"))

# Step 5c: Author enrichment (full names + affiliations from PubMed XML)
cli_h2("Step 5c: Author Enrichment")
source(here("R", "09_enrich_authors.R"))

# Step 5d: Author characteristics (gender, ACOG district, counts)
cli_h2("Step 5d: Author Characteristics")
source(here("R", "09c_author_characteristics.R"))

# Step 5e: Citation counts + journal impact from OpenAlex
cli_h2("Step 5e: Citation Metrics")
source(here("R", "09d_enrich_metrics.R"))

# Step 5f: Backfill affiliation_raw from cached ScienceDirect HTML
cli_h2("Step 5f: Affiliation Backfill")
source(here("scripts", "backfill_affiliations_from_cache.R"))

# Step 5g: ORCID enrichment (career stage, subspecialty, works count)
cli_h2("Step 5g: ORCID Enrichment")
source(here("R", "09e_enrich_orcid.R"))

# Step 5h: NPI matching (US first authors → ABOG-NPI for subspecialty/gender)
cli_h2("Step 5h: NPI Matching")
source(here("R", "10_npi_matching.R"))

# Step 5h2: Gender enrichment via PubMed author search (last name + affiliation)
cli_h2("Step 5h2: Gender from PubMed Author Search")
source(here("R", "09f_enrich_gender_from_pubmed.R"))

# Step 5h3: Gender enrichment from ORCID person records (cached given-names)
cli_h2("Step 5h3: Gender from ORCID Person Records")
source(here("R", "09g_gender_from_orcid.R"))

# Step 5i: Fidelity checks (abstract vs published paper comparison)
cli_h2("Step 5i: Fidelity Checks")
source(here("R", "09e_fidelity_checks.R"))

# Step 6: Analyze
cli_h2("Step 6: Analysis")
source(here("R", "06_analyze_results.R"))

# Step 6b: Gold standard validation
cli_h2("Step 6b: Gold Standard Validation")
source(here("R", "validation_gold_standard.R"))

# Step 6c: Interrater agreement (Cohen's kappa)
cli_h2("Step 6c: Interrater Agreement")
source(here("R", "10_interrater.R"))

# Step 7: Tables
cli_h2("Step 7: Tables")
source(here("R", "07_make_tables.R"))

# Step 8: Figures
cli_h2("Step 8: Figures")
source(here("R", "08_make_figures.R"))

cli_h1("Pipeline Complete")
cli_alert_success("Results in: {here('output')}")
