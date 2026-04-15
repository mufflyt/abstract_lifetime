#!/usr/bin/env Rscript
# 00_run_all.R — Master pipeline for AAGL 2023 abstract-to-publication tracking
#
# Usage: source("00_run_all.R") or Rscript 00_run_all.R

library(here)
library(config)
library(cli)

cfg <- config::get(file = here("config.yml"))
set.seed(cfg$pipeline$seed)

cli_h1("AAGL 2023 Abstract-to-Publication Pipeline")

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

# Step 3: Search PubMed
cli_h2("Step 3: PubMed Search")
source(here("R", "03_search_pubmed.R"))

# Step 3b: Supplementary search (CrossRef + Europe PMC)
cli_h2("Step 3b: Supplementary Search")
source(here("R", "03b_search_crossref.R"))

# Step 4: Score matches
cli_h2("Step 4: Scoring Matches")
source(here("R", "04_score_matches.R"))

# Step 5: Adjudicate
cli_h2("Step 5: Adjudication")
source(here("R", "05_adjudicate.R"))

# Step 6: Analyze
cli_h2("Step 6: Analysis")
source(here("R", "06_analyze_results.R"))

# Step 7: Tables
cli_h2("Step 7: Tables")
source(here("R", "07_make_tables.R"))

# Step 8: Figures
cli_h2("Step 8: Figures")
source(here("R", "08_make_figures.R"))

cli_h1("Pipeline Complete")
cli_alert_success("Results in: {here('output')}")
