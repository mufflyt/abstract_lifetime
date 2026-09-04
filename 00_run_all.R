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

# Step 2d: Re-derive the study characteristics now that the text exists.
# 02 computes them from `search_text`, which for 2012-2018 is the title alone
# because the backfills above had not yet run. Without this step the covariates
# carry a step change at 2018/2019 that is a measurement artefact, not a trend.
# See docs/FAILURE_MODES.md F3.
cli_h2("Step 2d: Re-derive Study Characteristics")
source(here("R", "02d_rederive_predictors.R"))

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

# Step 5h4: Gender from any OB/GYN publication by the author (PubMed journal filter)
cli_h2("Step 5h4: Gender from OB/GYN Publications")
source(here("R", "09h_gender_from_obgyn_pubs.R"))

# Step 5h5: Gender from OpenAlex works search (same journal list, different index)
cli_h2("Step 5h5: Gender from OpenAlex Works Search")
source(here("R", "09i_gender_from_openalex.R"))

# Step 5h6: Gender from CMS Open Payments (Sunshine Act conference-window match)
cli_h2("Step 5h6: Gender from Open Payments")
source(here("R", "09j_gender_from_open_payments.R"))

# Step 5h6b: Gender from the NPPES registry (tier 1 of the waterfall).
# Registrant-reported sex keyed on the NPI that 10_npi_matching.R resolved. Not
# inferred from a name, and regenerable from a public registry - which the ABOG
# gender column it supersedes no longer is. See docs/FAILURE_MODES.md F16.
cli_h2("Step 5h6b: Gender from NPPES")
source(here("R", "09k_gender_from_nppes.R"))

# Step 5h7: Consolidate all recovered first names into first_author_first
cli_h2("Step 5h7: Consolidate first_author_first")
local({
  matches  <- readr::read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)
  ac       <- readr::read_csv(here("data", "processed", "author_characteristics.csv"), show_col_types = FALSE)
  pubmed_n <- readr::read_csv(here("data", "processed", "gender_from_pubmed.csv"), show_col_types = FALSE) |>
    dplyr::filter(!is.na(pubmed_full_first), nchar(pubmed_full_first) >= 2) |>
    dplyr::select(abstract_id, pubmed_full_first)
  ac_names <- ac |>
    dplyr::filter(!is.na(first_author_first)) |>
    dplyr::select(abstract_id, ac_first = first_author_first)
  matches <- matches |>
    dplyr::left_join(pubmed_n, by = "abstract_id") |>
    dplyr::left_join(ac_names, by = "abstract_id") |>
    dplyr::mutate(first_author_first = dplyr::coalesce(
      first_author_first, pubmed_full_first,
      orcid_first_name, obgyn_first_name,
      openalex_first_name, op_first_name, ac_first
    )) |>
    dplyr::select(-pubmed_full_first, -ac_first)
  n_first  <- sum(!is.na(matches$first_author_first))
  gender_col <- if ("gender_unified" %in% names(matches)) "gender_unified" else "first_author_gender"
  n_gender <- sum(!is.na(matches[[gender_col]]))
  orphan   <- sum(!is.na(matches[[gender_col]]) & is.na(matches$first_author_first))
  cli::cli_alert_success("first_author_first: {n_first}/{nrow(matches)} | gender: {n_gender}/{nrow(matches)} | orphans: {orphan}")
  readr::write_csv(matches, here("output", "abstracts_with_matches.csv"))
})

# Step 5h8: Demographics merge. R/10e_merge_demographics.R resolves the ten-tier
# gender waterfall and joins the NPI and ORCID sidecars onto
# output/abstracts_with_matches.csv. It was previously reachable only through
# R/run_demographics.R, so a clean run of this pipeline produced a dataset with
# no gender_unified, npi_* or state_unified columns, and 06_analyze_results.R
# then dropped those model terms without warning. See docs/FAILURE_MODES.md F8.
cli_h2("Step 5h8: Demographics Merge")
source(here("R", "10b_resolve_names_openalex.R"))
source(here("R", "10d_orcid_demographics.R"))
source(here("R", "10f_senior_author_triangulation.R"))
source(here("R", "10g_second_author_triangulation.R"))
source(here("R", "10e_merge_demographics.R"))

# Step 5i: Fidelity checks (abstract vs published paper comparison)
cli_h2("Step 5i: Fidelity Checks")
source(here("R", "09e_fidelity_checks.R"))

# Step 6: Analyze
cli_h2("Step 6: Analysis")
source(here("R", "06_analyze_results.R"))

# Step 6b0: Missing-data analysis. Reports item-level missingness, Little's
# MCAR test, and whether the 55 unresolved abstracts differ from the evaluated
# set - the assumption the publication-rate denominator rests on.
cli_h2("Step 6b0: Missing-Data Analysis")
source(here("R", "06b_missingness.R"))

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

# Step 8b: STROBE cohort flow chart. Derives every count from the pipeline files
# and asserts the arithmetic with stopifnot() before drawing.
cli_h2("Step 8b: STROBE Flow Chart")
source(here("R", "strobe_flowchart.R"))

# Step 9: Deploy Shiny adjudication app
cli_h2("Step 9: Deploy Shiny App")
deploy_script <- here("shiny", "adjudication_app", "deploy.R")
if (file.exists(deploy_script)) {
  source(deploy_script)
} else {
  cli_alert_warning("Deploy script not found — skipping Shiny deploy")
}

# Step 10: Record the environment this run was produced in.
cli_h2("Step 10: Session Snapshot")
source(here("R", "06c_session_snapshot.R"))

cli_h1("Pipeline Complete")
cli_alert_success("Results in: {here('output')}")
