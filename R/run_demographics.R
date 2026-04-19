#!/usr/bin/env Rscript
# run_demographics.R — Demographics pipeline orchestrator
#
# Runs all demographic producer scripts in dependency order, then merges.
# Each producer writes its own sidecar CSV to data/processed/.
# Only 10e_merge_demographics.R writes to output/abstracts_with_matches.csv.
#
# Usage:
#   Rscript R/run_demographics.R          # full pipeline
#   Rscript R/10e_merge_demographics.R    # re-merge only (after manual edits)
#
# Dependency order:
#   09c (base demographics) → 10b (OpenAlex names) → 10_npi (NPI matching) →
#   10d (ORCID) → 10f (senior triangulation) → 10g (second triangulation) →
#   09f (PubMed gender) → 09g (ORCID gender) → 09h (OB/GYN pubs gender) →
#   09i (OpenAlex gender) → 09j (Open Payments gender) → 10e (MERGE)

suppressPackageStartupMessages(library(here))

cli::cli_h1("Demographics Pipeline — Full Run")
start_time <- Sys.time()

steps <- list(
  list(script = "R/09c_author_characteristics.R",         label = "09c: Base demographics"),
  list(script = "R/10b_resolve_names_openalex.R",         label = "10b: OpenAlex full names"),
  list(script = "R/10_npi_matching.R",                    label = "10:  NPI matching"),
  list(script = "R/10d_orcid_demographics.R",             label = "10d: ORCID demographics"),
  list(script = "R/10f_senior_author_triangulation.R",    label = "10f: Senior author triangulation"),
  list(script = "R/10g_second_author_triangulation.R",    label = "10g: Second author triangulation"),
  list(script = "R/09f_enrich_gender_from_pubmed.R",      label = "09f: Gender from PubMed"),
  list(script = "R/09g_gender_from_orcid.R",              label = "09g: Gender from ORCID"),
  list(script = "R/09h_gender_from_obgyn_pubs.R",         label = "09h: Gender from OB/GYN pubs"),
  list(script = "R/09i_gender_from_openalex.R",           label = "09i: Gender from OpenAlex"),
  list(script = "R/09j_gender_from_open_payments.R",      label = "09j: Gender from Open Payments"),
  list(script = "R/10e_merge_demographics.R",             label = "10e: MERGE (sole writer)")
)

for (step in steps) {
  path <- here(step$script)
  if (!file.exists(path)) {
    cli::cli_alert_warning("Skipping {step$label} — file not found")
    next
  }
  cli::cli_h2("{step$label}")
  tryCatch(
    source(path, local = new.env(parent = globalenv())),
    error = function(e) cli::cli_alert_danger("{step$label} FAILED: {e$message}")
  )
}

elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
cli::cli_alert_success("Demographics pipeline complete in {elapsed} minutes")
