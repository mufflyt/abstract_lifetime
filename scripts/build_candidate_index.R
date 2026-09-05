#!/usr/bin/env Rscript
# build_candidate_index.R — write the committable slice of the candidate pool.
#
# data/processed/pubmed_candidates.csv is 136,339,347 bytes and gitignored, so
# every assertion about the candidate pool skipped in CI. Among them was
# "F2: every winning PMID resolves in the candidate pool", one of the
# pipeline's core invariants, which had therefore never run in CI at all.
#
# Those assertions read two columns. abstract_id and pmid together are 1.4 MB
# over 65,697 rows, which commits comfortably. The index is not a substitute
# for the pool: it answers membership and coverage questions, not anything
# about candidate metadata or scores.
#
# Usage: Rscript scripts/build_candidate_index.R

suppressPackageStartupMessages({library(readr); library(dplyr); library(here)})

src <- here("data", "processed", "pubmed_candidates.csv")
if (!file.exists(src)) {
  stop("data/processed/pubmed_candidates.csv is absent. This script runs on a ",
       "machine that has done a full pipeline run; the committed index is what ",
       "CI uses.")
}

idx <- read_csv(src, show_col_types = FALSE,
                col_select = c("abstract_id", "pmid")) |>
  mutate(pmid = as.character(pmid)) |>
  distinct() |>
  arrange(abstract_id, pmid)

out <- here("output", "candidate_pool_index.csv")
write_csv(idx, out)

cat(sprintf("wrote %s: %d rows, %d abstracts, %d distinct PMIDs, %.1f MB\n",
            out, nrow(idx), dplyr::n_distinct(idx$abstract_id),
            dplyr::n_distinct(idx$pmid), file.size(out) / 1e6))
