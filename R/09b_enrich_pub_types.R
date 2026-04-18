# 09b_enrich_pub_types.R — Add publication-type information for every matched
# PMID. Uses the cached PubMed XML at data/cache/pubmed_xml/ where available;
# falls back to a fresh EFetch for any PMID not yet cached.
#
# Writes data/processed/matched_pub_types.csv (pmid, pub_types, pub_type_canonical)
# then merges pub_types + pub_type_canonical into output/abstracts_with_matches.csv.

suppressPackageStartupMessages({
  library(here); library(config); library(cli); library(dplyr); library(readr)
  library(stringr); library(tibble); library(purrr); library(xml2); library(rentrez)
})

cfg <- config::get(file = here("config.yml"))
source(here("R", "utils_pub_types.R"))
source(here("R", "utils_pubmed.R"))

cache_dir <- here("data", "cache", "pubmed_xml")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

matches_path <- here("output", "abstracts_with_matches.csv")
matches <- read_csv(matches_path, show_col_types = FALSE)

# Include reviewer-overridden PMIDs too, so human matches get typed.
decisions_path <- here("output", "manual_review_decisions.csv")
decisions <- if (file.exists(decisions_path))
  read_csv(decisions_path, show_col_types = FALSE) else tibble()

target_pmids <- unique(c(
  as.character(matches$best_pmid),
  if ("manual_pmid" %in% names(decisions)) as.character(decisions$manual_pmid) else character()
))
target_pmids <- target_pmids[grepl("^[0-9]+$", target_pmids)]

cli_h1("Publication-type enrichment")
cli_alert_info("Unique PMIDs to type: {length(target_pmids)}")

has_key <- nchar(Sys.getenv("ENTREZ_KEY", "")) > 0
delay <- if (has_key) 1 / cfg$pubmed$rate_limit_with_key else 1 / cfg$pubmed$rate_limit_per_sec

extract_types <- function(pmid) {
  xt <- fetch_pubmed_xml(pmid, cache_dir, delay)
  if (is.na(xt) || nchar(xt) < 100) return(NA_character_)
  doc <- tryCatch(read_xml(xt), error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)
  nodes <- xml_find_all(doc, "//PubmedArticle//PublicationTypeList/PublicationType")
  if (length(nodes) == 0) return(NA_character_)
  paste(xml_text(nodes), collapse = "; ")
}

results <- vector("list", length(target_pmids))
for (i in seq_along(target_pmids)) {
  results[[i]] <- tibble(pmid = target_pmids[i],
                         pub_types = extract_types(target_pmids[i]))
  if (i %% 50 == 0) cli_alert_info("  {i}/{length(target_pmids)}")
}

type_tbl <- bind_rows(results) |>
  mutate(pub_type_canonical = vapply(pub_types, canonical_pub_type, character(1)))

write_csv(type_tbl, here("data", "processed", "matched_pub_types.csv"))
cli_alert_success("Wrote matched_pub_types.csv ({nrow(type_tbl)} rows)")

cli_alert_info("Canonical type distribution:")
print(table(type_tbl$pub_type_canonical, useNA = "ifany"))

# --- Merge into abstracts_with_matches.csv ---
if (any(c("pub_types", "pub_type_canonical") %in% names(matches))) {
  matches$pub_types <- NULL
  matches$pub_type_canonical <- NULL
}
matches <- matches |>
  mutate(best_pmid = as.character(best_pmid)) |>
  left_join(type_tbl |> mutate(pmid = as.character(pmid)),
            by = c("best_pmid" = "pmid"))
write_csv(matches, matches_path)
cli_alert_success("Merged pub types into {basename(matches_path)}")
