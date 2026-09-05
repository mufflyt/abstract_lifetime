# Reproducibility

What a fresh clone gets by running `Rscript 00_run_all.R`, and what it does not.

---

## 0. Prior art this repository borrows from

The layout is a **research compendium** in the sense of Marwick, Boettiger and
Mullen: code, data and prose in one version-controlled project, with the
computational environment pinned. The conventions come from
[rrtools](https://github.com/benmarwick/rrtools) and
[rrrpkg](https://github.com/ropensci/rrrpkg) (rOpenSci) - a top-level runner,
analysis code under `R/`, committed inputs under `data/`, committed results
under `output/`, prose under `docs/`, and a lockfile pinning package versions.
This project reached that shape independently and adopts the names so reviewers
who know the convention can navigate it without reading a map.

Two departures are deliberate. There is no Docker image, so the environment is
pinned by `renv.lock` alone and the R version is recorded rather than enforced.
And the compendium is not an R package: there is no `DESCRIPTION` and the code
is sourced, not installed, which costs the automatic dependency declaration a
package would give and is why `tests/testthat/test-dependency_lockfile.R`
checks the lockfile against a source scan instead.

Data validation uses [pointblank](https://github.com/posit-dev/pointblank)
(Iannone & Vanderkam) rather than a hand-rolled validator; see
`config/data_contract.yml` and `R/utils_data_contract.R`.

The matching methodology's closest published relative is the IntoValue
programme ([intovalue-data](https://github.com/maia-sh/intovalue-data),
[IntoValue2](https://github.com/quest-bih/IntoValue2), MIT), discussed in the
manuscript Methods.

## 1. Environment

| Requirement | Value |
|---|---|
| R | ≥ 4.4 (developed and verified on 4.4.2) |
| Seed | `set.seed(cfg$pipeline$seed)` = 42, in `00_run_all.R`. No analysis step is stochastic; the seed matters only to `scripts/build_gold_standard.R`, which samples the validation set. |
| Dependency manager | **None.** There is no `renv.lock`, no `DESCRIPTION` and no package-version pinning. `.gitignore` reserves `renv/` paths but no `renv` project exists. Package versions are whatever the machine has. |
| Working-directory resolution | `here::here()` throughout; run from the project root. |

**Packages** (from `library()` calls across `R/`, `scripts/` and `shiny/`):

- Core: `here`, `config`, `cli`, `readr`, `dplyr`, `tidyr`, `purrr`, `stringr`,
  `tibble`
- Text and matching: `stringdist`, `stringi`, `digest`, `humaniformat`
- Web and APIs: `rvest`, `xml2`, `httr`, `jsonlite`, `rentrez`
- Statistics: `survival`, `broom`
- Demographics: `gender`
- Figures: `ggplot2`, `scales`, `DiagrammeR`, `htmlwidgets`, `webshot2`,
  `flowchart`, `survminer`
- Database: `DBI`, `duckdb` (NPI fallback only)
- Shiny: `shiny`, `bslib`, `DT`, `shinyjs`, `googlesheets4`, `rsconnect`
- Tests: `testthat`, `shinytest2` (optional), `irr` (optional)
- Gender tier 1: `mysterycall` and `npi`

### The one pinned external package

`R/09k_gender_from_nppes.R` calls
`mysterycall::mysterycall_nppes_gender()`. `mysterycall` is a development
package (`1.6.3.9000`) under active change, so depending on its `main` branch
would couple this analysis to a moving target. **Install it at the recorded
commit:**

```r
remotes::install_github("mufflyt/mysterycall@42d66d92ef52a0f85d1f7c61208c2ddd79d9c06e")
```

Installed here on 2026-09-04 from that SHA (311 exports). The script degrades
with a clear message rather than failing if the package is absent, and will not
overwrite an existing sidecar in that case — every other gender tier is
unaffected.

`irr` is **not installed** on the machine that produced the current outputs,
which is why `output/interrater_agreement.csv` reports `cohens_kappa = NA`
rather than failing. `shinytest2` is likewise absent, which is why the browser
end-to-end suite skips.

---

## 2. Credentials and environment variables

| Variable | Used by | Required? | Effect if absent |
|---|---|---|---|
| `ENTREZ_KEY` | `rate_limited_search()`, `fetch_pubmed_details()`, `fetch_pubmed_xml()` | No | Rate drops from 10 to 3 requests/second. A cold PubMed run takes roughly 3× longer. |
| `CROSSREF_EMAIL` | CrossRef polite pool | No | Requests leave the polite pool; CrossRef may throttle. |
| `PIPELINE_EMAIL` | OpenAlex and CrossRef `mailto` | No | Same. |
| `GENDERIZE_API_KEY` | `R/09c_author_characteristics.R` | No | genderize.io free tier is 100 names/day; beyond that the fallback silently returns nothing and gender tier 10 loses coverage. |
| `GOOGLE_SHEETS_ID` | Shiny app | No | Falls back to `config.yml: google_sheet_id`. |
| `shiny/adjudication_app/google_credentials.json` | `googlesheets4` service-account auth | **Yes, for the app** | `gs4_deauth()` — the app runs read-only from the bundled CSVs and cannot record decisions. |

`.Renviron.example` documents only `ENTREZ_KEY` and `CROSSREF_EMAIL`; the other
three are documented in the README but not in the example file.

---

## 3. Dependency classification

Using the five categories from the specification.

### Category 1 — fully reproducible from public sources

- NPPES registry gender (`gender_from_nppes.csv`) — public registry, no key,
  cached per NPI under `data/cache/nppes_gender/`

- Europe PMC search (`data/processed/europmc_candidates.csv`)
- OpenAlex search and citation metrics (`openalex_candidates.csv`,
  `publication_metrics.csv`)
- Semantic Scholar search (`semantic_scholar_candidates.csv`)
- ORCID lookups (`orcid_enrichment.csv`, `orcid_demographics.csv`,
  `gender_from_orcid.csv`)
- CMS Open Payments (`gender_from_open_payments.csv`)
- CrossRef (`crossref_candidates.csv`)
- All pure computation: `02_clean_abstracts.R` (given its input),
  `04_score_matches.R`, `05_adjudicate.R`, `06`, `07`, `08`,
  `strobe_flowchart.R`

### Category 2 — reproducible but better with credentials

- Everything reading PubMed: `03_search_pubmed.R`,
  `02b_backfill_abstract_text.R`, `09_enrich_authors.R`,
  `09b_enrich_pub_types.R`, `09f`, `09h`. Works without `ENTREZ_KEY` at one
  third the rate.
- genderize.io fallback in `09c` — usable without a key for 100 names/day.

### Category 3 — reproducible only while the cache survives

- **`data/cache/sd_html/` (1,154 files) is the single most fragile asset in the
  project.** ScienceDirect returns **HTTP 403** to this machine
  (verified 2026-09-03 against the 2012 supplement URL). Without the cache,
  neither `01b_parse_web.R` nor
  `scripts/backfill_sciencedirect_snippets.R` can run, and the cohort cannot be
  rebuilt at all. The directory is **gitignored**, so it exists on exactly one
  machine and in whatever backups of that machine exist.
- `data/cache/checkpoints/pubmed_search_checkpoint.rds` (35 MB) holds the only
  complete record of the search results that produced `match_scores.csv`; the
  CSV rendering of it has been overwritten (see
  [FAILURE_MODES.md](FAILURE_MODES.md) F2). Also gitignored.

### Category 4 — requires private or manual data

- `output/manual_review_decisions.csv` — 1,263 human decisions by three
  reviewers over 13 days. **Tracked in git**, so a clone gets them, but they
  cannot be regenerated by running code.
- `data/validation/gold_standard.csv` — 50 manually verified abstracts.
  Tracked.
- `data/validation/international_gender_lookup.csv` — 300 hand-curated names.
  Tracked.
- `data/validation/teaching_hospital_names.txt` — 2,754 ACGME names, an
  external snapshot with no retrieval script. Tracked.
- The Google Sheet — the live decision store. Requires a service-account key
  that is not in the repository.

### Package versions

`renv.lock` records the version of every package this project uses, together
with the R version and the exact GitHub commit for `mysterycall`. It is
generated by `Rscript scripts/build_lockfile.R` and consumed by
`renv::restore()`.

renv is deliberately **not activated** for the project. Activating it would add
`renv/activate.R` and rewrite `.Rprofile`, changing how every script and both
workflows resolve libraries. The lockfile is a record in renv's format, so it
restores on demand without imposing renv on anyone who just wants to run a
script.

`tests/testthat/test-dependency_lockfile.R` keeps it honest: it fails when the
code uses a package the lockfile does not carry, when the lockfile pins one the
code no longer references, or when the `mysterycall` commit in `renv.lock`
disagrees with the pin in either workflow. A lockfile nobody checks drifts
silently, which is worse than not having one.

Two known limits. A full `renv::restore()` is not run in CI, because `duckdb`,
`pdftools`, `rsvg` and `webshot2` need system libraries and the job would be
slow and brittle; the tests verify the lockfile still describes this codebase,
not that it rebuilds. And `shinytest2` is referenced but not locked: every use
site guards it with `skip_if_not_installed()`, so it is optional by design.

### Category 5 — currently irreproducible

- **NPI matching** (`R/10_npi_matching.R`, `data/processed/npi_matches.csv`,
  689 rows). Both inputs now come from `config.yml: external_data` and can be
  overridden with `ABOG_NPI_PATH` / `NPPES_DUCKDB_PATH`. Neither resolves to a
  usable source on this machine today:

  - The ABOG pool **is present** at the location given by `ABOG_NPI_PATH`
    (`.Renviron`, gitignored; it is no longer committed to `config.yml`), but the
    `LATEST` symlink has been repointed upstream since the shipped
    `npi_matches.csv` was built. The current target is an ABOG *workforce*
    export: 79,400 rows using `first`/`last`/`middle`/`subspecialty_name`
    instead of `first_name`/`last_name`/`middle_name`/`subspecialty`, carrying
    **no gender column at all**, and with an NPI for only **411 of 79,400
    rows**. `R/10_npi_matching.R` now maps the renamed columns and runs to
    completion against it, but produces 1 NPI instead of 265 and no gender, so
    it refuses to overwrite the richer existing sidecar and writes
    `npi_matches_nogender.csv` instead.
  - The NPPES DuckDB mirror **is present** on an external volume (84 GB), but
    its mount point is not stable: the volume name gains a numeric suffix
    depending on how many copies of that drive macOS has mounted, so a path
    that worked yesterday may not resolve today and the taxonomy fallback is
    skipped with a warning. Since 2026-09-05 the path is no longer committed at
    all — set `NPPES_DUCKDB_PATH` in `.Renviron` (see `.Renviron.example`) and
    the stage picks it up; leave it unset and the stage skips cleanly.

  **Gender no longer depends on this file.** Since 2026-09-04 tier 1 of the
  waterfall is `R/09k_gender_from_nppes.R`, which reads registrant-reported sex
  from the public NPPES registry keyed on the NPI, so 263 of the 267
  registry-sourced genders are regenerable from a public source. The ABOG
  export is still needed for `npi_state` and `npi_subspecialty` — neither of
  which is a model term — so `npi_matches.csv` remains a **category 4**
  artefact: usable, not currently regenerable, and no longer load-bearing for
  any reported estimate.
- **The original ScienceDirect scrape** — HTTP 403, see category 3.
- **2017 abstract text** — `scripts/jmig_2017_scraper.js` fails on CORS at the
  Elsevier SSO redirect; the Wayback Machine has no snapshots. 96 of 97 abstracts
  from AAGL 2017 have no text and never will without institutional PDF access.

---

## 4. What `Rscript 00_run_all.R` actually gives a fresh clone

**You get**, because the inputs are tracked in git:

- The parsed cohort (`abstracts_parsed_web.csv`, `abstracts_cleaned.csv`) —
  reused, not rebuilt: `01b` short-circuits on the existing CSV.
- Scores, classification, adjudication join, all five aims, the three model
  objects, tables and figures.
- The headline result 178/1,051 = 16.9%.

**You do not get**:

| Missing | Why |
|---|---|
| A rebuilt cohort | `01b` short-circuits; and if you delete the CSV, ScienceDirect returns 403. |
| A rebuilt candidate pool | `pubmed_candidates.csv` is **gitignored**. A fresh clone has none, so `04_score_matches.R` fails at `read_csv()`. **The pipeline cannot run end to end from a clean clone.** |
| The demographics block | `00_run_all.R` never calls `10e_merge_demographics.R`. See [FAILURE_MODES.md](FAILURE_MODES.md) F8. |
| NPI columns | Category 5. |
| `output/tables/` and most of `output/figures/` | Gitignored except the six README figures. |
| A Shiny deployment | Needs `google_credentials.json` and an `rsconnect` account. |
| Cohen's kappa | Needs `irr`. |
| The browser end-to-end tests | Need `shinytest2`. |

**The critical gap**: `data/processed/pubmed_candidates.csv` is 98 MB and
gitignored (`.gitignore`, last line). It is an input to `04_score_matches.R`,
`05_adjudicate.R` and `06_analyze_results.R`. A clean clone therefore stops at
step 4 unless the full search is re-run — which takes hours and, per
[FAILURE_MODES.md](FAILURE_MODES.md) F2, will produce a *different* pool from
the one the shipped scores were computed against.

---

## 5. Minimum viable reproduction

To reproduce the **reported numbers** without re-running the search layer:

```r
# Everything below reads only tracked files.
source("R/06_analyze_results.R")   # aims 1-5, models, final_analytical_dataset
source("R/07_make_tables.R")       # tables 1-4
source("R/08_make_figures.R")      # figures 1-6, S1-S4
source("R/strobe_flowchart.R")     # cohort figure, with assertions
testthat::test_dir("tests/testthat")
```

`output/abstracts_with_matches.csv` and `output/manual_review_decisions.csv` are
both tracked, so this path works from a clean clone and reproduces
178/1,051 = 16.9% exactly.

To reproduce **from the cohort forward** you additionally need
`pubmed_candidates.csv`, which means either a copy of the 98 MB file or a
multi-hour re-run of `03`, `03b` and `03c` with `ENTREZ_KEY` set.

To reproduce **from the source documents** you need institutional
ScienceDirect access, and even then the listing-truncation problem
([FAILURE_MODES.md](FAILURE_MODES.md) F1) means you should not reproduce the
current cohort — you should build a larger one.

---

## 5b. Regenerating the documentation itself

`docs/data_inventory.csv`, `docs/data_dictionary.csv` and
`docs/DATA_DICTIONARY.md` are produced by `scripts/build_docs_metadata.R`. They
join hand-authored prose in `docs/_meta/` to counts recomputed from the live
tree, and the script **fails** if the two halves disagree about which files or
columns exist. Before 2026-09-04 they were committed with no producer at all —
the orphan-artefact pattern recorded as [FAILURE_MODES.md](FAILURE_MODES.md)
F15.

```r
Rscript scripts/build_docs_metadata.R      # docs/*.csv and DATA_DICTIONARY.md
Rscript scripts/audit_cohort_completeness.R # ingestion vs the Crossref deposit
```

The second needs network access to Crossref (public, unauthenticated) and caches
one JSON per congress under `data/cache/crossref_supplements/`.

---

## 6. Recommendations, in order of cost

1. Commit an `renv.lock`. Nothing else in this list is verifiable without one.
2. Move `data/cache/sd_html/` off the single machine — it is unreplaceable and
   currently unbacked in version control.
3. Move the two NPI paths into `config.yml` and fail loudly when absent.
4. Either commit `pubmed_candidates.csv` (compressed, ~10 MB) or ship a
   reconstruction script that rebuilds it from
   `pubmed_search_checkpoint.rds` plus the four supplementary candidate CSVs.
5. Add `run_demographics.R` to `00_run_all.R`.
