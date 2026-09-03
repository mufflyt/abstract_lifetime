# Publication Search

How candidate publications are found for each AAGL abstract. Every query below
is transcribed from executable code, not from comments.

Five external sources are active today, plus one derived source:

| Source | Script | Scope | Active |
|---|---|---|---|
| PubMed (NCBI E-utilities) | `R/03_search_pubmed.R` | all 1,106 abstracts, 6 strategies | yes |
| CrossRef | `R/03b_search_crossref.R` | only abstracts with ≤ 2 PubMed hits | yes |
| Europe PMC | `R/03b_search_crossref.R` | all abstracts, 3 strategies | yes |
| OpenAlex | `R/03b_search_crossref.R` | all abstracts | yes |
| Semantic Scholar | `R/03b_search_crossref.R` | all abstracts, 2 strategies | yes |
| DOI-chain (reverse citation, via OpenAlex) | `R/03c_doi_chain_search.R` | all abstracts | yes |

All six write into a single pooled candidate table,
`data/processed/pubmed_candidates.csv`, keyed on (`abstract_id`, `pmid`).
Only PubMed-resolvable records enter the pool: CrossRef, Europe PMC, OpenAlex,
Semantic Scholar and the DOI chain all contribute by handing a PMID to
`fetch_pubmed_details()`. **A candidate with no PMID cannot be scored.**

---

## 1. PubMed — the six strategies, in executable order

Built by `build_search_strategies()` in `R/utils_pubmed.R:429-506` and executed
by `search_abstract()` (`R/utils_pubmed.R:544-582`) with
`retmax = 100` (`config.yml: pubmed.max_results_per_query`).

Every strategy is `AND`-ed with the shared date filter from
`build_date_filter()` (`R/utils_pubmed.R:320-329`):

```
2012/11/01:2026/04/01[PDAT]
AND NOT ("Letter"[PT] OR "Comment"[PT] OR "Editorial"[PT]
         OR "Published Erratum"[PT] OR "Retraction of Publication"[PT])
```

Note that the date window is **global**, not per-congress: a 2023 abstract can
match a 2012 paper and vice versa. Pre-congress candidates are handled later, at
scoring time, not here.

| # | Strategy | Query components | Purpose | Failure mode |
|---|---|---|---|---|
| 1 | `title` | `"<8 consecutive normalised title words, starting at the first word of ≥3 characters>"[TI] AND <date filter>` | Highest-precision hit: an exact title phrase. | Any wording change between abstract and paper (added subtitle, reordered clause, British/US spelling) returns zero. Emitted 0 hits for 1,739 of 1,742 abstract-queries in the last recorded run. |
| 2 | `first_author` | `"<first_author_normalized>"[1AU] AND <date filter>` | Papers where the presenter stayed first author. | Fails on name change, on transliteration differences, and whenever the ScienceDirect author string was unparseable (4 abstracts). |
| 3 | `last_author` | `"<last_author_normalized>"[LASTAU] AND <date filter>` | Papers where the senior author is unchanged. | Skipped for the 340 abstracts with no `last_author_normalized` — 336 because ScienceDirect truncated the author list with an ellipsis (`authors_truncated`), where the last visible name is deliberately set to `NA` rather than credited as senior author. |
| 4 | `author_keywords` | `"<first_author_normalized>"[AU] AND (<up to 4 TF keywords joined by OR>)[TIAB] AND <date filter>` | Author moved position but topic is stable. | Requires ≥ 2 keywords, so it is skipped for the 667 abstracts with an empty `keywords_str` — keywords are extracted from `abstract_text`, which `02_clean_abstracts.R` sees as empty for 2012–2018 because the text backfill runs afterwards. |
| 5 | `title_fragment` | `"<distinctive 4-word phrase>"[TIAB] AND <date filter>` | Catches a retitled paper that reuses a distinctive phrase. | `distinctive_phrase()` picks the 4-gram with the rarest tokens; a generic title yields a generic phrase and thousands of hits. |
| 6 | `author_broad` | `"<first_author_normalized>"[AU] AND <date filter>` | Catch-all. Highest yield. | Returns the author's entire 14-year output; common surnames return hundreds of irrelevant papers, which is why the score carries a −2 no-text-evidence penalty. |

Strategies 1, 2, 3 and 6 need only a title or an author, so they run for every
abstract. Strategies 4 and 5 need extracted keywords.

Per-PMID provenance is preserved: `search_abstract()` groups by PMID and stores
a semicolon-separated `strategies` string plus `n_strategies`. Aim 4 attributes
each confirmed match back to the strategies that found it.

### Self-match filtering

After details are fetched, `is_supplement_article()`
(`R/utils_pubmed.R:359-379`) drops candidates that are the AAGL supplement
itself. The current rule requires **all four** of:

- journal abbreviation matches `j minim invasive gynecol`,
- `pub_volume` ∈ `config.yml: pubmed.exclude_supplement_vol` (19–30),
- `pub_year` ∈ `pubmed.exclude_supplement_year` (2012–2023),
- `pub_issue` contains `suppl` **or**, when `pub_issue` is missing, `pub_month`
  starts with `nov`/`11`.

The last condition is the April 2026 correction (A12.3). Before it, every
regular JMIG article sharing a volume with a congress supplement was discarded —
precisely the journal where AAGL abstracts are most likely to publish.

**Two places still use the pre-correction rule**, and neither was updated:

- `R/03b_search_crossref.R:292-297, 336-341, 380-…` — the inline supplement
  filter applied to OpenAlex, Semantic Scholar and Europe PMC candidates tests
  journal + volume + year only, with no `Suppl` requirement.
- `R/utils_crossref.R:553-556` — `search_europmc()` drops **every**
  `j minim invasive gynecol` record in the year range, supplement or not.

Both over-exclude. They are documented here, not fixed.

### Rate limiting, caching, retries

- `rate_limited_search()` sleeps to hold 3 req/s, or 10 req/s when `ENTREZ_KEY`
  is set (`config.yml: pubmed.rate_limit_per_sec` / `rate_limit_with_key`).
- `fetch_pubmed_details()` batches PMIDs in groups of 100.
- `fetch_pubmed_xml()` caches one XML file per PMID under
  `data/cache/pubmed_xml/`, treating files under 100 bytes as invalid.
- Checkpoint: `data/cache/checkpoints/pubmed_search_checkpoint.rds`, written
  every `pubmed.cache_every_n` = 10 abstracts, holding `completed_ids`,
  `all_candidates` and `all_strategy_results`.

### Failed search vs genuine zero result — you cannot tell them apart

`rate_limited_search()` catches every error, emits a `cli` warning and returns
`NULL` (`R/utils_pubmed.R:69-73`). `search_abstract()` treats `NULL` exactly like
an empty result set. `03_search_pubmed.R` then records `n_results = 0` for that
strategy and marks the abstract **complete** in the checkpoint.

There is no retry, no error column, and no distinction in any output file
between "PubMed returned nothing" and "the request failed". A transient NCBI
outage during a run therefore produces a permanently under-searched abstract
that the checkpoint will never revisit. This is the highest-severity design
weakness in the search layer.

---

## 2. CrossRef

`search_crossref()` (`R/utils_crossref.R:50`). Called only for abstracts with
zero PubMed candidates **or** ≤ 2 candidates (`R/03b_search_crossref.R:34-41`) —
12 abstracts in the last recorded 2023-only run. Title-based query against
`api.crossref.org/works` with `from-pub-date`/`until-pub-date` from
`config.yml: pubmed.date_start/date_end`, `rows = crossref.max_results` (20),
polite pool via `mailto`. No API key. `Sys.sleep(0.5)` between calls.

CrossRef returns DOIs, not PMIDs, so `03b` resolves each DOI to a PMID through
Europe PMC before it can enter the pool. DOIs that Europe PMC cannot resolve are
lost silently.

---

## 3. Europe PMC

`search_europmc()` (`R/utils_crossref.R:502-559`). Three strategies against
`www.ebi.ac.uk/europepmc/webservices/rest/search`, all filtered by
`PUB_YEAR:[2012 TO 2026]`:

| # | Strategy name | Query |
|---|---|---|
| 1 | `author_title_kw` | `AUTH:<surname> AND TITLE:<kw1> AND TITLE:<kw2> AND TITLE:<kw3> AND TITLE:<kw4> AND PUB_YEAR:[…]` |
| 2 | `title_kw_only` | `TITLE:<kw1..kw5 joined by AND> AND PUB_YEAR:[…]` |
| 3 | `author_broad_kw` | `AUTH:<surname> AND TITLE:<kw1> AND TITLE:<kw2> AND PUB_YEAR:[…]` |

Keywords come from `.title_keywords()`; the surname from `.author_lastname()`.
Results are combined and deduplicated on (`pmid`, `doi`). Fully open API, no
key, `Sys.sleep(0.3)` between abstracts. Checkpoint:
`europmc_search_checkpoint.rds`, every 20 abstracts.

---

## 4. OpenAlex

`search_openalex()` (`R/utils_crossref.R:291-…`). Keyword search against
`api.openalex.org/works` with a `from_publication_date` filter, `per-page` =
`openalex.max_results` (20), polite pool via `mailto` = `PIPELINE_EMAIL` or
`config.yml: contact_email`. OpenAlex exposes `ids.pmid`, which is parsed to a
bare PMID. No key. Checkpoint: `openalex_search_checkpoint.rds`.

---

## 5. Semantic Scholar

`search_semantic_scholar()` (`R/utils_crossref.R:604-…`) against the bulk
endpoint `api.semanticscholar.org/graph/v1/paper/search/bulk`, which has more
generous limits than the relevance endpoint. Two strategies: (1) top 6 title
keywords, (2) surname + top 4 keywords. `year=<start>-<end>` filter. On HTTP 429
it sleeps 5 seconds and retries **once**, then gives up. No key.
Checkpoint: `semantic_scholar_checkpoint.rds`.

---

## 6. DOI-chain reverse-citation search

`R/03c_doi_chain_search.R`. For each abstract, takes the abstract's own JMIG
supplement DOI and asks OpenAlex for works that cite it
(`filter=cites:<openalex work id>`). The premise is that a full paper often
cites the conference abstract that preceded it, or shares its citation
neighbourhood. 407 candidate rows across the cohort. Checkpoint:
`doi_chain_checkpoint.rds`. Merged into the PubMed pool by `03b` on the
following run.

---

## 7. Deduplication and pooling

`R/03b_search_crossref.R:264-464` merges each supplementary source in turn:

1. Keep rows with a non-empty PMID that is not already in the pool.
2. `fetch_pubmed_details()` on the new PMIDs.
3. `inner_join` back to (`pmid`, `abstract_id`) with
   `relationship = "many-to-many"` — one PMID may legitimately be a candidate
   for several abstracts.
4. Tag `strategies = "<source name>"`, `n_strategies = 1`.
5. Apply the inline JMIG supplement filter (see the caveat in §1).
6. `bind_rows()` then `distinct(abstract_id, pmid, .keep_all = TRUE)`.
7. Rewrite `pubmed_candidates.csv` **in place**.

Step 7 is destructive and non-idempotent with respect to a pool built by an
earlier code version. It is the mechanism behind
[FAILURE_MODES.md](FAILURE_MODES.md) F2.

---

## 8. Historical defects in this layer

Four defects were corrected in April 2026, all biasing in the same direction —
each suppressed true matches. Full narrative in
[METHODOLOGICAL_HISTORY.md](METHODOLOGICAL_HISTORY.md); the code-level statement
is:

| Defect | Original behaviour | Why it biased | Correction | Data regenerated? | Test |
|---|---|---|---|---|---|
| Session-number title prefixes | 2013/2017/2018/2021 titles begin `"12 - "`; the prefix entered the `[TI]` phrase | Affected years are a non-random subset, so it confounded the reported time trend | `str_remove(title, "^[0-9]+\\s+[-–]\\s*")` in `R/02_clean_abstracts.R:43` | Yes — the whole pipeline was re-run 2026-04-19 | None directly. `test-utils_text.R` covers normalisation, not the prefix strip. **GAP.** |
| Non-article publication types | Letters, comments, editorials, errata and retractions were eligible candidates | They carry the title and authors of the paper they discuss and can outrank it | `NOT ("Letter"[PT] OR …)` added to `build_date_filter()` | Yes | None. **GAP.** |
| Over-broad JMIG supplement exclusion | Journal + volume + year matched every regular JMIG article in a congress volume | Removed candidates from the single most likely destination journal. The most consequential of the four. | `is_supplement_article()` now also requires `Suppl` in `pub_issue` | Yes | None. **GAP** — and two call sites still use the old rule (§1). |
| Stopword removal broke `[TI]` phrase search | Tokens under 3 characters were dropped from the title phrase | A phrase with holes matches no title, disabling the highest-precision strategy | Strategy 1 now takes a **consecutive** 8-word window starting at the first word of ≥ 3 characters | Yes | None. **GAP.** |

The four shipped in a single re-run. Their individual contributions are **not
separately identifiable** — no ablation was performed.

`output/search_strategy_efficacy.csv` still carries the **pre-correction** run
(1,738 abstract-queries, `title` yield 0.2%) and should not be quoted.
