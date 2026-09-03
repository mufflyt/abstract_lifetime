# Methodological History

Chronological record of the corrections that changed the reported numbers, so
that the Methods documentation does not have to carry a changelog.

Sources: git history (`git log --date=short`), `CHANGELOG.md`, `NEWS.md`,
technical appendix sections A12 and A13, and the diffs themselves. Where a
quantitative impact was never measured, this document says so rather than
estimating one.

---

## Timeline

| Date | Commit | Event |
|---|---|---|
| 2026-04-13 | `3659076` | Initial pipeline, AAGL 2023 only |
| 2026-04-14 | `03c0ab5` | "Fully scraped" 2023 supplement committed — **686 rows** |
| 2026-04-14 | `9ea4d62` | Pagination-dedup fix: 686 → **98** |
| 2026-04-17 | `a39fe0c`, `387207d` | Expanded to 11 then 12 congresses (2012–2023) |
| 2026-04-18 | `d9383c1` | Pre-conference publications excluded; genderize crash fixed |
| 2026-04-18–19 | many | Demographics build-out: NPI matching, ORCID, five gender sources |
| 2026-04-19 | `ddd876b` | Gender resolution formalised as an explicit 10-tier policy object |
| **2026-04-28** | *uncommitted at the time* | **The four matching corrections** (A12.1–A12.4). Worked on the external drive; not committed for four months. |
| 2026-09-01 | `4e7da03` | April 28 work recovered from `/Volumes/MufflySamsung` and committed |
| 2026-09-01 | `69120a4`, `33a5ce3`, `961e244` | Corrections documented; the denominator defect logged as a known issue |
| 2026-09-01 | `1781d59` (#2) | **Denominator defect fixed** — pre-conference exclusions retained |
| 2026-09-01 | `e288259` | Steps 05–08 re-run with demographics restored |
| 2026-09-03 | `fb71bbb` | Denominator chain documented (A13); STROBE flowchart added |
| 2026-09-03 | `49e6c95` | Decision logic extracted to `R/utils_decisions.R`; BVA + mutation tests; CI gates |
| 2026-09-03 | `665c551` | Threshold, contract and vintage tests |

---

## 1. April 2026 — the pagination "fix" that created the truncation

**Problem as understood at the time.** The 2023 scrape produced 686 rows, of
which 588 were duplicates: the old loop requested `?offset=0,100,…,600` and
ScienceDirect returned the *same* first page every time, so the 98 real items
were parsed seven times.

**Correction** (`9ea4d62`, `R/01b_parse_web.R`). Attempt pagination only when
the first page returns exactly 100 items; compare the first title of each new
page against the first title already held and break when they match; then
`distinct(doi)` and `distinct(title)`.

**Effect.** 686 → 98 for 2023. The duplicate problem was real and the fix was
correct.

**What was missed.** The conclusion drawn — "offset returns same content — no
true pagination" — is true of the *offset parameter* but was generalised to "the
listing has no further pages". It does. Crossref shows 392 supplement items for
Nov 2023 and 453–852 for the earlier years, against the 93–100 captured. See
[COHORT_ASSEMBLY.md](COHORT_ASSEMBLY.md) §5 and
[FAILURE_MODES.md](FAILURE_MODES.md) F1.

- **Direction of bias**: unknown. The cohort is the leading ~100 items of each
  supplement in page order. Whether ordering within the oral block correlates
  with publication propensity has not been tested.
- **Effect on numerator / denominator / rate**: **not separately identifiable.**
  The missing abstracts were never searched, so nothing can be said about their
  publication status without re-ingesting them.

---

## 2. April 28, 2026 — four matching corrections (A12.1–A12.4)

All four suppressed true abstract-to-publication matches, so all four biased the
publication rate **downward**. Committed on 2026-09-01 as `4e7da03` after
recovery from the external drive.

### A12.1 Session-number title prefixes

- **Problem.** The 2013, 2017, 2018 and 2021 congress programmes prefix titles
  with a session number (`"12 - Laparoscopic …"`). The prefix entered the PubMed
  `[TI]` phrase and matched nothing.
- **Affected files.** `R/02_clean_abstracts.R:43`, `R/utils_text.R`.
- **Direction.** Downward, and **differentially by year** — four congresses out
  of twelve, so it confounded the reported time trend.
- **Correction.** `str_remove(title, "^[0-9]+\\s+[-–]\\s*")`.
- **Searches re-run?** Yes, in the 2026-04-19 full run.
- **Human review repeated?** No. Decisions recorded before the correction stand.
- **Effect on the numerator / rate.** Not separately identifiable.

### A12.2 Non-article publication types in the candidate pool

- **Problem.** Letters, comments, editorials, errata and retractions were
  eligible candidates. They carry the title and authors of the paper they
  discuss, so they can outrank the genuine publication.
- **Affected file.** `build_date_filter()`, `R/utils_pubmed.R:320-329`.
- **Direction.** Downward.
- **Correction.** `NOT ("Letter"[PT] OR "Comment"[PT] OR "Editorial"[PT] OR
  "Published Erratum"[PT] OR "Retraction of Publication"[PT])`.
- **Note.** The filter applies to PubMed only. `pub_type_canonical` still
  contains an `Editorial/Letter` level because the four supplementary sources do
  not apply it.
- **Effect.** Not separately identifiable.

### A12.3 Over-broad JMIG supplement exclusion — the most consequential

- **Problem.** `is_supplement_article()` matched on journal + volume + year,
  which excluded **every regular JMIG article sharing a volume with a congress
  supplement** — precisely the journal where AAGL abstracts are most likely to
  publish.
- **Affected file.** `R/utils_pubmed.R:359-379`; `pub_issue` added to
  `parse_pubmed_xml()` to support the fix.
- **Direction.** Downward, and concentrated on the single most likely
  destination journal.
- **Correction.** The rule now also requires `Suppl` in `pub_issue`, or — when
  PubMed omits the issue field — a November publication month.
- **Residual.** Two call sites were **not** updated and still use the old rule:
  the inline filter in `R/03b_search_crossref.R` and `search_europmc()` in
  `R/utils_crossref.R:553-556`, the latter dropping *all* JMIG records in range.
  See [PUBLICATION_SEARCH.md](PUBLICATION_SEARCH.md) §1.
- **Effect.** Not separately identifiable.

### A12.4 Stopword removal destroyed the title phrase search

- **Problem.** Tokens shorter than three characters were dropped before the
  `[TI]` phrase was built. Dropping words from a phrase does not shorten it — it
  produces a word sequence that appears in no title, disabling the
  highest-precision strategy entirely.
- **Affected file.** `build_search_strategies()`, `R/utils_pubmed.R:441-459`.
- **Direction.** Downward.
- **Correction.** Strategy 1 now takes a **consecutive** 8-word window starting
  at the first word of ≥ 3 characters, preserving stopwords.
- **Evidence of the original severity.** `output/search_strategy_efficacy.csv`,
  the pre-correction measurement, records the `title` strategy returning hits
  for **0.2%** of abstract-queries (3 of 1,742).
- **Effect.** Not separately identifiable.

### Combined

All four shipped in one re-run on 2026-04-19. **No ablation was performed**, so
their individual contributions are not separately identifiable. The combined
before/after is also not cleanly measurable, because the cohort changed from 11
to 12 congresses in the same period.

Testing status: **none of the four corrections has a direct regression test.**
Four documented GAPs in [VALIDATION.md](VALIDATION.md) §2.

---

## 3. September 1, 2026 — the denominator defect (issue #2)

- **Problem.** `R/05_adjudicate.R` contained
  `results <- results |> filter(classification != "excluded")`.
  `classification == "excluded"` means the best-scoring *candidate* predates the
  congress. That is a fact about the candidate, not about the abstract's
  eligibility. Filtering removed 39 abstracts from the cohort entirely.
- **Direction of bias.** **Upward.** The removed abstracts are overwhelmingly
  non-events, so removing them shrank the denominator more than the numerator.
- **Affected files.** `R/05_adjudicate.R`; everything downstream of
  `output/abstracts_with_matches.csv`.
- **Correction** (`1781d59`). The `filter()` was removed and replaced with the
  explanatory comment now at `R/05_adjudicate.R:58-88`. No downstream change was
  needed: `06`, `07`, `08` and the Shiny app already map `excluded` to
  `published = FALSE`.
- **Searches re-run?** No — the search results were unaffected.
- **Human review repeated?** No, and it did not need to be: 4 of the 39 already
  carried a reviewer `manual_decision == "match"` in the decision log, which is
  how they entered the numerator on restoration.

**Quantified effect** — this is the one correction whose impact *is* separately
identifiable:

| | before | after |
|---|---:|---:|
| Cohort | 1,067 | **1,106** |
| Evaluated (denominator) | ~1,012 | **1,051** |
| Published (numerator) | 174 | **178** |
| Publication rate | 17.2% | **16.9%** |

Of the 39 restored abstracts, **35 are unpublished and 4 carry a reviewer's
confirmed match**. The filter was therefore discarding confirmed publications
from the numerator as well as non-events from the denominator, which is why the
rate moves by 0.3 points rather than the ≈0.6 the denominator change alone would
predict.

**Detection history.** The defect was masked by
`test-pipeline_semantics.R`'s abstract-ID test, which had been weakened from set
equality to a one-directional subset check. Restoring `expect_setequal()` was
part of the fix.

---

## 4. September 2026 — adjudication and denominator hardening

No numbers changed. Three structural corrections:

1. **`R/utils_decisions.R` extracted** (`49e6c95`). The dedup and outcome
   cascade moved out of `06_analyze_results.R` into two pure functions, and the
   **human-outranks-AUTO precedence rule was made explicit and
   timestamp-independent.** Previously the dedup kept the latest timestamp
   regardless of reviewer population; it happened to give the right answer
   because the AUTO prefill preceded human review. Verified: the old and new
   logic agree on all 1,106 rows.
2. **`n_evaluated` added to `aim1_publication_rate.csv`.** The file reported
   `total_abstracts` = 1,106 alongside `publication_rate` = 16.9 with no row
   stating the 1,051 denominator, so a reader dividing 178 by the stated total
   obtained 16.1%. `test-decision_precedence_bva.R:223` now asserts
   reconstructibility.
3. **Mutation testing wired into CI** (`test-decision_mutation.R`). Ten planted
   defects in the decision logic must each be killed by a named invariant, so
   the build fails when the suite *stops detecting* a defect rather than only
   when a defect appears.

Also added: `R/strobe_flowchart.R`, which derives its counts from the pipeline
files and asserts the arithmetic with `stopifnot()` before drawing, so the
cohort figure cannot drift from the data the way the prose numbers did.

---

## 5. Numbers that should no longer be quoted

| Value | What it was | Superseded by |
|---|---|---|
| 686 abstracts | the 2023 scrape before the duplicate fix (7 × 98) | 98 for 2023 |
| 1,070 abstracts | the initial 12-congress cohort, 2026-04-17 | 1,106 |
| 1,067 abstracts | the cohort with the denominator defect live | **1,106** |
| 1,051 abstracts | *still current* — but it is the **denominator**, not the cohort | — |
| 174 publications | numerator with the denominator defect live | **178** |
| 17.2% | rate with the denominator defect live | **16.9%** |
| 16.6% | the rate computed during the issue-#2 investigation with the 39 restored but before the 4 reviewer matches were credited | **16.9%** |
| 16.1% | 178/1,106 — the rate against the cohort rather than the denominator | 16.9% against 1,051 |
| 55 "skipped" records | correct as the count of **unresolved** abstracts; **212** abstracts carry a reviewer `skip`, of which 157 are resolved by the cascade | both, distinguished |
| 277 gender conflicts | pre-2026-09-01 value | **228** |
| 1,738 abstracts searched | `search_strategy_efficacy.csv`, contaminated by the superseded 2023 ID scheme | not currently measurable |

---

## 6. Open methodological decisions

None of these is a defect; each is a choice that has been made implicitly and
should be stated explicitly in the manuscript.

1. **The 55 unresolved.** Removed from the denominator (available-case).
   Bounds: 16.1% if all unpublished, 21.1% if all published.
2. **Branch order in `assign_final_published()`.** `definite` beats every
   reviewer decision, so 4 reviewer `no_match` and 44 reviewer `skip` decisions
   on `definite` abstracts are recorded as published. Reordering moves 48
   abstracts.
3. **The pre-congress boundary.** Currently the congress start date. Appendix
   A13.6 argues for the abstract submission deadline (roughly six months
   earlier), which is not recorded in `config.yml` and has not been verified.
4. **Which publication date.** Currently the print/issue date. `ArticleDate`
   would be earlier and would resolve the year-only misdating in
   [FAILURE_MODES.md](FAILURE_MODES.md) F14.
5. **The A13.6 audit of the 39 `excluded` abstracts** found 10 misclassified
   (6 self-matches to the AAGL supplement, 2 genuinely post-congress, 2 scoring
   false positives). This has not been acted on.
