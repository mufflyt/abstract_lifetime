# Cohort Assembly

**What is the denominator of this study, and what happened to every abstract
that was parsed from the congress programs?**

Every count in this document was recomputed from the files in the working tree
on 2026-09-03 at commit `665c551`. Nothing is copied from prose.

---

## 1. The short answer

| Quantity | n | Where it comes from |
|---|---:|---|
| Presentations parsed from the JMIG AAGL supplements, 2012–2023 | 1,154 | `data/processed/abstracts_parsed_web.csv` |
| Video presentations excluded | 48 | `session_type == "Video"` |
| **Eligible oral presentations (the COHORT)** | **1,106** | `data/processed/abstracts_cleaned.csv` |
| Unresolved adjudication (removed from the rate) | 55 | `is.na(final_published)` |
| **Evaluated for publication (the DENOMINATOR)** | **1,051** | `output/final_analytical_dataset.csv` |
| Published | 178 | `sum(final_published)` |
| Not published | 873 | 1,051 − 178 |

Publication rate **178 / 1,051 = 16.9%** (95% CI 14.8–19.3, Wilson/`prop.test`
without continuity correction).

The **cohort** (1,106) and the **denominator** (1,051) are different numbers and
must not be interchanged. `output/aim1_publication_rate.csv` reports both, but
labels the cohort `total_abstracts` and never labels 1,051 — see
[RESULTS_PROVENANCE.md](RESULTS_PROVENANCE.md).

---

## 2. Per-congress reconciliation

VERIFIED — recomputed by `scripts`-free R over the current files.

| Year | Parsed | Oral | Video excluded | Other excluded | Search cohort | Final cohort | Unresolved | Evaluated | Published | Rate |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2012 | 95 | 95 | 0 | 0 | 95 | 95 | 1 | 94 | 13 | 13.8% |
| 2013 | 96 | 96 | 0 | 0 | 96 | 96 | 6 | 90 | 8 | 8.9% |
| 2014 | 93 | 93 | 0 | 0 | 93 | 93 | 3 | 90 | 10 | 11.1% |
| 2015 | 95 | 95 | 0 | 0 | 95 | 95 | 3 | 92 | 17 | 18.5% |
| 2016 | 96 | 96 | 0 | 0 | 96 | 96 | 4 | 92 | 11 | 12.0% |
| 2017 | 97 | 97 | 0 | 0 | 97 | 97 | 7 | 90 | 5 | 5.6% |
| 2018 | 95 | 95 | 0 | 0 | 95 | 95 | 0 | 95 | 26 | 27.4% |
| 2019 | 96 | 96 | 0 | 0 | 96 | 96 | 9 | 87 | 12 | 13.8% |
| 2020 | 95 | 95 | 0 | 0 | 95 | 95 | 10 | 85 | 14 | 16.5% |
| 2021 | 98 | 98 | 0 | 0 | 98 | 98 | 12 | 86 | 14 | 16.3% |
| 2022 | 100 | 90 | 10 | 0 | 90 | 90 | 0 | 90 | 28 | 31.1% |
| 2023 | 98 | 60 | 38 | 0 | 60 | 60 | 0 | 60 | 20 | 33.3% |
| **ALL** | **1,154** | **1,106** | **48** | **0** | **1,106** | **1,106** | **55** | **1,051** | **178** | **16.9%** |

Every transition closes exactly:

```
1,154 parsed − 48 video − 0 other exclusions + 0 restorations = 1,106 cohort
1,106 cohort − 55 unresolved                                  = 1,051 evaluated
1,051 evaluated = 178 published + 873 not published
```

There is **no unexplained row loss between parsing and the analytical dataset.**
`data/processed/abstracts_cleaned.csv`, `output/abstracts_with_matches.csv`,
`data/processed/match_scores.csv` and `output/final_analytical_dataset.csv` all
carry exactly the same 1,106 `abstract_id` values, each appearing once.

---

## 3. The accounting identity

```
ALL PARSED AAGL PRESENTATIONS                       1,154
  − non-oral presentations (none other than video)      0
  − videos                                             48
  − duplicates (removed inside the parser, see §5)      0
  − explicit exclusions                                 0
  ± documented parsing corrections                      0
= ELIGIBLE ORAL PRESENTATIONS                       1,106

ELIGIBLE ORAL PRESENTATIONS                         1,106
  = confirmed publication                             178
  + confirmed unpublished                             873
  + unresolved                                         55
  + explicitly excluded analytical category              0
```

Note that `classification == "excluded"` (39 abstracts) is **not** an excluded
analytical category. It is a statement about the *candidate* — the best-scoring
candidate publication predates the congress — and those abstracts are retained
in the cohort and counted as unpublished. See
[METHODOLOGICAL_HISTORY.md](METHODOLOGICAL_HISTORY.md) §3.

---

## 4. Where each reduction happens in code

| Reduction | n | Code |
|---|---:|---|
| Non-abstract front matter dropped from the listing | see §5 | `R/01b_parse_web.R:379-385` |
| Duplicate DOI / duplicate title collapsed | see §5 | `R/01b_parse_web.R:384-385` |
| Video presentations dropped | 48 | `R/02_clean_abstracts.R:31-36` |
| Unresolved adjudication removed from the rate | 55 | `R/06_analyze_results.R:56` (`n_evaluated <- n_total - n_pending`) |

`R/strobe_flowchart.R` renders this chain and asserts the arithmetic with
`stopifnot()` before drawing, so the diagram cannot drift from the data.

---

## 5. What is *not* in the 1,154 — a data-integrity defect

**VERIFIED. This is the most important limitation of the cohort.**

`R/01b_parse_web.R` fetches one ScienceDirect issue-listing page per congress
and parses the `li.js-article-list-item` elements it returns. Offset pagination
is attempted only when the first page returns exactly 100 items
(`R/01b_parse_web.R:353`), and when it is attempted, ScienceDirect returns the
same first page again, so the loop breaks (`R/01b_parse_web.R:361`). The
practical effect is a hard ceiling of roughly 100 items per congress.

Crossref holds the complete deposit for each supplement. Querying
`api.crossref.org/journals/1553-4650/works` for each November issue and counting
records with supplement (`S`-prefixed) pagination gives:

| Year | Items captured | Page span captured | Supplement items in Crossref | Supplement page span | Captured share |
|---:|---:|---|---:|---|---:|
| 2012 | 95 | S1–S33 | 663 | S1–S189 | 14% |
| 2013 | 96 | S1–S31 | 698 | S1–S195 | 14% |
| 2014 | 93 | S1–S26 | 762 | S1–S230 | 12% |
| 2015 | 95 | S1–S38 | 852 | S1–S254 | 11% |
| 2016 | 96 | S1–S40 | 793 | S1–S253 | 12% |
| 2017 | 97 | S1–S59 | 614 | S1–S202 | 16% |
| 2018 | 95 | S1–S60 | 745 | S1–S286 | 13% |
| 2019 | 96 | S1–S30 | 744 | S1–S232 | 13% |
| 2020 | 95 | S1–S33 | 453 | S1–S147 | 21% |
| 2021 | 98 | S1–S34 | 498 | S1–S161 | 20% |
| 2022 | 100 | S1–S40 | 497 | S1–S162 | 20% |
| 2023 | 98 | S1–S38 | 392 | S1–S136 | 25% |

Every captured DOI is present in the Crossref set, and within the captured page
span the capture is essentially complete (the small residual is front matter and
items straddling a page boundary). The loss is entirely at the tail: the
listing simply stops.

**Does the loss touch oral presentations?** The supplements are ordered by
programme section. In 2022 and 2023 the fetched window crosses the Oral → Video
boundary (2022: items 1–90 Oral, 91–100 Video; 2023: items 1–60 Oral, 61–98
Video), so the oral block for those two congresses is complete. In **2012–2021
every captured item is tagged Oral and the window ends before any section
change**, which means the oral block was still running when the listing stopped.
Reading the Crossref titles immediately past the cutoff confirms that at least
some are oral-format research abstracts, not videos or posters:

- 2015, page S39–S41: *"Vasopressin Administration During Laparoscopic
  Myomectomy: A Randomized Controlled Trial"*, *"Incidence and Prevention of
  Vaginal Cuff Dehiscence Following Laparoscopic and Robotic Hysterectomy"*,
  *"The SONATA Study: Sonography-Guided Transcervical Ablation of Uterine
  Fibroids"*.
- 2018, page S61–S62: *"12-Month Primary Clinical Endpoints and Safety Analysis
  of the SONATA Pivotal IDE Trial"*.

By contrast the items past the 2021 (S35+) and 2023 (S39+) cutoffs read as
video/surgical-tutorial titles.

**Status.** The *existence* of the truncation is VERIFIED. Its *magnitude in
oral presentations* is UNRESOLVED: the section headings for the untruncated
listing cannot currently be read, because ScienceDirect returns HTTP 403 to this
machine (verified 2026-09-03) and Crossref does not deposit programme-section
metadata for these records. An INFERRED bound, taking the 2022–2023 oral share
of all supplement items (18% and 15%) as a guide, puts the true oral count for
2012–2021 somewhere between the captured 93–98 and roughly 90–150 per congress.

**Consequence for the study.** The cohort is best described as *"the first
~95–100 presentations listed in each JMIG AAGL congress supplement, of which the
oral ones were retained"* rather than *"all oral presentations at the AAGL
Global Congress"*. Because the truncation point is a fixed listing position
rather than a scientific criterion, whether it induces bias depends on whether
supplement ordering within the oral block correlates with publication
propensity, which has not been tested.

This has **not** been fixed as part of this documentation pass. See
[FAILURE_MODES.md](FAILURE_MODES.md) F1 for the detection and remediation path.

---

## 6. Duplicate handling inside the parser

`R/01b_parse_web.R:378-389` applies, in order:

1. `str_detect(tolower(subtype), "abstract|conference") | is.na(subtype)` —
   keeps only items ScienceDirect types as a conference abstract. Every one of
   the 1,154 retained rows carries `subtype == "Conference abstract"`.
2. `nchar(title) > 10`.
3. A title blocklist: `^toc$|^cover|^board|^editorial|^international
   societies|^officers|^committees`.
4. `distinct(doi, .keep_all = TRUE)` then `distinct(title, .keep_all = TRUE)`.

The number of items removed at each of these steps is **not recorded** — the
parser logs only the post-filter count, and the run predates the current working
tree. Comparing the 93–100 retained rows against the 100-item listing ceiling
implies 0–7 removals per congress, which is consistent with front matter, but
this is INFERRED, not logged. Adding per-step counts to the parser is the
cheapest way to close this gap.

`abstract_id` is assigned **after** filtering as
`sprintf("AAGL%d_%03d", year, row_number())`, so the numeric suffix is a
position in the filtered listing and carries no AAGL meaning. Re-running the
parser against a differently filtered listing would reassign every ID and break
the join to `output/manual_review_decisions.csv`.

---

## 7. Video identification

`R/01d_tag_session_type.R` walks the supplement table of contents in DOM order,
pairing every `li.js-article-list-item` with the nearest preceding
`h3.section-title`, and collapses the heading text to `Oral` / `Video` /
`Poster` by substring match (`R/01d_tag_session_type.R:72-80`). The result is
joined back onto the pipeline CSVs by ScienceDirect PII.

`R/02_clean_abstracts.R:31-36` then imputes `NA` session types to `Oral` before
filtering out `Video`. In the current data no row is `NA`, so the imputation is
inert, but it would silently admit an untagged video if the TOC scrape were to
fail for a congress. No `Poster` row exists in the parsed data — consistent with
§5, the listing never reaches the poster sections.

---

## 8. The 55 unresolved abstracts

Every unresolved abstract is the same combination: the algorithm returned
`probable` or `possible`, and the reviewer recorded `skip`.

| Algorithm classification | Reviewer decision | n |
|---|---|---:|
| probable | skip | 31 |
| possible | skip | 24 |

They are spread across nine congress years (2012–2021; none in 2018, 2022 or
2023). They are not unreviewable records — they require adjudication, not a
methodological ruling. Treated as unpublished they would give 178/1,106 =
16.1%; treated as published, 233/1,106 = 21.1%. The reported figure removes
them from the denominator, which is the standard available-case treatment and
assumes they are missing at random with respect to publication status. That
assumption has not been tested.

See [ADJUDICATION.md](ADJUDICATION.md) for the full decision accounting.
