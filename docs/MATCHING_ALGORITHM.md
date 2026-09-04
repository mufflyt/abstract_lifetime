# Matching Algorithm

Complete specification of the composite match score and the classification
rules. Detailed enough to reimplement independently.

- Scoring: `score_match()`, `R/utils_scoring.R:72-253`
- Selection: `score_abstract_candidates()`, `R/utils_scoring.R:432-494`
- Classification: `classify_match()`, `R/utils_scoring.R:380-390`
- Driver: `R/04_score_matches.R`
- Thresholds: `config.yml`, `scoring:` block

Grain: the score is computed for **every (abstract × candidate) pair**. 64,718
pairs were scored for the 1,106 abstracts (median 44 candidates per abstract,
maximum 866). Only the top-scoring pair per abstract survives into
`match_scores.csv`; all pairs survive in `match_scores_detailed.rds`.

---

## 1. The ten components

| # | Component | Field | Definition | Range | Missing-data behaviour |
|---|---|---|---|---|---|
| 1 | Title similarity | `title_points` | Jaccard similarity of word tokens between the AAGL title and `pub_title`, after `normalize_title()` (lowercase, punctuation stripped, whitespace collapsed). 3 if ≥ `title_jaccard_high` (0.75); 2 if ≥ `title_jaccard_mid` (0.55); 1 if ≥ `title_jaccard_low` (0.35); else 0. | 0–3 | `jaccard_similarity()` returns 0 on `NA`, so the component is 0. |
| 2 | Abstract semantic similarity | `abstract_points` | TF-weighted cosine similarity between `abstract_text` and `pub_abstract` over the union vocabulary, after `normalize_title()`, dropping tokens < 3 characters and a fixed 60-word stoplist. No IDF. 2 if ≥ `abstract_semantic_high` (0.70); 1 if ≥ `abstract_semantic_mid` (0.50); else 0. | 0–2 | Gated on **both** texts being non-`NA` and > 20 characters. 0 whenever either side lacks text — which is every 2017 abstract and, in the current data, every 2017 and 2018 abstract. |
| 3 | First author | `first_author_points` | 2 if `tolower(first_author_normalized) == tolower(normalize_author(pub_first_author))`; else 1 if Jaro-Winkler similarity ≥ `author_fuzzy_threshold` (0.95); else 0. | 0–2 | 0 when either name is `NA`. |
| 4 | Last author | `last_author_points` | Same rule against `pub_last_author`. | 0–2 | 0 when either is `NA` — including the 336 truncated AAGL author lists. |
| 5 | Coauthor overlap | `coauthor_points` | 1 when ≥ 2 names appear in both the semicolon-split `all_authors_str` and `normalize_authors(pub_all_authors)`, case-insensitively. | 0–1 | 0 when either list is empty. |
| 6 | Author-team bonus | `author_team_bonus` | 1 when `coauthor_points == 1` **and** `first_author_points ≥ 1`. Effectively upgrades a first-author match backed by a matching team. | 0–1 | 0. |
| 7 | Journal relevance | `journal_points` | `max` Jaro-Winkler similarity between `tolower(pub_journal)` and each of 12 hard-coded OB/GYN journal names (`R/utils_scoring.R:178-186`), rounded to 2 dp. | 0–1, continuous | 0 when `pub_journal` is `NA`. |
| 8 | Keyword overlap | `keyword_points` | 1 when ≥ 3 elements of the abstract's TF keyword vector intersect the semicolon-split `pub_keywords`. | 0–1 | 0 when either is `NA`. |
| 9 | Publication timing | `date_points` | `months_diff = (pub_date − congress_date)/30.44`, where `pub_date` is assembled from `pub_year`/`pub_month`/`pub_day` with missing month → January and missing day → the 1st. If `months_diff < 0`: `pre_conference_penalty` = **−3**. Else if ≤ `pub_date_early_months` (18): **+1**. Else if ≤ `pub_date_late_months` (30): **+0.5**. Else 0. | −3 to +1 | 0 when the date cannot be assembled. |
| 10 | No-text-evidence penalty | `no_text_penalty` | **−2** when `title_points == 0` **and** `title_sim < 0.20` **and** `abstract_points == 0`. Prevents a coincidental surname from producing a match. | −2 or 0 | Applied whenever the text evidence is absent, including when it is absent because the abstract has no text. |

`total = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10`.

**Theoretical range** −5 (pre-conference, no text, nothing else) to **+14**.
**Observed range in the current data**: −1.29 to 13.00.

### Component 8 is inert — VERIFIED

`keyword_pts` is `0` for all 1,102 scored abstracts in
`output/final_analytical_dataset.csv`. The abstract side holds TF-selected
single tokens from `extract_keywords()`; the PubMed side holds author keywords
and MeSH-style multi-word phrases. The two vocabularies effectively never
intersect at the required threshold of 3. The score is functionally
nine-component. This is a defect of no consequence for the current results
(adding at most 1 point, and only where 3 keywords already agreed), but the
"10-component score" description in the README and manuscript overstates what
runs.

### Component 7 gives every candidate free points

`journal_points` is a *similarity*, not an indicator. Jaro-Winkler between an
unrelated journal name and the nearest OB/GYN journal is routinely 0.4–0.6, so
essentially every candidate receives roughly half a point regardless of
relevance. This inflates all scores approximately uniformly and therefore shifts
the effective meaning of the `auto_accept = 7` and `manual_review = 3`
thresholds. It does not reorder candidates much, but it is not a discriminating
feature.

---

## 2. Selecting the best candidate and breaking ties

`score_abstract_candidates()`:

1. Score every candidate.
2. `arrange(desc(total_score))`. `dplyr::arrange` is a stable sort, so **ties
   are broken by the candidate's position in `pubmed_candidates.csv`**, which is
   the order the search strategies happened to return. This is arbitrary but
   deterministic given a fixed candidate pool.
3. `best <- all_scores[1, ]`.
4. `has_tie <- nrow(all_scores) > 1 && total_score[1] == total_score[2]` — only
   the top **two** rows are compared, so a three-way tie is recorded the same as
   a two-way tie.
5. `has_text_evidence <- best$title_pts >= 1 || best$abstract_pts >= 1`.
6. `pre_conference <- !is.na(best$date_pts) && best$date_pts < 0`.
7. Classify (below).
8. If `has_tie` and the class is `definite`, demote to `probable` so a human
   chooses.

Current tie counts: 75 abstracts have `has_tie == TRUE` (62 `no_match`, 10
`possible`, 3 `probable`, 0 `definite`).

---

## 3. Classification

`classify_match(score, cfg, has_text_evidence, pre_conference)`, evaluated in
this order:

```
1. pre_conference                                   -> "excluded"
2. score >= auto_accept (7)  AND has_text_evidence  -> "definite"
3. score >= manual_review (3) AND has_text_evidence -> "probable"
4. score >= manual_review (3)                       -> "possible"
5. otherwise                                        -> "no_match"
```

`no_candidates` is assigned upstream in `R/04_score_matches.R:41-51` when the
abstract has zero candidates and never reaches `classify_match()`.

`auto_reject_below: 3` in `config.yml` is **not read by any code**; the effective
reject boundary is `< manual_review`. It is a vestigial key.

| Class | Definition | n | Human review required |
|---|---|---:|---|
| `definite` | ≥ 7 with text evidence, no tie, post-congress | 131 | No — auto-accepted |
| `probable` | 3 ≤ score < 7 with text evidence, or a demoted tie | 81 | Yes |
| `possible` | ≥ 3 without text evidence | 142 | Yes |
| `no_match` | < 3 | 709 | No — auto-rejected |
| `excluded` | best candidate predates the congress | 39 | No — counted unpublished |
| `no_candidates` | no candidate retrieved at all | 4 | No |

Boundary behaviour is inclusive at the cutoff and exclusive below; this is
pinned by `tests/testthat/test-cycle01_thresholds_contracts.R`.

---

## 4. Pre-conference candidates

A pre-conference best candidate produces two effects, and they are independent:

1. `date_points = −3`, which lowers `total`.
2. `classify_match()` short-circuits to `"excluded"` **before** any score tier is
   consulted, so a pre-conference candidate scoring 13 is still `excluded`.

`"excluded"` is a statement about the **candidate**, not the abstract. The
abstract stays in the cohort and is counted unpublished
(`R/05_adjudicate.R:58-88`). A reviewer can still override it: 4 of the 39
carry `manual_decision == "match"` and are counted as published.

The technical appendix (A13.6) audits these 39 and finds 10 misclassified: six
matched the AAGL supplement itself (2015, JMIG 22(6S)), two were published
*after* the congress but carry a coarse PubMed date that the scorer resolves to
January 1, and two are scoring false positives. That audit has **not** been
acted on.

---

## 5. Which publication date is used

**The date used is the first day of the PubMed `JournalIssue/PubDate` month.**

`parse_pubmed_xml()` (`R/utils_pubmed.R:205-207`) reads
`.//JournalIssue/PubDate/Year`, `/Month` and `/Day`, defaulting month and day to
`"01"` when absent. `score_match()` and `R/05_adjudicate.R:34-45` then assemble
`as.Date(sprintf("%s-%02d-%s", year, month, day))`.

This is the **print/issue date**, not:

- the electronic publication date (`ArticleDate`, which PubMed exposes and the
  pipeline does not read),
- the PubMed entry date,
- the earliest of the available dates.

Consequences:

- An online-ahead-of-print paper is dated to its later print issue, so
  `months_to_pub` is systematically **too long** for journals with a long
  online-to-print lag.
- A record with a year-only `PubDate` is dated 1 January of that year. Eleven of
  the 39 `excluded` abstracts have year-only dates, and at least two of them are
  genuine post-congress publications misdated into the pre-congress window
  (appendix A13.6).

Preferring `ArticleDate` when present would fix both. It has not been changed.

---

## 6. What happens after the best PMID is chosen

`R/05_adjudicate.R` joins the winning PMID's publication fields back from
`pubmed_candidates.csv`, then blanks `pub_title`, `pub_journal`, `pub_year`,
`pub_doi`, `pub_first_author` and `months_to_pub` for
`no_match`/`no_candidates`/`possible` rows, on the grounds that those fields
describe a candidate the pipeline is rejecting.

**283 of the 1,102 winning PMIDs are absent from the candidate file on disk**,
so their publication fields come back `NA` even when the abstract is classified
`definite`. Only 137 of 1,106 rows carry publication metadata, and only 104 of
the 178 published abstracts have a `months_to_pub`. This is a file-staleness
defect, not a scoring one — see [FAILURE_MODES.md](FAILURE_MODES.md) F2.

---

## 7. Reimplementation checklist

To reproduce `best_score` for one (abstract, candidate) pair you need:

- from the abstract: `title` (session prefix stripped), `abstract_text`,
  `first_author_normalized`, `last_author_normalized`, `all_authors_str`,
  `keywords`, `congress_year`;
- from the candidate: `pub_title`, `pub_abstract`, `pub_first_author`,
  `pub_last_author`, `pub_all_authors`, `pub_journal`, `pub_keywords`,
  `pub_year`, `pub_month`, `pub_day`;
- from config: the eight `scoring:` thresholds and the congress date table;
- the three helper definitions: `normalize_title()`, `normalize_author()`
  (`R/utils_text.R`) and the 12-journal list plus the 60-word stoplist
  (`R/utils_scoring.R`).

`stringdist::stringdist(method = "jw")` is used for both author and journal
similarity; the score is `1 − distance`.
