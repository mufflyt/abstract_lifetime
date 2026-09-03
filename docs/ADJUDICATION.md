# Human Adjudication

How a human decision is produced, stored, deduplicated and turned into the
primary outcome.

---

## 1. What requires human review

`R/05_adjudicate.R:120-132` writes `output/manual_review_queue.csv` containing
every abstract where

```r
classification %in% c("probable", "possible") | has_tie
```

That is **285 abstracts** (81 probable + 142 possible + 62 `no_match` rows that
carry a tie). VERIFIED: recomputing the filter against
`data/processed/match_scores.csv` reproduces 285 exactly, so the queue file is
current.

Accepted without human review: `definite` (131) and `no_match` /
`no_candidates` / `excluded` (752). The Shiny app nevertheless presents all
1,106 abstracts so reviewers can spot-check auto-accepts and auto-rejects
(`shiny/adjudication_app/app.R:191-197`), and in practice they did.

---

## 2. The decision vocabulary

Reviewers choose exactly one of three values
(`shiny/adjudication_app/app.R:668-671`):

| Stored value | UI label | Meaning |
|---|---|---|
| `match` | "Confirmed match" | This publication is the full-text version of this abstract. Requires a PMID (`app.R:1628`). |
| `no_match` | "No match found" | No candidate shown is the publication of this abstract. |
| `skip` | "Skip / Unsure" | Reviewer declined to rule. The default selection. |

There is **no** `excluded`, `definite`, `probable` or `possible` in the reviewer
vocabulary. Those words are algorithm classifications and appear in the app only
as filters (`app.R:525`). Historical labels: none — the vocabulary has been
`match`/`no_match`/`skip` since the app was written.

The `skip` default matters: a reviewer who advances without choosing records
`skip`, which is indistinguishable from a deliberate "unsure".

---

## 3. The Google Sheets backend

Sheet ID `1d2YAsndMxCPK0AQMHw4bgKp3glFg_znGj8Riq5YpfaI`
(`config.yml: google_sheet_id`, overridable by `GOOGLE_SHEETS_ID`). Worksheet
name `decisions`. Service-account credentials at
`shiny/adjudication_app/google_credentials.json` (gitignored).

Schema — `DECISION_COLS`, `shiny/adjudication_app/app.R:78-89`. Order is
canonical; new columns are appended on the right so older rows stay valid:

```
abstract_id, reviewer, manual_decision, manual_pmid, reviewer_notes,
review_timestamp, abstract_title, abstract_first_author, abstract_subtype,
session_type, congress_year, sciencedirect_url, matched_pub_title,
matched_pub_journal, matched_pub_year, matched_score,
matched_title_similarity, matched_pub_type, matched_pub_types_raw,
n_authors, n_unique_affiliations, first_author_state,
first_author_acog_district, first_author_gender
```

The exported CSV, `output/manual_review_decisions.csv`, has **56** columns —
`gs_append_decision()` reorders to the sheet's *actual* header
(`app.R:153-166`), and `scripts/backfill_*.R` have added further context columns
over time. The four fields that matter downstream are `abstract_id`,
`reviewer`, `manual_decision` and `review_timestamp`, plus `manual_pmid` for
confirmed matches.

Writes go to the sheet with `sheet_append()`. `gs_dedup_sheet()`
(`app.R:177-188`) can clear `A2:Z` and rewrite the deduplicated set.
`gs_read_decisions()` normalises `reviewer` to upper case and maps a missing
reviewer to the literal `"AUTO"`.

`app.R:1760` also writes `output/manual_review_decisions.csv` locally. On
shinyapps.io that write lands in an ephemeral container, so **the Google Sheet
is the live store and the repository CSV is a manual export.** There is no
scripted sync; the CSV in the tree is a snapshot taken on 2026-09-01.

---

## 4. Reviewers, blinding, and the AUTO pass

`output/manual_review_decisions.csv` holds **2,372** rows.

| Reviewer | match | no_match | skip | total |
|---|---:|---:|---:|---:|
| AUTO | 78 | 828 | 203 | 1,109 |
| GW | 135 | 330 | 179 | 644 |
| JM | 104 | 275 | 151 | 530 |
| TMM | 28 | 58 | 3 | 89 |
| **All** | **345** | **1,491** | **536** | **2,372** |

`AUTO` is not a person. `scripts/prefill_algorithm_decisions.R` writes the
algorithm's own classification into the sheet as a reviewer row, mapping
`definite → match`, `no_match`/`no_candidates`/`excluded` → `no_match`, and
everything else → `skip` (`app.R:1058-1065` implements the same map for the
UI's pre-selected radio button).

**Blinding.** Review is *not* blinded to the algorithm: the app shows the
classification tier, the composite score and every score component
(`CHIP_TIPS`), and pre-selects the radio button to the algorithm's answer. It
*is* side-by-side — abstract text against candidate title, journal, year and
authors. Reviewer identity is self-entered initials validated against a regex
(`app.R:423`); there is no authentication.

---

## 5. Duplicate reviews and disagreement

Rows per abstract in the raw log range from 1 to 10; the modal abstract has 3
(one AUTO plus two humans).

- **Repeat submissions by the same reviewer** are resolved by keeping the latest
  `review_timestamp` for that (`abstract_id`, `reviewer`).
- **Disagreement between two humans** is resolved the same way — latest
  timestamp wins. There is no consensus meeting, no third-reviewer arbitration,
  and no recorded adjudication of conflicts.

Interrater agreement (`R/10_interrater.R`, `output/interrater_agreement.csv`):
**519 abstracts** reviewed by ≥ 2 humans, **98.1%** raw agreement, **Cohen's
κ = 0.994**.

κ was `NA` until 2026-09-03 because the `irr` package was not installed on the
machine producing the file; `R/10_interrater.R:67` degrades silently rather than
failing, so the gap was invisible in the output. It matters: with 1,491 of 2,372
decisions being `no_match`, raw agreement is inflated by the base rate and κ is
the number that should be reported. κ = 0.994 is near-perfect, which is
consistent with 29 disagreements over 519 abstracts, but note that reviewers
were **not** blinded to the algorithm's answer — the app pre-selects it — so
agreement partly measures agreement with the algorithm rather than independent
concordance.

Reviewers per abstract among the human-reviewed set: 14 abstracts have one
reviewer, 507 have two, 12 have three.

---

## 6. From the sheet to the analysis

`R/06_analyze_results.R:30-32` calls two pure functions in
`R/utils_decisions.R`:

### `dedup_decisions_for_analysis(decisions)`

1. Drop rows with `NA` reviewer.
2. Compute the set of `abstract_id`s that have **any** non-`AUTO` reviewer.
3. Drop every `AUTO` row for those abstracts — **a human decision always
   outranks AUTO regardless of timestamp**. AUTO is retained where no human
   ruled, because discarding it would strand `probable`/`possible` abstracts at
   `NA` and silently shrink the denominator.
4. Within what remains, keep the latest `review_timestamp` per `abstract_id`.

2,372 rows → **1,153** rows, one per abstract. 533 are human, 620 are AUTO.

### `assign_final_published(results, decisions_deduped)`

```r
final_published = case_when(
  classification == "definite"                                 ~ TRUE,
  manual_decision == "match"                                   ~ TRUE,
  manual_decision == "no_match"                                ~ FALSE,
  classification %in% c("no_match","no_candidates","excluded")  ~ FALSE,
  TRUE                                                         ~ NA
)
final_pmid = coalesce(manual_pmid, best_pmid)
```

Branch order is a **methodological decision, deliberately documented rather than
defended** (`R/utils_decisions.R:56-62`). Branch 1 precedes every reviewer
branch, so a `definite` classification is recorded as published even where a
reviewer said `no_match` (4 abstracts) or `skip` (44 abstracts). Reordering
would move 48 abstracts and is a scientific call, not a bug fix.
`tests/testthat/test-decision_precedence_bva.R` asserts the current order so any
change is visible.

---

## 7. Adjudication accounting — the numbers must close

**Coverage.** 1,153 deduplicated decisions. 1,106 join the cohort; **47 are
orphans** — decisions on video presentations that were later excluded (48 videos
exist; one never received a decision). **Every one of the 1,106 cohort abstracts
has exactly one decision.** No cohort abstract is undecided.

### Deduplicated decisions, all 1,153

| Decision | n |
|---|---:|
| match | 133 |
| no_match | 808 |
| skip | 212 |
| **Total** | **1,153** |

### Restricted to the 1,106-abstract cohort

| Decision | n |
|---|---:|
| match | 130 |
| no_match | 764 |
| skip | 212 |
| **Total** | **1,106** |

### Algorithm classification × reviewer decision (cohort)

| classification | match | no_match | skip | total |
|---|---:|---:|---:|---:|
| definite | 83 | 4 | 44 | 131 |
| probable | 25 | 25 | 31 | 81 |
| possible | 11 | 107 | 24 | 142 |
| no_match | 7 | 591 | 111 | 709 |
| excluded | 4 | 33 | 2 | 39 |
| no_candidates | 0 | 4 | 0 | 4 |
| **total** | **130** | **764** | **212** | **1,106** |

### Final outcome, in the vocabulary Phase 7 asks for

| Decision | n |
|---|---:|
| Confirmed publication (`final_published == TRUE`) | 178 |
| No publication (`final_published == FALSE`) | 873 |
| Unresolved (`final_published` is `NA`) | 55 |
| Skipped by a reviewer but still resolved by the cascade | 157 |
| Explicitly excluded from the analytical cohort | 0 |
| **Cohort total** | **1,106** |

The first three rows sum to 1,106. "Skipped but resolved" is a subset of the
first two rows (212 skips − 55 that fall through to `NA` = 157), not an
additional category.

### Where the 178 come from

| Route | n |
|---|---:|
| `classification == "definite"` (branch 1) | 131 |
| Non-definite with a reviewer `match` (branch 2) | 47 |
| **Total published** | **178** |

Of the 47: 25 `probable`, 11 `possible`, 7 `no_match` and 4 `excluded`. The
seven `no_match` cases are abstracts the algorithm scored below 3 where a
reviewer nevertheless found the publication — a direct measure of the search
layer's recall gap.

### Where the 55 unresolved come from

| classification | manual_decision | n |
|---|---|---:|
| probable | skip | 31 |
| possible | skip | 24 |

This is the **only** route to `NA`, and
`tests/testthat/test-decision_precedence_bva.R:142` asserts it.

---

## 8. What happens when no human decision exists

`R/06_analyze_results.R:33-40`: if `output/manual_review_decisions.csv` is
absent, the script warns and sets `final_published = (classification ==
"definite")`. Every `probable`, `possible`, `no_match`, `excluded` and
`no_candidates` abstract becomes `FALSE`, no row is `NA`, and the reported rate
becomes 131/1,106 = 11.8% with a denominator of 1,106. A fresh checkout without
the decisions CSV therefore silently produces a different headline number with
no warning in the output files. The CSV **is** tracked in git, so this path only
fires if it is deleted.

If a decision exists for an abstract that is not in the cohort, the
`left_join` simply drops it — the 47 video orphans.

If an abstract has no decision at all, `manual_decision` is `NA`, both reviewer
branches fail, and the abstract falls through to branch 4 or to `NA` on its
classification alone.
