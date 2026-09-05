# Author Identity and Demographics

Two different problems are solved by different code paths, and the distinction
matters for how the results should be read.

- **Identity resolution** — deciding *which real person* an AAGL author string
  refers to. Sources: PubMed author records, OpenAlex, ORCID, NPI/NPPES, CMS
  Open Payments.
- **Demographic inference** — attaching an attribute to that person. In this
  project the only demographic actually used in a model is **gender**, and it is
  *inferred from a given name*, never self-reported.

Everything below is traced from executable code. The manuscript-era narrative in
technical appendix A10 covers NPI matching in more detail; where the two differ,
this document is the current one.

---

## 1. The name chain

| Stage | Field | Produced by | Note |
|---|---|---|---|
| Congress programme string | `authors_raw` | `R/01b_parse_web.R` | ScienceDirect format `"J Hayden, M Milla, …"`. **Truncated to 5 names with an ellipsis** for 336 of 1,106 abstracts. |
| Split list | `authors_list` (dropped at save) | `R/02_clean_abstracts.R:58-71` | Credentials (`MD`, `PhD`, `FACOG`, …) and superscript digits stripped, split on commas, ellipsis entries removed. |
| Normalised names | `first_author_normalized`, `last_author_normalized`, `all_authors_str` | `normalize_author()`, `R/utils_text.R:84` | `"LastName FI"`. Unicode dashes normalised to ASCII. `last_author_normalized` is forced `NA` when the list was truncated. |
| Publication author names | `pub_first_author`, `pub_last_author`, `pub_all_authors` | `parse_pubmed_xml()` | `"LastName Initials"` from PubMed `<Author>`. |
| Recovered given name | `first_author_first` | six sources coalesced in `00_run_all.R:127-146` | `coalesce(PubMed ForeName, gender_from_pubmed, ORCID, obgyn_pubs, OpenAlex, Open Payments, author_characteristics)`. 211 of 1,106. |

The congress programme gives **initials only**. Every full given name in this
project was recovered from an external source, which is why so much machinery
exists.

---

## 2. Identity resolution — NPI matching

`R/10_npi_matching.R`. Target: the **first author** of each abstract, restricted
to authors who appear to be US-based.

**Candidate pool.** Primary pool is the ABOG board-certified OB/GYN file
(`$ABOG_NPI_PATH`,
~60,800 records) — much smaller and more relevant than raw NPPES. Fallback pool
is an NPPES mirror in DuckDB
(`$NPPES_DUCKDB_PATH`), queried with an OB/GYN
taxonomy filter (`taxonomy_1/2/3 LIKE '207V%'`), with Physician Compare as a
second fallback.

**Both paths are currently unavailable on this machine** — neither absolute path
resolves (verified 2026-09-03). `data/processed/npi_matches.csv` can only be
taken as given.

**Candidate generation and scoring.** Surname exact match, then a composite
score built from: given-name agreement (full name > middle initial > initial
only), practice city, OB/GYN taxonomy (+10), and temporal plausibility of the
certification/enumeration date against the congress year.

**Confidence tiers** (`R/10_npi_matching.R:351-369`, and the same rule again at
`:551-553` for the NPPES fallback):

```
total_score >= 50 AND (best − runner_up) >= 10   -> "high"
sole candidate AND total_score >= 35             -> "high"
total_score >= 30                                -> "ambiguous"
otherwise                                        -> "low"
```

**One-to-one enforcement.** If two abstracts resolve to the same NPI at `high`
confidence, only the higher-scoring one keeps `high`; the others are demoted to
`ambiguous` (`:393-406`).

**Rejection criteria.** Anything below 30, and any `high` match that loses the
one-to-one contest.

**Manual verification.** None. No human has adjudicated NPI matches.

**Current yield** (`data/processed/npi_matches.csv`, 689 rows):

| `npi_match_confidence` | n |
|---|---:|
| high / ambiguous / low across 689 scored abstracts | 689 total |

with 265 abstracts carrying an `npi_number`, 272 an `npi_state`, 253 an
`npi_subspecialty` and 256 an `npi_gender` in the final dataset.
`npi_match_strategy` records which pool won: `exact`, `initial`, or
`fallback_nppes_taxonomy`.

---

## 3. Identity resolution — the other sources

| Source | Script | What it resolves | Cache | Yield in the final dataset |
|---|---|---|---|---|
| OpenAlex works-by-DOI | `10b_resolve_names_openalex.R` | Full given name from the abstract's own supplement DOI record | `data/cache/openalex_author/` (369) | `openalex_author_names.csv`, 1,106 rows |
| PubMed author + affiliation search | `09f_enrich_gender_from_pubmed.R` | Full given name by searching the surname with an institutional anchor | `data/cache/pubmed_author/` (204) | 132 rows |
| ORCID person records | `09g_gender_from_orcid.R`, `10d_orcid_demographics.R`, `09e_enrich_orcid.R` | ORCID iD, given name, country, institution, role, department, works count | `data/cache/orcid/` (3,312) | `orcid_id` on 198 abstracts |
| OB/GYN journal author search | `09h_gender_from_obgyn_pubs.R` | Full given name from any OB/GYN publication by the same surname | `data/cache/pubmed_obgyn/` (684) | 380 rows |
| OpenAlex works search | `09i_gender_from_openalex.R` | Same idea, different index, journal-ISSN filtered | `data/cache/openalex_author/` | 157 rows |
| CMS Open Payments | `09j_gender_from_open_payments.R` | Full given name from Sunshine Act physician records in the congress window | `data/cache/open_payments/` (232) | 16 rows |
| Senior-coauthor triangulation | `10f_senior_author_triangulation.R` | First author's name via a co-publication with the senior author | — | 2 rows |
| Second-coauthor triangulation | `10g_second_author_triangulation.R` | Same via the second author | — | **0 rows — contributes nothing** |

`orcid_false_positive` records ORCID resolutions that failed a name/affiliation
consistency check. In the shipped data it is single-valued and therefore carries
no information.

---

## 4. Demographic inference — gender

**The resolution hierarchy is defined in code, not in prose.**
`R/10e_merge_demographics.R:285-308` builds `GENDER_PRIORITY` and writes it to
`data/processed/gender_resolution_policy.csv`; lines 361-380 apply it with a
single `coalesce()` and a parallel `case_when()` that records `gender_source`.

| Tier | Source | Column | Resolution | Rationale |
|---:|---|---|---|---|
| 1 | `nppes` | `gender_nppes` | **registry** | Registrant-reported sex from the NPPES `basic_sex` field, keyed on the NPI that `10_npi_matching.R` resolved. Not inferred from a name. Added 2026-09-04. |
| 2 | `npi` | `gender_npi` | **registry** | ABOG board-certification record. Also a recorded field rather than an inference, but no longer regenerable — see below. |
| 3 | `openalex` | `gender_oa` | full name | Given name from the abstract DOI via OpenAlex |
| 4 | `pubmed_fullname` | `gender_pubmed` | full name | Given name from PubMed author + affiliation search |
| 5 | `obgyn_pubs` | `gender_obgyn` | full name | Given name from an OB/GYN journal author search |
| 6 | `openalex_search` | `gender_oax` | full name | Given name from an OpenAlex works search |
| 7 | `orcid` | `gender_orcid` | full name | Given name from an ORCID person profile |
| 8 | `open_payments` | `gender_opm` | full name | Given name from CMS Open Payments |
| 9 | `senior_triangulation` | `gender_tri_sr` | full name | Given name via a senior-coauthor co-publication |
| 10 | `second_triangulation` | `gender_tri_2nd` | full name | Never fires |
| 11 | `ssa` | `first_author_gender` | **initial only** | SSA baby-name data via the `gender` package, then genderize.io for names SSA misses (`R/09c_author_characteristics.R:67-130`), plus a hand-curated 300-name international lookup (`data/validation/international_gender_lookup.csv`) |

**Tiers 1–2 are registry lookups; tiers 3–10 infer from a full given name; tier
11 infers from a single initial.** That three-way distinction, not the tier
number, is what should qualify any gender-stratified estimate.

### Why NPPES outranks ABOG

Not accuracy — they agree on **251 of 252** shared abstracts (99.6%). Two other
reasons:

1. **Reproducibility.** The ABOG export's `LATEST` symlink was repointed
   upstream to a workforce file with no gender column at all, so tier 2 can be
   used but not rebuilt on another machine
   ([FAILURE_MODES.md](FAILURE_MODES.md) F16). NPPES is a public registry keyed
   on an NPI the pipeline already resolved, so tier 1 is regenerable.
2. **Coverage over the same population.** Of the 265 high-confidence NPIs,
   NPPES resolves **263** against ABOG's 252. Putting NPPES first gains 11
   abstracts and loses none, because tier 2 still covers the four NPPES leaves
   blank.

Produced by `R/09k_gender_from_nppes.R` via
`mysterycall::mysterycall_nppes_gender()`, cached one file per NPI under
`data/cache/nppes_gender/`. A failed lookup is deliberately **not** cached, so
it is retried rather than frozen as a negative.

**What changed when it was added** (2026-09-04): coverage 1,065 → 1,066; **11
abstracts moved from a name-inferred tier to a registry tier**; four values
changed, all four now registry-sourced —

| abstract | was | from | now |
|---|---|---|---|
| `AAGL2018_021` | male | `ssa` (initial only) | female |
| `AAGL2021_053` | female | `openalex_search` | male |
| `AAGL2022_003` | male | `ssa` (initial only) | female |
| `AAGL2022_031` | female | `npi` (ABOG) | male |

The first three replaced a name inference with a registry value; two of those
had rested on a single first initial. The fourth is the only NPPES/ABOG
disagreement and is logged to
`data/processed/gender_nppes_abog_conflicts.csv`.

**Disagreement handling.** `R/10e_merge_demographics.R:314-338` computes, per
abstract, the set of distinct non-`NA` values across all ten columns. An
abstract with more than one distinct value is written to
`data/processed/gender_conflicts.csv` (228 abstracts) and flagged
`gender_conflict = TRUE`. The conflict is **resolved by priority, not
adjudicated** — the highest-ranked source simply wins.

**`gender_n_sources`** counts how many of the ten columns returned any value
(0–4 in the current data). It is a confidence proxy: a value of 1 means the
result rests on a single source with no corroboration.

**Current coverage.** `gender_unified` is non-`NA` for **1,065 of 1,106
(96.3%)**. By source:

| `gender_source` | n |
|---|---:|
| `ssa` (tier 11, **initial only**) | 287 |
| `nppes` (tier 1, **registry**) | 263 |
| `obgyn_pubs` (tier 5) | 224 |
| `openalex_search` (tier 6) | 100 |
| `openalex` (tier 3) | 82 |
| `pubmed_fullname` (tier 4) | 82 |
| `orcid` (tier 7) | 16 |
| `open_payments` (tier 8) | 7 |
| `npi` (tier 2, **registry**) | 4 |
| `senior_triangulation` (tier 9) | 1 |
| `second_triangulation` (tier 10) | 0 |
| unresolved | 40 |

**267 of the 1,066 resolved genders (25.0%) now come from a registry** rather
than from a name. **287 (26.9%) still rest on tier 11 — a single first
initial** — and that remains the largest single contributor and the main reason
to treat gender-stratified estimates cautiously.

**40 abstracts have no gender at all.** 231 of the 1,066 resolved values
(21.7%) rest on a cross-source disagreement — up from 228 because NPPES adds an
eleventh source that can disagree.

### The required statement

**Inferred gender is not self-reported gender.** Every value in
`gender_unified` except the two registry tiers (`nppes`, `npi` — 267 of 1,066)
is a probabilistic inference from a given name, and 287 of them from a single
initial. Name-based inference is
known to perform worse for non-Western names, which are heavily represented in
this international cohort. Any estimate stratified by `gender_unified` —
including the Cox term reported in the README — carries non-differential
misclassification at minimum, and possibly differential misclassification if
name-inference accuracy correlates with the outcome through country of origin.

The README already flags the Cox gender estimate as provisional pending a re-run
on the unconflicted subset. That re-run has **not** been done.

---

## 5. Other author-derived variables

| Variable | Function | File | Coverage |
|---|---|---|---:|
| `first_author_state` | `parse_us_state()` | `R/utils_states.R:163` | 108 |
| `first_author_country` | `parse_country()` | `R/utils_states.R:308` | 208 |
| `first_author_acog_district` | `acog_district_for_state()` | `R/utils_acog.R:68` | 197 |
| `practice_type` | `classify_practice_type()` | `R/utils_affiliation.R:122` | 193 |
| `subspecialty` | `classify_subspecialty()` | `R/utils_affiliation.R:218` | 179 |
| `career_stage` | `classify_career_stage()` | `R/utils_affiliation.R:283` | 3 |

All six are parsed from the **matched publication's** PubMed affiliation string,
so they exist only where a publication was matched — which is why coverage is
17–19% rather than a data-quality failure. `classify_practice_type()` also
consults `data/validation/teaching_hospital_names.txt` (2,754 ACGME names) via
`is_teaching_hospital()`.

`demographics_from_matched_pub` marks rows whose demographics came from a
confirmed match. It replaced an earlier destructive blanking of these columns,
so the values are recoverable rather than overwritten.

### Unified columns mix two vocabularies

```r
state_unified        = coalesce(npi_state, first_author_state)
subspecialty_unified = coalesce(npi_subspecialty, subspecialty)
```

`state_unified` is fine: both `npi_state` and `first_author_state` use
two-letter USPS codes, and its 40 levels are simply the union of the two
sources' coverage.

`subspecialty_unified` was **not** fine. `npi_subspecialty` carries ABOG's
spelled-out certification names and `subspecialty` carries
`classify_subspecialty()`'s short codes, so a plain `coalesce()` produced 13
levels for about 8 concepts: `MIG`/`MIGS`, `FPMRS`/`Female Pelvic Medicine &
Reconstructive Surgery`, `general_OBGYN`/`Generalist`, `GYN_ONC`/`Gynecologic
Oncology`, `REI`/`Reproductive Endocrinology and Infertility`. Any subgroup
analysis on that column split real categories in two.

**Fixed.** `harmonise_subspecialty()` in `R/10e_merge_demographics.R` now maps
every known spelling onto the short-code vocabulary before the coalesce, and
returns unrecognised values unchanged so a new upstream label stays visible
rather than being silently dropped.

---

## 6. Coverage summary

| Quantity | n | % of 1,106 |
|---|---:|---:|
| Full given name recovered for the first author | 211 | 19.1% |
| Gender resolved | 1,066 | 96.4% |
| Gender from a registry rather than a name | 267 | 24.1% |
| Gender with a cross-source disagreement | 231 | 20.9% |
| NPI number assigned | 265 | 24.0% |
| ORCID iD resolved | 198 | 17.9% |
| Practice type classified | 193 | 17.5% |
| Subspecialty classified (either source) | 387 | 35.0% |
| US state (either source) | 336 | 30.4% |

Note the tension between rows 1 and 2: **gender is resolved for 1,065 abstracts
but a full given name is recorded for only 211.** The gap is the SSA/genderize
tier working from initials, the registry tiers that need no name at all, and the
sidecar sources that returned a gender without their given name surviving into
the final dataset. Read `gender_source` before using `gender_unified`.
