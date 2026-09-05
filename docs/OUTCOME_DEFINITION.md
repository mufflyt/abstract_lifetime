# Operational Definition of the Primary Outcome

Written to be adapted directly into a manuscript Methods section. Every clause
states what the code actually does, and flags where the implemented rule differs
from what a reader would assume.

---

## The definition as implemented

> An AAGL oral presentation was counted as **subsequently published** when
> either (a) the automated matching algorithm assigned its best candidate a
> composite score of at least 7 with supporting title or abstract text
> similarity and no scoring tie (`classification == "definite"`), or (b) a human
> reviewer, presented with the abstract alongside its ranked candidate
> publications, recorded a confirmed match and supplied the corresponding PMID.
> Presentations were counted as **not published** when the reviewer recorded no
> match, or when — absent a reviewer ruling — the algorithm found no candidate,
> found only candidates scoring below 3, or found that the best-scoring
> candidate had been published before the congress. Presentations for which the
> algorithm returned an intermediate classification (`probable` or `possible`)
> and the reviewer declined to rule were treated as unresolved and excluded from
> the denominator (n = 55 of 1,106).

Code: `assign_final_published()`, `R/utils_decisions.R:70-93`.

---

## Clause-by-clause

**Does publication before the conference count?**
No, with one exception. A best candidate dated before the congress date forces
`classification == "excluded"`, and the abstract is retained in the cohort and
counted **unpublished** (39 abstracts). The exception is branch order: a
reviewer who records `match` on such an abstract overrides it to published
(4 of the 39). No rule constrains the reviewer's PMID to be post-congress.

The comparison date is the congress start date from `config.yml`, not the
abstract submission deadline, which is not recorded anywhere in the repository.
The technical appendix (A13.6) argues that a paper appearing within about six
months of the meeting was plausibly still in press at submission and belongs in
the numerator; that argument has not been adopted, and the six-month boundary is
an assumption, not a validated cutpoint. It also plays no part in the current
behaviour: the four abstracts that survive the exclusion are separated from the
other 35 purely by reviewer verdict, and their intervals overlap the excluded
ones, so no cutpoint reproduces the split. Whether a reviewer `match` should
override a pre-congress exclusion remains an open decision.

**Does online-ahead-of-print count?**
Yes, but it is dated to the print issue. **PI decision, 2026-09-05: the print
issue date is the publication date.** The pipeline reads
`JournalIssue/PubDate` and ignores `ArticleDate`, which is now the intended
behaviour rather than an undocumented one, and issue dates given only to the
month resolve to the first of that month. An OAP paper's `months_to_pub` is
therefore measured to its later print month, which lengthens time to
publication.

The same date decides whether a publication preceded its congress. That was
previously ambiguous in a way that mattered: `output/excluded_pre_congress_publications.csv`
was built from `ArticleDate` while the analysis measured from
`JournalIssue/PubDate`, so the two disagreed by 1.5 to 4.9 months on the four
contested abstracts. Under the decided rule the analysis is already correct and
**that file is on the wrong basis and needs regenerating**; it is evidence, not
an input to any number, so nothing downstream is wrong today. See appendix A18.

`tests/testthat/test-publication_date_basis.R` pins the decision: it fails if
the parser starts preferring `ArticleDate`, and it fails if any interval in the
analytical dataset can only be reproduced from an electronic date.

**Must the abstract and the paper share authors?**
Not as a requirement. Author agreement is scored (components 3–6, up to 6 of a
maximum 14 points) but no component is mandatory. An abstract can reach
`definite` on title similarity, journal relevance and timing alone: 3 + 1 + 1 =
5 is below the threshold of 7, so in practice at least some author or abstract
evidence is needed, but no *specific* author must match.

**Are title changes allowed?**
Yes. Jaccard title similarity of 0.35 earns 1 point and 0.55 earns 2; a
substantially retitled paper can still reach `definite` on author and abstract
evidence. The `title_fragment` and author-based search strategies exist
precisely to find retitled papers.

**Can the sample size differ?**
Yes — sample size is never compared. `R/09e_fidelity_checks.R` records
abstract-versus-paper discrepancies descriptively in
`data/processed/fidelity_checks.csv`, but nothing in the outcome definition
consults it.

**Do secondary analyses of the same data count?**
Yes, if they score highly enough or a reviewer confirms them. There is no rule
distinguishing "the full publication of this abstract" from "a later paper by
the same team on the same dataset". This is a known limitation shared with most
abstract-to-publication studies.

**Do conference supplements count?**
No. `is_supplement_article()` removes candidates that are the AAGL supplement
itself (JMIG, congress volume, congress year, `Suppl` in the issue field). Six
2015 abstracts nevertheless matched their own supplement listing and were then
classified `excluded` because the supplement posted online before the meeting
(appendix A13.6) — so the filter is not airtight.

**Do letters, editorials and comments count?**
They are excluded at search time by `build_date_filter()`'s
`NOT ("Letter"[PT] OR "Comment"[PT] OR "Editorial"[PT] OR "Published
Erratum"[PT] OR "Retraction of Publication"[PT])`. But `pub_type_canonical` in
the final dataset still contains an `Editorial/Letter` level, because the
supplementary sources (Europe PMC, OpenAlex, Semantic Scholar, DOI chain) do not
apply that filter and can inject such a record into the pool.

**Do papers with substantially overlapping data count?**
Yes, implicitly — see "secondary analyses" above.

**Can one abstract match more than one publication?**
No. Exactly one PMID is credited per abstract: `final_pmid =
coalesce(manual_pmid, best_pmid)`. Multiple full publications arising from one
abstract are not represented.

**What if several publications could represent the abstract?**
The highest composite score wins. If the top two scores are exactly equal,
`has_tie` is set and a `definite` classification is demoted to `probable` so a
human chooses. Below `definite` a tie does not change the class; it only adds
the abstract to the review queue. Ties among three or more candidates are
recorded identically to two-way ties. When no reviewer resolves a tie, the
arbitrary stable-sort winner stands.

**Which date is the publication date?**
The first day of the PubMed `JournalIssue/PubDate` month; a year-only record
becomes 1 January. `months_to_pub = (that date − congress date) / 30.44`. See
[MATCHING_ALGORITHM.md](MATCHING_ALGORITHM.md) §5.

---

## Important caveat on the time-to-publication outcome

`months_to_pub` is available for **104 of the 178 published abstracts (58%)**.
The other 74 have a confirmed publication and a PMID but no publication date,
because their PMID is missing from the candidate file that
`R/05_adjudicate.R` joins against. Aim 2's median, the Kaplan–Meier curve and
the Cox model therefore run on 104 events, not 178.

The missingness is a file-staleness artefact
([FAILURE_MODES.md](FAILURE_MODES.md) F2), not a property of the publications,
but it is not random with respect to congress year and should be treated as
informative until the candidate pool is rebuilt.
