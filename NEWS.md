# NEWS

## 2026-09-05 - the denominator question, answered

The rule is now: a publication that appeared before the congress cannot count as
having come out of it, and no reviewer judgment overrides that. The numerator
moves from 178 to 171 and the rate from 16.9% to 16.3%.

The denominator does not move. An excluded abstract stays in the cohort and is
counted unpublished; it does not leave the study. Neither does the median time
to publication, which stays at 13.7 months, because every abstract this affects
had a negative interval and was already outside the time-to-event analysis. The
Cox model actually gains seven observations, since an abstract counted
unpublished contributes censored follow-up while one counted published with a
negative interval is discarded outright.

Implementing it was less obvious than it sounds. The scorer already flags
pre-conference candidates with an `excluded` classification, and trusting that
flag would have missed three abstracts. Two of them carry a reviewer-supplied
PMID different from the algorithm's best candidate, and the pre-conference
penalty had been computed against the wrong paper; for those two the interval
reaching the cascade was missing altogether, and only becomes 10.3 months before
the congress after 06 re-joins the publication fields on the credited PMID. A
third was scored `definite` despite its paper predating the congress by two
weeks, and the `definite` branch used to win outright. So the rule is applied to
the credited publication's date, twice: once in the cascade, and once more after
the refresh.

That in turn exposed a drift. The table and figure scripts recompute the outcome
through the same cascade, with comments explaining that this is how they avoid
diverging from the analysis. They do not do 06's date refresh, so under the new
rule they would have called two abstracts published that the analysis calls
unpublished. Both now adopt the settled outcome rather than recomputing it.

The best evidence that the rule is right is a number that went to zero.
`n_pre_congress`, the count of published abstracts whose paper predates their
congress, was 7 and is now 0. The estimand is internally coherent: published
means published after the congress, by construction rather than by convention.

One finding weakened and should be said plainly. Academic affiliation now
survives only 50.4% of bootstrap resamples, down from 62.0%. It was already
described as not sampling-robust; it is now about a coin flip, and should be
read as suggestive at most.

## 2026-09-05 - which date counts

PubMed gives an article two dates: the electronic release and the print issue.
For a journal that publishes online ahead of print they can be months apart. The
study had been using both at once. Every interval in the analysis was measured
to the print issue, while the file identifying papers that predated their
congress had been built from the electronic date. Neither was wrong on its own
terms; nothing had recorded which question the study was asking.

It mattered most where it was hardest to see. For one contested abstract the
electronic date puts the paper five months before the congress and the print
issue puts it six days before, which is the difference between a pre-existing
paper and one that appeared essentially at the meeting.

The PI has decided: the print issue date is the publication date. Because the
analysis already worked that way, no reported number moves. What changes is that
the Methods now say so, and a test enforces it. The sharpest part of that test
uses the disagreement itself: the two bases give answers 1.5 to 4.9 months apart
for the four contested abstracts, so a recorded interval identifies which basis
produced it.

Two things are deliberately still open. The exclusion file is on the electronic
basis and needs regenerating, and until it is, whether the remaining question
concerns four abstracts or seven is unknown. And whether a reviewer's `match`
should override a pre-congress exclusion, which is what moves the numerator
between 178 and 174, is a separate decision that has not been taken.

## 2026-09-05 (later) - the registry's last two copies

The test-governance work earlier today removed the hand-written table of
registered failures from the README. There was a second copy in
`docs/VALIDATION.md`, and it had drifted the same way: three rows against a
manifest of twenty-three. Anyone reading it would have concluded the project had
three open questions.

That is the failure the suite gate exists to prevent, reappearing one level up.
A description of the truth, kept beside the truth, updated separately, and
wrong. Both copies are gone and both documents link to the generated registry.
What remains in prose is a count, written as a marker a test checks against the
manifest, plus a guard that fails if a table of manifest rows ever comes back.

Writing that guard was instructive twice over. Its first form flagged nineteen
rows in VALIDATION.md, which turned out to be the legitimate test *inventory*, a
catalogue of every test file with its assertion count and gate. The difference
is that a manifest row cites a line number and an inventory row does not, so the
guard now requires one. A check that forbids the wrong thing is worse than no
check, because it teaches people to work around it.

Then it failed on its first real run, and it was right to. The README said 12
skips; the skip manifest holds 13 entries. Both numbers are correct. An entry in
that manifest is a registration, not a prediction: whether a test actually skips
depends on whether the gitignored caches are present, and the whole point of the
gate is that an approved skip which manages to run is fine. So the marker
belongs on the registered count and the observed count is labelled as an
observation. Marking both would have forced them to agree, which would have
meant either falsifying the README or deleting a legitimate manifest entry to
make a number match.

The last thing was the same principle applied to numbers. Several figures
justifying the proportional-hazards remediation had been worked out at a prompt
and typed into prose: the model frame size, what the global test becomes with
the violating term dropped, the AIC of each fit, the Schoenfeld correlation, the
share of the frame at the author ceiling. A number with no producer cannot be
re-derived, so nothing notices when it stops being true, and every one of them
had already shifted at the last refit. They now come from a script, and seven
are registered as claims the suite recomputes.

Nothing scientific was decided. Twenty-three decisions remain open.

## 2026-09-05: the tests that were not running

The suite had been reporting a number that was partly fiction.

A test run has three outcomes and only two of them were being watched. A test
can pass, it can fail, or it can not run at all, and the gate's rule for
"passing" was `failed == 0 & error == 0`, which is true of a skipped test. So a
test that quietly stopped asserting anything was counted on the good side of
every summary line it appeared in.

That is not a hypothetical. It had already cost two days. `main` went red on
2026-09-03 with an error insisting that a registered failure had started
passing. It had not. The test read the PubMed XML cache, which is gitignored, so
in CI it skipped, the gate read the skip as a pass, and then complained that the
manifest entry describing the real finding was stale. There was no correct fix
available while the gate believed a skip was evidence of anything.

Pulling that thread found 24 test blocks that skipped in CI, holding **75
assertions that only ever ran on a developer machine**. Among them were 45
guarding the Shiny deploy bundle against being stale, written after a bundle
was found 135 days behind the analysis, with reviewers adjudicating a
pre-denominator-fix cohort, and `F2: every winning PMID resolves in the
candidate pool`, one of the pipeline's central invariants, which had never once
run in CI.

Most of them are now running, and not by relaxing anything:

- The bundle checks read `bundle_manifest.csv`, which is committed and records
  each source's md5 and byte count at the moment the bundle was built. A source
  that still matches it is byte-identical to its copy, so the assertion can read
  the source. Simulating a stale bundle now turns three tests red where CI
  previously reported nothing at all.
- The candidate checks read a committed 1.4 MB `abstract_id`/`pmid` index rather
  than the gitignored 130 MB pool. Two columns, 65,697 pairs, which is all those
  assertions ever looked at.
- Table 1's reconciliation needed one 4 KB file that had never been added.

**75 inert assertions, down to 23.** The 23 that remain are genuinely
environment-bound and each says so, with what it would take to enable it.

The durable part is the guard. `tests/expected_skips.yaml` now records every
approved skip with a reason and a route to enabling it, and the gate fails on
any skip that is not there. It is deliberately one-directional: an unapproved
skip fails, an approved skip that runs anyway does not, because the bundle and
the cache legitimately exist on one machine and not the other, and a guard that
cannot tell those apart would be turned off within a week.

`test-shiny_e2e.R` deserves its own sentence. Three hundred and thirty-two lines
of browser tests, credited with 17 tests in the validation inventory, asserting
nothing anywhere, because `shinytest2` is installed on no machine involved. It
is now explicitly opt-in, and carries a floor assertion that runs everywhere and
fails if that exclusion stops being recorded.

## 2026-09-05 (later): cycles 17 to 24, and what they found

The 24-cycle test-generation protocol had stopped at 16. The remaining eight
cycles added 165 assertions and surfaced eight decisions. Two of them move a
number that appears in the manuscript.

**Four abstracts are counted as published on the strength of a paper that
appeared before the congress at which the abstract was presented.** The study
maintains `output/excluded_pre_congress_publications.csv`, listing 39 such
abstracts, and applies the exclusion to 35 of them. For `AAGL2021_002`,
`AAGL2021_049`, `AAGL2023_042` and `AAGL2023_048` the matched PMID is exactly
the one listed as excluded. Applying the rule consistently moves the numerator
from 178 to 174.

**Four more are counted as published against an explicit human `no_match`.** A
different four. A reviewer looked at the abstract, said the candidate was not its
publication, and the outcome column says published anyway. One was already known
from the shared-PMID finding; three are new.

Neither is fixed here. Both are definitions of the outcome, and choosing one
silently is exactly what the expected-failure manifest exists to prevent.

The rest: a search strategy that searched for nothing because the cohort has no
keywords column; one survival event lying four months beyond its own censoring
horizon, because it is timed to a print-issue date after the search ended; 156
of the 285 abstracts queued for human review that were never humanly reviewed
and were closed by the algorithm they had been escalated away from; and
title-fidelity verification that covers the matches the algorithm was already
confident about while skipping the 22 that human adjudication had to settle.

One process note worth keeping, because it invalidated a day of my own local
verification. A test passed on every local run and failed the instant the gate
ran in a clean checkout: the working tree held a regenerated artefact belonging
to a concurrent agent, so every local run had been reading a fix that was not
committed. **A shared working tree is not a safe place to validate.** Gate
results quoted as authoritative here now come from a throwaway `git worktree`,
which holds only tracked files and is therefore what CI actually sees.

Eight of my own assertions in these cycles were wrong before they were right,
every one because I asserted a contract nobody had stated rather than the one
the code holds. Twice the corrected version then found something the wrong one
would have buried.


## 2026-09-04 (later) — the proportional-hazards question, answered

Yesterday's audit left four open decisions parked in an expected-failure
manifest. This is the first of them closed, and it is closed on evidence rather
than by picking a convention.

The complaint was that the Cox model's proportional-hazards assumption failed a
global test at p = 0.043. A global test says the model is wrong somewhere; it
does not say where, and the three standard remedies point in different
directions. So the first thing added was the per-term Schoenfeld tests, which
turn out to be unambiguous: `n_authors` fails at p = 0.002 and **nothing else
comes close** — the other five terms sit between p = 0.13 and p = 0.84, and
refitting without `n_authors` puts the global test back at p = 0.497.

That reframed the decision. Stratifying on `n_authors` does restore the
assumption (global p = 0.688) and it moves no other hazard ratio by more than
2%, which is genuinely reassuring — but `n_authors` is one of only two terms
that survive bootstrap resampling, so stratifying it away would have deleted a
real finding in order to fix a diagnostic. It is fitted with a log-time
interaction instead, and the stratified model is kept as the sensitivity
analysis it should always have been.

What the constant hazard ratio was hiding is the interesting part. The Cox table
reports HR 1.26 per additional author. Time-resolved, that is HR 1.01
(0.84–1.21) at three months and 1.51 (1.25–1.84) at two years. **Team size does
not make publication faster; it makes it keep happening.** Larger teams are not
quicker to first publication — they are the ones still converting abstracts into
papers years after the congress, while small-team abstracts go quiet. A single
averaged number could not have said that, and read as a constant it was
misleading in both directions at once.

One caveat is not resolved and should not be glossed: `n_authors` is truncated
at five by the source ingest, and 48.7% of the model frame sits at that ceiling.
The time-varying *shape* is not an artefact of the cap, but the per-author
magnitude is estimated on a compressed covariate.

Two things were found while doing this that had nothing to do with hazards. The
Results paragraph in `docs/abstract_results_section.Rmd` still asserted
"proportional hazards assumption met" and named US-based affiliation as the only
significant predictor — stale on both counts, and it rendered "was associated
with higher odds" over a p-value of 0.083. That paragraph now derives its
numbers *and* its significance wording from the fitted model. And
`docs/STATISTICAL_ANALYSIS.md` printed a Cox formula containing `has_funding`
two sections after its own table recorded `has_funding` as screened out.

The test that guarded this is not deleted, it is replaced by a stronger one. The
old assertion was "the global test passes". The new pair requires that any
violation be attributed to specific terms, that every violating term be named in
a remedy, that the remedy actually restore the assumption, and that no
non-violating hazard ratio move more than 15% or change direction under
stratification. A future model that violates PH silently will fail, which the
old test could not detect once it had been added to the manifest.

The manifest gate learned something too. It already failed when a listed test
started passing. It now also fails when an entry names a test that no longer
runs — which is exactly what would have happened here, since the test was
renamed. Three entries remain.

## 2026-09-04 — audit and remediation

A full documentation audit produced a `docs/` reference set and found seventeen
ways the pipeline could be plausibly wrong. Nine were then fixed. The narrative
is appendix A15; the mechanisms are in `docs/FAILURE_MODES.md`; the arithmetic
is in `CHANGELOG.md`.

**The publication rate did not change: 178 of 1,051 evaluated, 16.9%.** That is
worth stating plainly, because almost everything underneath it did.

The largest repair was to the candidate pool. `03b_search_crossref.R` rewrites
`pubmed_candidates.csv` in place, and on 19 April it ran after the scoring step,
leaving a file that was a strict subset of the pool the scores came from. 283 of
the 1,102 winning PMIDs could not be resolved, so 74 confirmed publications
carried no publication date and every time-to-event analysis silently ran on 104
events instead of 178. Rebuilding the pool from the surviving detailed scores
restored all of them.

The second was to the covariates. `02_clean_abstracts.R` derives about twenty
predictors from abstract text, but runs before the two scripts that recover that
text for 2012-2018 — so for seven of the twelve congresses it had been reading
titles. `is_academic` was TRUE for 148 abstracts and is now TRUE for 371.
That changed the models: academic affiliation went from no effect to a
significant negative one. The residual gradient is now confined to 2017 and
2018, which genuinely have no recoverable text.

Along the way the snippet backfill was found to have written the page footnote
`"*: Corresponding author."` into all 95 abstracts of the 2018 congress. At 24
characters it passed the length gate that decides whether a row still needs
backfilling, so those rows could never be repaired and the footnote had
displaced the title as the source for every derived variable.

Gender is now led by a registry rather than by a name. NPPES registrant-reported
sex, keyed on the NPI already resolved, is tier 1; the ABOG board-certification
export that used to hold that position lost its gender column upstream and could
no longer be regenerated. The two agree on 251 of 252 shared abstracts. Eleven
abstracts moved from a name-inferred tier to a registry one, and two values that
had rested on a single first initial were corrected.

Three new analyses answer questions the manuscript had asserted rather than
tested. The 55 unresolved abstracts are **not** missing completely at random —
they differ on study design and author count, the latter a significant predictor
— so the 16.1%-21.1% bounds are the honest envelope rather than a formality.
Only two of seven regression terms survive resampling: author count (97.2%) and
randomized design (93.6%); academic affiliation survives 67.4%. And no term
changes direction when any single congress is dropped, which matters because
2017 and 2018 have no abstract text at all.

Two things got worse in the sense that matters. The proportional-hazards
assumption, marginal at p = 0.056, is now violated at p = 0.043 — dropping a
term with seven events made a latent violation visible rather than creating one.
(Resolved later the same day; see the entry above.) And the cohort truncation catalogued in appendix A14 was confirmed against the
Crossref deposit: the pipeline ingested 1,154 of 7,711 supplement items, and ten
of twelve congresses captured no video presentations at all, meaning the capture
window closed while still inside the oral block. Neither is remediated here.
Both are now measured, tested and impossible to miss.

Five functions were borrowed from the `mysterycall` package, pinned at a commit,
each degrading to the previous behaviour when it is absent.

Tests went from 519 passing with 1 failure to 900 passing with 4. All four
failures are deliberate: each marks a decision that belongs to the author.


## 2026-04-28

Recovered from an external drive on 2026-09-01; this work was completed on
2026-04-28 but never committed. See `docs/technical_appendix.Rmd` section A12
for the full write-up.

### Matching corrections

Four defects in the matching layer, all biasing in the same direction — each
suppressed true abstract-to-publication matches.

- **Session-number prefixes stripped** (`utils_text.R`, `02_clean_abstracts.R`).
  The 2013, 2017, 2018, and 2021 congress programs prefix titles with a session
  number (`"4 - Laparoscopic..."`). The prefix entered PubMed Strategy 1 as part
  of the title phrase, which then matched nothing. Because the affected
  congresses are a non-random subset of years, this confounded the time trend.
  Stripped via `^[0-9]+\s+[-–]\s*`; the required space before the dash
  protects titles like `"5-year outcomes..."`.
- **Non-article publication types excluded** (`build_date_filter()`). Letters,
  comments, editorials, errata, and retractions were eligible candidates. They
  carry the title and authors of the paper they discuss, so they score highly on
  the two heaviest components and can outrank the genuine publication. Now
  excluded at the search layer rather than post-hoc.
- **JMIG supplement detection narrowed** (`is_supplement_article()`). The test
  matched on journal + volume + year, but a supplement shares its volume with
  that volume's regular issues — so every regular JMIG article in the matching
  volume and year was excluded. This was the most consequential defect: AAGL
  abstracts are likeliest to publish in JMIG within about a year, exactly the
  window being blanked. Now requires the Issue field to contain `"Suppl"`, or
  November publication where PubMed omits the issue.
- **Title phrase search fixed** (`build_search_strategies()`). Strategy 1
  dropped tokens under three characters, which does not shorten the phrase — it
  produces a word sequence that appears in no title. Now takes a consecutive
  8-word window preserving stopwords, anchored at the first token of 3+
  characters.

### Gender resolution transparency

- **`gender_conflict` and `gender_n_sources`** populated for every row, recording
  the shape of the evidence rather than only the winner of the priority
  waterfall. Enables sensitivity analysis restricted to uncontested assignments.
- **`data/processed/gender_conflicts.csv`** — 228 cross-source disagreements
  with competing values.
- **Two new waterfall sources**: OpenAlex author search (157 resolutions),
  CMS Open Payments (16). `gender_unified` coverage 98.8%.
- **Second-author triangulation returns zero rows.** Script retained (the
  senior-author equivalent does resolve names) but contributes nothing; it
  should not be counted as an active source.

### PubMed metadata

- `parse_pubmed_xml()` extracts `JournalIssue/Issue` as `pub_issue`, required by
  the corrected supplement test.

### New outputs

- `output/final_analytical_dataset.csv` — unified dataset (1,106 rows x 90
  columns) with demographics and human decisions merged, exported by
  `06_analyze_results.R` for external analysis.
- `docs/aagl_abstract_programmatic.Rmd` / `.docx` — programmatic abstract draft.

### Regenerated

Processed data, Cox/KM/logistic models, and result tables re-run against the
corrected pipeline. Figure set renamed (`figure2_km_curve`, `figure3_km_by_year`,
`figure4_subgroup_rates`, `figure5_cox_forest`, `figure6_time_to_pub`,
`figureS1`-`figureS4`); stale `figure2_time_to_pub`, `figure3_km_curve`,
`figure4_strategy_perf`, and `figure5_score_dist` files removed.

### Denominator defect fixed (issue #2)

`R/05_adjudicate.R` dropped abstracts whose best candidate predates the
conference out of the cohort. `excluded` describes the candidate, not the
abstract, and the Cochrane MR000005 denominator is abstracts presented. The 39
are retained and counted as unpublished. Nothing downstream needed changing —
`06`, `07`, `08` and the Shiny app already treated `excluded` as unpublished.

Publication rate **17.2% → 16.9%** (95% CI 14.8–19.3), cohort 1,067 → 1,106,
published 174 → 178. Four of the 39 carry a reviewer's `manual_decision ==
"match"`, so the filter was also discarding confirmed publications; the two
effects partly offset. Details and the four PMIDs in appendix A12.7.

### CI restored

CI had failed on `main` since at least 2026-04-19. Two causes, neither a real
regression: `test-shiny_app.R` read gitignored artefacts with no existence
guard (impossible to pass in a fresh checkout), and two coverage thresholds
were unsatisfiable by construction rather than merely unmet. Suite is now
392 passing / 0 failing locally, and green in a tracked-files-only checkout.

### Known gaps

- The four matching corrections shipped in one re-run, so their individual
  contributions are not separately identified. No ablation was performed.
- Supplement detection still falls back to a November-month heuristic where
  PubMed omits the issue field.
- ~~Three pre-existing `test-pipeline_semantics.R` failures remain.~~ Resolved
  as of 2026-09-03: the two coverage thresholds were unsatisfiable rather than
  unmet and were re-pointed at regression floors, and the 1,106 vs 1,067 row
  mismatch was the denominator defect, now fixed. `test-pipeline_semantics.R`
  passes in full; the suite's one remaining failure is `test-shiny_app.R:458`,
  which reports a genuinely stale deploy bundle.
- Three further defects were found in the 2026-09-03 documentation audit and
  have **not** been fixed. They are documented in `docs/FAILURE_MODES.md`:
  the ScienceDirect listing is truncated at roughly 100 items per congress
  supplement (F1); `data/processed/pubmed_candidates.csv` is a stale subset of
  the pool the scores were computed against, so 74 of the 178 published
  abstracts carry no publication date and every time-to-event analysis runs on
  104 events (F2); and the text-derived study characteristics were computed
  before the abstract-text backfill and never recomputed, producing a step
  change at 2018/2019 in five model variables (F3).

## 2026-04-19

### Demographics pipeline hardening

- **Single merge point**: `10e_merge_demographics.R` is now the sole writer to `abstracts_with_matches.csv`. All 12 producer scripts write sidecar CSVs only.
- **Unified gender column**: Replaced dual `first_author_gender`/`gender_unified` with a single `gender_unified` column using a 10-tier priority waterfall (NPI > OpenAlex > PubMed > OB/GYN pubs > OpenAlex search > ORCID > Open Payments > senior triangulation > second triangulation > SSA).
- **Gender coverage**: 73.9% -> 99% (1056/1067).
- **Reversible blanking**: PubMed-derived demographics for non-confirmed matches are blanked via a `demographics_from_matched_pub` flag rather than destructive NA assignment.
- **Orchestrator script**: `R/run_demographics.R` runs all demographic producers in dependency order.

### NPI matching enhancements (isochrones-inspired)

- **Additional name sources**: 5 gender enrichment sidecars (PubMed, OB/GYN pubs, OpenAlex search, Open Payments, ORCID) now feed full names into NPI matching.
- **NPPES taxonomy fallback**: Queries `temporal_all_years_fixed` with OB/GYN taxonomy filtering (`207V%`) for authors not in the ABOG pool.
- **Middle initial scoring**: +5 pts for middle initial agreement (from ABOG pool and NPPES).
- **City matching**: +10 pts when PubMed affiliation city matches NPPES practice city.
- **Temporal scoring**: +5 pts when NPI was enumerated before congress year.
- **Initial-only authors**: NPPES fallback now includes authors without full names.
- **NPI high-confidence**: 248 -> 278 (40.3% of US authors).
- **State coverage**: 10% -> 31%. Subspecialty coverage: 17% -> 36%.

### Shiny adjudication app

- Google Sheet link always visible in sidebar (hardcoded URL).
- Auto-advance to next abstract + scroll to top after saving a decision.
- Decision form resets (radio, PMID, notes) after save.
- Removed conflict confirmation modal — all reviewer decisions saved directly.
- "Show unreviewed only" filter now excludes AUTO (algorithm) rows.
- `deploy.R` script auto-slims `pubmed_candidates.csv` for bundle.
- Auto-deploy added as final step in `00_run_all.R`.

### New scripts

- `R/09i_gender_from_openalex.R` — Gender from OpenAlex author search.
- `R/09j_gender_from_open_payments.R` — Gender from CMS Open Payments database.
- `R/10g_second_author_triangulation.R` — Name resolution via second coauthor PubMed search.
- `R/run_demographics.R` — Demographics pipeline orchestrator.
- `shiny/adjudication_app/deploy.R` — Bundle preparation + shinyapps.io deployment.

### Technical appendix

- Added section A10.11: Coauthor Triangulation for Name Disambiguation (senior + second author PubMed co-publication search, results and limitations).

## 2026-04-18 (earlier)

### Gender enrichment

- `R/09f_enrich_gender_from_pubmed.R` — PubMed full-name search for gender resolution.
- `R/09g_gender_from_orcid.R` — Gender from ORCID person records.
- `R/09h_gender_from_obgyn_pubs.R` — Gender from OB/GYN publication author search.

## 2026-04-17

### Initial pipeline

- 12-congress pipeline (2012-2023), 1070 oral abstracts.
- 6-strategy PubMed search + 4 supplementary databases + DOI-chain reverse citations.
- 10-component composite scoring with Cochrane MR000005 5-tier classification.
- NPI matching via ABOG pool (60,846 board-certified OB/GYNs).
- Shiny adjudication app with Google Sheets backend.
- Full manuscript + technical appendix with inline R code.
