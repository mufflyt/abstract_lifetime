# Cumulative Test Ledger

Repo: abstract_lifetime. Started 2026-09-03.

## Cycle 0 (baseline, pre-loop)

Written before the loop began, in response to defects found by inspection.

**R/utils_decisions.R** (new; logic extracted from 06_analyze_results.R so it is
testable): `dedup_decisions_for_analysis()`, `assign_final_published()`,
`publication_rate_summary()`.

**tests/testthat/test-decision_precedence_bva.R** (50 assertions)
BVA on human/AUTO precedence across the timestamp boundary, the
`final_published` cascade branch by branch, denominator arithmetic and its
boundaries, plus invariants on the shipped outputs.

**tests/testthat/test-decision_mutation.R** (18 assertions, 8 mutants)
M1 timestamp-only precedence; M2 AUTO excluded outright; M3 oldest wins;
M4 no deduplication; M5 `excluded` dropped from the FALSE branch; M6 skip
treated as no_match; M7 rate divided by cohort; M8 pending counted as
evaluated. All killed.

### Defects found and fixed before the loop
1. `06_analyze_results.R:29` ranked AUTO against human decisions by timestamp.
   Dormant only because the AUTO pass ended 2026-04-17 and human review ran to
   2026-04-27. Simulating a re-run showed it would discard 489 of 533 human
   adjudications and move the rate from 16.9% to 14.1%. Fixed by dropping AUTO
   only where a human decision exists, matching app.R:310-315.
2. First attempt at that fix (excluding AUTO outright) stranded 93
   probable/possible abstracts at NA and inflated the rate to 18.5%. Caught by
   comparing full outputs, not by the unit test. Now mutant M2.
3. `aim1_publication_rate.csv` exported `total_abstracts` next to a rate
   computed on a different denominator. Added `n_evaluated` to the export.

### Open scientific decisions (do NOT silently resolve)
- **Branch order in `final_published`.** `classification == "definite"` is
  evaluated before any reviewer branch, so a definite match records TRUE even
  where a human answered `no_match` (4 abstracts) or `skip` (44 abstracts).
  Tests assert the CURRENT order so a change is visible. Whether human
  adjudication should outrank algorithmic classification is unresolved.
- **The 55 unresolved abstracts.** Dropped from the denominator (1,051) rather
  than counted as unpublished (1,106). Rate ranges 16.1% to 17.2% across the
  four defensible combinations.
- **Pre-congress publication window.** Six-month boundary is an assumption;
  AAGL submission deadlines are not recorded in config.yml.

### Known coverage gap (not a code defect)
Human review covers 2012 (9 of 95) and 2018-2023 only. Congress years 2013,
2014, 2015, 2016 and 2017 have ZERO human adjudication; 528 of 1,106 abstracts
reviewed overall. Year-over-year rate comparisons are confounded by this.

### Pre-existing suite state at baseline
Recorded at cycle 1 inventory.

---

## CI wiring (pre-cycle-1)

`.github/workflows/tests.yaml` added. Runs on every push and PR on ANY branch,
nightly at 08:00 UTC (02:00 America/Denver), and on manual dispatch.

Three ordered gates:
1. `test-decision_precedence_bva.R` — decision precedence and denominator
   contracts. Fails the job before the slower suite runs.
2. `test-decision_mutation.R` — every planted defect must be killed. A surviving
   mutant fails with an explicit instruction not to weaken the mutant.
3. Full `test_dir()`, printing the failing file and test names.

Why nightly matters here: this repository commits its analytical outputs, and
the BVA file asserts invariants against those committed files. A nightly run
catches an output tree that has drifted out of agreement with the code that
produced it, which is the failure mode behind the 1,067 / 1,012 / 1,051
confusion.

CONSTRAINT: GitHub fires `schedule` only on the repository default branch. The
nightly does not start until this workflow is merged to master. Push and PR
triggers work immediately on the feature branch.

The existing `R-CMD-check.yaml` is left in place; it triggers only on main and
master and does not cover feature branches.

## Pre-existing failure recorded at CI wiring (NOT introduced by this work)

`test-shiny_app.R:447` "bundle abstracts_cleaned.csv is not stale vs main copy"
fails locally. Neither file it compares is in this branch's diff. Last touched
by 4e7da03.

**The test's mechanism is wrong.** It compares filesystem mtimes with a 24-hour
tolerance. mtimes are reset to checkout time by actions/checkout, so this test
will PASS in CI regardless of content. It is simultaneously failing locally for
a superficial reason and unable to detect the real problem in CI. A content
hash is the correct assertion. Not changed here: the fix would make CI red on
its first run, which is a decision for the maintainer.

**What the two files actually differ on.** Same 1,106 abstracts, same ids, same
49 columns, same session types. One column differs:

    affiliation_raw    583 of 1,106 rows

The April bundle has affiliations; the September main copy has NA. The bundle is
not stale, it is MORE complete. The 2026-09-01 re-run (e288259, "re-run 05-08
with demographics restored") appears to have dropped affiliation text for 583
abstracts.

**Possible consequence.** Affiliation drives practice_type, subspecialty, state
and ACOG district. In output/final_analytical_dataset.csv these are now:

    practice_type                82.5% missing
    subspecialty                 83.8% missing
    first_author_state           90.2% missing
    first_author_acog_district   82.2% missing
    first_author_country         81.2% missing

The draft abstract reports practice type and geography as NON-significant
predictors of publication. A null result on a variable that is 82-90% missing is
underpowered rather than null. Whether restoring affiliation_raw changes those
conclusions has not been tested.

STATUS: unresolved, handed to maintainer. Not a code defect introduced here.

---

## Cycle 1 — 2026-09-03 21:40 MDT

Mix required: 4 BVA / 3 semantic / 3 adversarial. File:
`tests/testthat/test-cycle01_thresholds_contracts.R` (59 assertions).

| # | Category | Target | Assumption challenged | Not covered before because |
|---|---|---|---|---|
| 1.1 | BVA | `classify_match()` | cutoffs inclusive at 7 and 3, exclusive just below; definite tier unreachable without text evidence at any score | cycle 0 tested the decision cascade, never the score tiers; test-utils_classify.R covers study-design classification, a different function |
| 1.2 | BVA | `classify_match()` | Inf / -Inf / NA scores must not classify as a match | no non-finite score case existed anywhere |
| 1.3 | BVA | `publication_rate_summary()` | cohort of size 1 and size 0 | cycle 0 tested zero-pending and all-pending, not minimum cohort size |
| 1.4 | BVA | `publication_rate_summary()` | returns an unrounded proportion so export controls precision | rounding was only asserted downstream at the export |
| 1.5 | semantic | `classify_match()` | `pre_conference` dominates score; it is a validity statement, not a penalty | the flag's precedence over a perfect score was untested |
| 1.6 | semantic | `conference_date_for()` | Date class preserved, length equals input, integer and character years agree, length-0 in gives length-0 out | existing file tests values and NA fallback, never class, length, or type equivalence |
| 1.7 | semantic | `publication_rate_summary()` | pending means unresolved, not unpublished; FALSE is evaluated | mutant M8 covered miscounting, not the label/quantity distinction |
| 1.8 | adversarial | `congress_date_lookup()` | duplicate congress years in config must not silently yield one of two conflicting dates | no malformed-config case existed |
| 1.9 | adversarial | `dedup_decisions_for_analysis()` | result invariant to input row order across 8 shuffles | cycle 0 fixtures were always in a fixed order |
| 1.10 | adversarial | config vs cohort vintage | every cohort congress year has a config date, else it silently falls back to the legacy date and corrupts months_to_pub for that year | no test compared config vintage to shipped data |

**Result:** 10/10 pass on first run. No implementation defects found this cycle.

**Suite after cycle 1:** 15 files, 519 passed (+59), 1 failed, 0 errors, 0 skipped.
The single failure is the pre-existing `test-shiny_app.R` mtime check recorded
above. No new regressions.

**CI:** `tests` workflow green on all three gates
(run 33808813998). Worth noting: the full-suite gate passed in CI while the same
suite fails locally, which confirms the mtime staleness test is inert in CI
because actions/checkout resets mtimes. That test still cannot detect the real
`affiliation_raw` divergence.

**Unresolved from cycle 0:** branch order in `final_published`; treatment of the
55 unresolved abstracts; pre-congress window; `affiliation_raw` loss.

---

## Cycle 2 — 2026-09-03 22:15 MDT

Mix required: 3 BVA / 4 semantic / 3 adversarial. File:
`tests/testthat/test-cycle02_survival_estimand.R` (25 assertions).

| # | Category | Target | Assumption challenged | Not covered before because |
|---|---|---|---|---|
| 2.1 | BVA | KM set construction, `06:187` | `filter(time > 0)` is exclusive, so an abstract published ON its congress date is dropped without being censored or counted | no test touched the survival set |
| 2.2 | BVA | `aim1_by_congress_year` | per-year n must partition the global denominator exactly | cycles 0-1 tested the global denominator only |
| 2.3 | BVA | censoring window | follow-up strictly decreases with congress year and every congress precedes the search end date | censoring was untested |
| 2.4 | semantic | `aim2_time_to_pub` | the label describes published abstracts; the quantity is computed only on those with a usable interval | population/label mismatch never asserted |
| 2.5 | semantic | `aim1_by_congress_year` | each year's rate divides by that year's n, not the global denominator | per-year estimand untested |
| 2.6 | semantic | `aim2_time_to_pub` | quartiles ordered, units are months not days or years | unit correctness untested |
| 2.7 | semantic | `months_to_pub` | positive means AFTER the congress; a negative interval must never be a counted event | sign convention untested |
| 2.8 | adversarial | `assign_final_published()` | duplicate abstract_ids must not multiply cohort rows | cycle 0 always passed deduplicated input |
| 2.9 | adversarial | `aim1_by_congress_year` | no cohort year may vanish from the by-year table | sparse-year case untested |
| 2.10 | adversarial | by-year summarisation | a zero-publication year yields rate 0 rather than disappearing | empty-subset case untested |

### Defect found and fixed
**2.8 — `assign_final_published()` silently inflated the cohort.** Passing a
decisions table with duplicate `abstract_id` turned a 2-row cohort into 3 rows.
The left join is one-to-one by contract and had no guard; the comment in
`06_analyze_results.R` even names this failure mode as the reason dedup exists,
but nothing enforced it. Added a duplicate-id check that stops with an
instruction to call `dedup_decisions_for_analysis()` first.

Not currently triggered in production because the pipeline always dedups, but
the function is callable directly and the hazard was real. Smallest defensible
change: validation only, no behaviour change on valid input.
`final_analytical_dataset.csv` is byte-identical after re-running 06.

### Observation for the manuscript (not a code defect)
2.4 confirms `n_with_dates` = 104 against `published` = 178. The reported median
time to publication of 13.8 months is computed on **58% of the published set**;
the other 74 publications have no resolvable interval. The draft abstract states
the median without noting the subset. This compounds the date-granularity issue
recorded in the technical appendix A13.6.

**Result:** 9/10 pass on first run, 1 real defect found and fixed, 10/10 after.

**Suite after cycle 2:** 16 files, 544 passed (+25), 1 failed, 0 errors.
Same pre-existing `test-shiny_app.R` mtime failure. No new regressions.
Related files re-run green: BVA 50, mutation 18, cycle01 59.

---

## Cycle 3 — 2026-09-03 22:25 MDT

Mix required: 3 BVA / 3 semantic / 4 adversarial. File:
`tests/testthat/test-cycle03_model_contracts.R` (27 assertions).

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 3.1 | BVA | aim3 / aim2b | ratios positive, finite, bracketed by their own CI |
| 3.2 | BVA | aim3 | no term reported with an uninterpretably wide interval |
| 3.3 | BVA | aim3 / aim2b | p-values in [0,1]; a printed 0 means below precision |
| 3.4 | semantic | aim3 | estimates are exponentiated ratios, not log-odds |
| 3.5 | semantic | aim3 | an effect table must carry the N it was fitted on |
| 3.6 | semantic | aim3 vs aim2b | shared predictors agree in direction |
| 3.7 | adversarial | model spec | no term from a variable >=50% missing |
| 3.8 | adversarial | complete-case | attrition does not quietly halve the model cohort |
| 3.9 | adversarial | glm | determinism across seeds and row order |
| 3.10 | adversarial | artifacts | model outputs not older than the dataset |

### Defect found and fixed
**3.5 — `aim3_logistic_regression.csv` reported odds ratios with no N.** The
model is complete-case, so its sample is smaller than the publication-rate
denominator and could not be recovered from the file. Added `n_obs` to the
export. It is **1,010**, against a denominator of 1,051: 41 abstracts drop out
of the model through complete-case deletion and this was previously invisible.

### Test defect found in my own work
3.5 initially passed for the wrong reason: the regex `^n_` matched the
*predictor* `n_authors` rather than a sample-size column. Corrected to require a
named column, after which it failed correctly. Recorded because a test that
passes spuriously is worse than no test.

3.9's first fixture created perfect separation, so `glm` did not converge and
the determinism claim was being tested on a degenerate fit. Rebuilt with
overlapping groups.

### PRESERVED FAILING TEST — decision required (3.2)
`has_funding` is TRUE for **3 of 1,051** evaluated abstracts: 2 unpublished, 1
published. Its odds ratio of 2.609 (0.117 to 29.04) spans 248-fold and is
estimated from a single event.

The draft abstract states that "the presence of declared funding" was not a
statistically significant predictor. **Not significant and not estimable are
different claims.** The decision needed is either to drop the term from the
model specification, or to report it explicitly as not estimable. Both change
what the manuscript may say, so neither is taken here. The test fails until
someone decides.

### Manuscript discrepancy found while reading the model output
The April draft abstract reports multicenter status as significant:
OR 2.23, 95% CI 1.01-4.64, p=0.038. The current model gives
**OR 1.884, 95% CI 0.861-3.881, p=0.096** — no longer significant. RCT has also
moved (2.48 to 2.244) though it remains significant. Team size is unchanged.
The abstract's multicenter claim does not survive the current data.

**Result:** 8/10 pass on first run. One real defect fixed, two test defects of
my own corrected, one failure preserved by design.

**Suite after cycle 3:** 17 files, 570 passed (+26), 2 failed, 0 errors.
Failure 1 is the preserved 3.2 above. Failure 2 is the pre-existing
`test-shiny_app.R` mtime check. No unintended regressions.

**CI IS NOW RED.** This is intentional and follows the protocol's instruction to
preserve failing tests that represent genuine scientific ambiguity rather than
silently choosing an estimand. It will stay red until the `has_funding` decision
is made.

---

## Cycle 4 — 2026-09-03 22:35 MDT

Mix required: 4 BVA / 3 semantic / 3 adversarial. File:
`tests/testthat/test-cycle04_validation_sensitivity.R` (29 assertions).
First tests ever written against validation_metrics, sensitivity_analyses,
interrater_agreement, aim4_strategy_performance and aim5_publication_bias.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 4.1 | BVA | validation_metrics | confusion cells partition their population; accuracy divides by it |
| 4.2 | BVA | sensitivity_analyses | each scenario recomputes from its own counts |
| 4.3 | BVA | interrater_agreement | agreement bounded; population no larger than the cohort |
| 4.4 | BVA | aim4 | yield percentages recompute from their own counts |
| 4.5 | semantic | sensitivity_analyses | every scenario exposes its denominator; none exceeds the cohort |
| 4.6 | semantic | aim5 | OR direction agrees with the underlying rates |
| 4.7 | semantic | validation_metrics | the sample is drawn from the human-reviewed population |
| 4.8 | adversarial | interrater_agreement | kappa is reported, not silently absent |
| 4.9 | adversarial | aim4 | no strategy silently contributes nothing |
| 4.10 | adversarial | sensitivity_analyses | longer follow-up windows cannot grow the cohort |

### Defect 1 fixed — accuracy divided by the wrong denominator
`R/validation_gold_standard.R` counts the four confusion cells with
`na.rm = TRUE`, so a row with NA in `truth` or `predicted` is dropped from all
four. `n` was still `nrow(validation)`. The cells summed to 49 while n said 50,
and `accuracy <- (tp + tn) / nrow(validation)` mixed a numerator measured on 49
rows with a denominator of 50.

Fixed: `n_classified` is now computed and exported, and accuracy divides by it.
**Accuracy moves from 0.720 to 0.735.** This is a bug fix, not an estimand
change: the numerator and denominator now describe the same rows.

### Defect 2 fixed — Cohen's kappa was silently absent
`R/10_interrater.R` guards the kappa computation with
`requireNamespace("irr", quietly = TRUE)`. The package was not installed, so
`cohens_kappa` was written as NA with no indication why. A reader sees 98.1%
raw agreement on 519 abstracts and an absent kappa, and cannot tell whether
kappa was undefined for the data or never attempted.

Installed `irr` and added it to the CI extra-packages so the metric cannot go
missing again. **Cohen's kappa is 0.994** (p = 0) on 519 multi-reviewed
abstracts. Raw agreement alone overstates reliability when one category
dominates, which is why Cochrane MR000005 asks for kappa; it is now reported.

### Test defects of my own, corrected
- **4.1** asserted the cells sum to `n`. After adding `n_classified` the correct
  assertion is against that, plus `n_classified <= n` and the accuracy identity.
- **4.5** demanded a single denominator across all match-definition scenarios.
  That premise was wrong. "Definite only" is decidable for all 1,106 because
  classification is always present; "Definite + reviewer-confirmed" needs a
  reviewer and is decidable only for the 1,051 evaluated. The denominators
  differ for a principled reason and the table exposes both. Rewritten to assert
  that no scenario exceeds the cohort and that the reviewer-confirmed scenario
  matches the evaluated denominator.

### PRESERVED FAILING TEST — tracks an un-executed fix (4.9)
The `title` search strategy searched 1,742 abstracts and hit **3** (0.2% yield).
Technical appendix A12.4 records that stopword removal broke the title phrase
search; the fix was written 2026-04-28 while the candidate pool was last
retrieved 2026-04-19. This test is the cheapest available signal that the
re-run has happened, and goes green when it does. Not a defect to fix in code.

**Result:** 6/10 pass on first run. Two real defects found and fixed, two of my
own tests corrected, one failure preserved as a re-run tracker.

**Suite after cycle 4:** 19 files, 627 passed (+57), 3 failed, 0 errors.
Failures: 3.2 (funding term, decision required), 4.9 (re-run tracker),
shiny mtime (pre-existing). No unintended regressions.

---

## Cycle 5 — 2026-09-03 22:45 MDT

Mix required: 3 BVA / 4 semantic / 3 adversarial. File:
`tests/testthat/test-cycle05_flow_fidelity_tables.R` (28 assertions).

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 5.1 | BVA | figure1_flow_data | classification tiers partition the cohort |
| 5.2 | BVA | fidelity_checks | title_jaccard bounded; unchanged titles not scored as dissimilar |
| 5.3 | BVA | figure1_flow_data | searched splits exactly into with/without candidates |
| 5.4 | semantic | figure1_flow_data | a step label names the quantity beneath it |
| 5.5 | semantic | gender fields | a conflict flag requires two or more sources |
| 5.6 | semantic | output/tables | a derived table agrees with the analysis it derives from |
| 5.7 | semantic | R/strobe_flowchart.R | the stopifnot guards actually reject bad arithmetic |
| 5.8 | adversarial | config.yml | scoring thresholds ordered so the tiers cannot invert |
| 5.9 | adversarial | R/strobe_flowchart.R | no hardcoded cohort numbers |
| 5.10 | adversarial | fidelity_checks | one row per abstract, valid classifications only |

### Defect fixed — a flow-diagram step did not name what it counted
`figure1_flow_data.csv` reported step "No match" as **713**, while
`classification == "no_match"` is **709**. The extra 4 are the `no_candidates`
abstracts, which the figure already lists as their own step above. A reader
summing the labelled steps counts those 4 twice.

Worse, the DiagrammeR node labelled the same box "No match (score < 3)" — but
the four no-candidate abstracts never had a score to fall below 3.

Renamed the step and the graph node to "No match or no candidates" in
`R/08_make_figures.R` and regenerated. Counts are unchanged; only the labels
were wrong. The tiers still partition the cohort exactly (131 + 81 + 142 + 39 +
713 = 1,106), which is why test 5.1 passed while 5.4 failed.

### Self-coverage note
5.7 and 5.9 test `R/strobe_flowchart.R`, written earlier in this session. 5.9
strips comments and string literals before scanning for cohort literals, so the
explanatory comments naming 1,154 and 1,051 do not mask a genuine hardcoded
number. Both pass: the guards reject all three inconsistent-arithmetic cases,
and no cohort number appears as a literal.

**Result:** 9/10 pass on first run, 1 real defect found and fixed, 10/10 after.

**Suite after cycle 5:** 20 files, 655 passed (+28), 3 failed, 0 errors.
Failures unchanged: 3.2 (funding term, decision required), 4.9 (re-run tracker),
shiny mtime (pre-existing). No unintended regressions.

---

## Cycle 6 — 2026-09-03 22:55 MDT

Mix required: 3 BVA / 3 semantic / 4 adversarial. File:
`tests/testthat/test-cycle06_scoring_composite.R` (32 assertions).
Target: the composite score itself. Cycle 1 tested the tiers built on top of the
score; nothing had tested the score, its components, or the uniqueness of what
it selects.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 6.1 | BVA | best_score | equals the sum of its ten components |
| 6.2 | BVA | components | each stays inside its documented range |
| 6.3 | BVA | title_sim | bounded; zero similarity earns no title points |
| 6.4 | semantic | tie rule | a tied best candidate is never left definite |
| 6.5 | semantic | components | every component can actually contribute |
| 6.6 | semantic | no_text_penalty | non-positive, and bars the definite tier |
| 6.7 | adversarial | final_pmid | one publication is not two abstracts' conversion |
| 6.8 | adversarial | n_candidates | zero candidates cannot carry a match |
| 6.9 | adversarial | match tiers | a match tier carries the PMID it matched |
| 6.10 | adversarial | components | no NA that rowSums would swallow |

### Invariants confirmed (worth locking in)
`best_score` equals the sum of its ten components on **1,106 of 1,106** rows.
Every component stays inside its range. The tie-demotion rule at
`utils_scoring.R:481` works: zero definite classifications carry `has_tie`.

### PRESERVED FAILING TEST 6.5 — a scoring component is structurally dead
`keyword_pts` is **0 for all 1,106 abstracts**. The scorer guards the block with
`!is.null(abstract$keywords)`, and the cleaned abstracts carry **no keywords
column at all**, so the branch is unreachable. The manuscript describes a
"10-component composite scoring system"; one of the ten cannot fire.

Decision needed: remove the component, source keywords for the abstracts, or
describe the composite as nine components. All three change either the score or
the methods text, so none is taken here.

### PRESERVED FAILING TEST 6.7 — three publications double-counted
Three PMIDs are each claimed by two abstracts counted as published, so the
numerator of 178 carries **three duplicate credits**. PMIDs 32604198, 38906210,
39490893.

The clearest example: AAGL2019_036 "Occult Uterine Malignancy at the Time of
Surgery for Pelvic Organ Prolapse: A Systematic Review" and AAGL2019_081
"Occult Uterine Malignancy at the Time of Surgery for Benign Gynecologic
Indications: An Updated Systematic Review" both matched PMID 32604198. These are
different studies; at most one is the publication of that paper.

Decision needed: adjudicate which abstract owns each PMID. If all three
duplicates resolve to one abstract each, the numerator falls from 178 to 175 and
the rate from 16.9% to 16.7%.

### Test defects of my own, corrected
- **6.10** flagged NA components on the four `no_candidates` abstracts. Those
  rows correctly have no components because nothing was scored. Rescoped to
  abstracts that have a candidate.
- **6.5 and 6.7** each reported twice, using `expect_*` followed by `fail()`.
  Collapsed to one assertion apiece so the failure count reflects findings.

**Result:** 7/10 pass on first run. Two findings preserved as failing tests, one
test of my own rescoped, two collapsed.

**Suite after cycle 6:** 21 files, 685 passed (+30), 5 failed, 0 errors.
Failures: 3.2 funding term, 4.9 re-run tracker, 6.5 dead component, 6.7
duplicate PMIDs, shiny mtime (pre-existing). Four are open decisions; none is a
regression.

---

## Cycle 7 — 2026-09-03 23:05 MDT

Mix required: 4 BVA / 3 semantic / 3 adversarial. File:
`tests/testthat/test-cycle07_manuscript_consistency.R` (19 assertions).
Target: agreement between the manuscript prose and the artifacts it describes.
Cycles 0-6 tested the pipeline; nothing had checked that the sentences in docs/
still describe the numbers in output/.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 7.1 | BVA | both cohort artifacts | same rows, ids and classification distribution |
| 7.2 | BVA | prose video count | the hardcoded n=48 matches the data |
| 7.3 | BVA | prose ordinals | "41st through 52nd" matches config |
| 7.4 | BVA | prose windows | 12/24/36/48 months exist as scenarios |
| 7.5 | semantic | time-to-pub sentence | the stated population is the real one |
| 7.6 | semantic | prose video years | "only in 2022-2023" is true of the data |
| 7.7 | semantic | n_total | the reported cohort is the cohort on disk |
| 7.8 | adversarial | both Rmds | every file read actually exists |
| 7.9 | adversarial | study_design | counts stay inside the cohort |
| 7.10 | adversarial | technical appendix | A13 still derives from the stage files |

### Defect fixed — the results section named the wrong population
Line 194 read "**Among definite matches**, the median time from conference
presentation to full publication was 13.8 months". `med_ttp` is read from
`aim2_time_to_pub.csv`, which cycle 2 established is computed over every
published abstract carrying a usable interval, **not** over definite matches.
The test measured the gap exactly: the median covers **104** abstracts while
definite-with-a-date is **89**.

Rewritten to name its real population and expose the subset:

> Among published abstracts with a resolvable publication date (n = 104 of 178),
> the median time from conference presentation to full publication was 13.8
> months (IQR 6.3-25).

This also surfaces in the manuscript the caveat cycle 2 recorded: the median
rests on 58% of the published set. Document re-knits cleanly.

### Regression I introduced and fixed in the same cycle
The first edit referenced `n_published_total` without defining it, which would
have broken the knit. Caught by rendering the document rather than by a test,
because **no test asserts the Rmd files knit**. Defined the variable from
`aim1_publication_rate.csv` and confirmed the render. Added to the cycle 8
target list: a test that both Rmd documents knit.

### Drift guards now in place
7.2, 7.3, 7.4 and 7.6 lock the manuscript's hardcoded claims (48 videos, 41st
through 52nd, the four follow-up windows, videos only in 2022-2023) against the
data. All four are currently true; they will fail if either side moves.

**Result:** 9/10 pass on first run, 1 real defect found and fixed, 10/10 after.

**Suite after cycle 7:** 22 files, 704 passed (+19), 5 failed, 0 errors.
Failures unchanged from cycle 6: four open decisions and one pre-existing.

---

## Cycle 8 — 2026-09-03 23:15 MDT

Mix required: 3 BVA / 4 semantic / 3 adversarial. File:
`tests/testthat/test-cycle08_reproducibility_contracts.R` (30 assertions).
Targets: identifier contracts, pipeline dependency order, document rendering,
environment independence, artifact vintage, Shiny bundle schema.

**Result:** 10/10 pass. No implementation defects. Three defects in my own tests
were found and corrected:

- **8.5** asserted the runner sources stages in ascending numeric order. Wrong
  premise: the 09* and 10* enrichment stages deliberately run before
  `06_analyze_results`, because the analysis consumes the demographics they
  produce. Rewritten to assert the seven dependency relationships that actually
  hold.
- **8.6** guarded the knit test with `Sys.which("pandoc")`, which misses the copy
  rmarkdown bundles. The test skipped on a machine where rendering demonstrably
  works. A permanently skipping test is a hole, not a safeguard. Now uses
  `rmarkdown::pandoc_available()` and runs.
- **8.7** shelled out to `06_analyze_results.R` and skipped whenever that call
  failed, which is the same hole. Rewritten as a property test: the transform is
  a pure function of its inputs, so it must be idempotent and invariant to row
  order. Always runs.

Also re-rendered `docs/technical_appendix.docx`, which predated its source by 91
minutes and therefore reported pre-cycle-4 numbers.

---

# LOOP STOPPED AFTER CYCLE 8

## Reason 1: concurrent agent in the same working tree

A test file I did not write, `tests/testthat/test-remediation_invariants.R`,
appeared at 17:06 between cycles 7 and 8, alongside `docs/FAILURE_MODES.md`,
`docs/SOURCE_OF_TRUTH.md`, `scripts/rebuild_candidate_pool.R` and edits to
`00_run_all.R`, `config.yml` and several `R/` stages.

**My `git add -A` swept 26 files I did not author into three of my commits**
(`0bd4541`, `238651e`, `c50f3de`), all already pushed. Those commit messages
describe only my test work and therefore misrepresent the contents. This is my
error: `add -A` is unsafe in a tree I do not have exclusively. Not unpicked;
rewriting pushed history would destroy the other agent's work.

## Reason 2: the cohort is truncated at ingestion

Verified independently against Crossref, then documented in technical appendix
**A14**. Every congress captures 93-100 presentations, 2022 captures exactly
100, and each capture is a contiguous prefix from page S1 stopping between S26
and S60 while the supplements run to S141-S286. The 2012 cutoff falls mid-block:
the records immediately after it are ordinary abstracts of the same kind already
in the cohort.

The captured cohort is a contiguous prefix of each supplement, not a sample of
it. Every number validated across cycles 0-8 is arithmetically correct for the
1,106 abstracts captured and does not describe "all oral presentations at the
AAGL Global Congress, 2012-2023".

Per the user's instruction, this is documented and then set aside. Remediation
is tracked in `docs/FAILURE_MODES.md`, not here.

## Loop totals, cycles 1-8

- 80 tests added across 8 files; 30 BVA, 27 semantic, 23 adversarial
- 7 real defects found and fixed
- 4 findings preserved as failing tests pending a decision
- 8 defects in my own tests found and corrected
- Suite: 743 passing at cycle 8

---

# LOOP RESTARTED 2026-09-04

State on restart had moved. The concurrent agent added an expected-failure gate
(`tests/expected_failures.yaml` + `tests/run_suite_gate.R`), six test files
(docs drift, model stability, remediation invariants, gender NPPES tier,
mysterycall integrations, shiny bundle currency), and adopted
`mufflyt/mysterycall` as a pinned dependency in CI. Suite was 28 files / 907
passing / 4 expected failures, gate green.

Two of my four preserved failures had resolved on their own after their
candidate-pool repair (Cox events 104 -> 171): 3.2 `has_funding` now passes, and
the shiny mtime test was replaced by a proper byte-identical manifest check. The
remaining two of mine (4.9 search-strategy yield, 6.5 dead component, 6.7
duplicate PMIDs) are now registered on the manifest.

**Cohort unchanged: still 1,106, still 93-100 per congress.** The truncation in
A14 has not been remediated. Documented and set aside per instruction.

Process change: stage file-by-file. `git add -A` is what pulled 26 of the other
agent's files into three of my commits.

## Cycle 9 - 2026-09-04

Mix required: 3 BVA / 3 semantic / 4 adversarial. File:
`tests/testthat/test-cycle09_encoding_locale_seed.R` (19 assertions).
Ground chosen to avoid both cycles 1-8 and the concurrent suite: encoding,
locale, seeding, timestamp ambiguity, CSV round-trip fidelity. These are the
failures that reproduce differently on another machine rather than failing here.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 9.1 | BVA | pubmed$date_end | the censor date sits beyond every congress, with real follow-up |
| 9.2 | BVA | CSV round trip | scores survive write/read at tolerance 0 |
| 9.3 | BVA | encoding | non-ASCII text is preserved, no mojibake signatures |
| 9.4 | semantic | config seed | a seed is declared AND the runner calls set.seed() |
| 9.5 | semantic | review_timestamp | parses to one instant and carries a timezone |
| 9.6 | semantic | candidate pools | rows reference known abstracts; cohort orphans are explained |
| 9.7 | adversarial | collation | ordering is stable between C and native locale |
| 9.8 | adversarial | column types | numeric columns are not read back as character |
| 9.9 | adversarial | candidate pools | no source record listed twice for one abstract |
| 9.10 | adversarial | shipped CSVs | no BOM to rename the first column |

**Result:** 8/10 pass on first run. No implementation defects. Two premises of
my own were wrong and were corrected:

- **9.6** required every candidate to map into the CLEANED cohort. Wrong: the
  searches run against the parsed set, so candidates legitimately survive for
  abstracts the video filter later removes. All 145 orphans across five pools
  are Video abstracts. Rewritten to assert no candidate references an id the
  pipeline has never seen, and that every cohort orphan is explained by the
  documented exclusion.
- **9.9** keyed duplicates on (abstract_id, pmid, doi). That is not a key for
  the OpenAlex and Semantic Scholar pools, where most records carry neither;
  it flagged 2,159 "duplicates" in one file that were distinct works with
  distinct oa_id/s2_id, titles and authors. Rewritten to key on the source's own
  identifier.

Both corrections matter beyond this cycle: they are the same error, assuming a
key without checking it holds.

**Suite after cycle 9:** 29 files, 926 passed (+19), 4 failed, all on the
manifest. Gate green.

## Cycle 10 - 2026-09-04

Mix required: 4 BVA / 3 semantic / 3 adversarial. File:
`tests/testthat/test-cycle10_score_component_rules.R` (33 assertions).

Cycle 6 tested the composite score as a SUM: components in range, parts adding
to the total, no dead NA. It never tested the RULES that produce each component.
This cycle drives `score_match()` directly at each configured threshold, which
is where an off-by-one in a comparison operator lives.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 10.1 | BVA | title_points | steps at the Jaccard cutoffs, inclusive from above |
| 10.2 | BVA | date_points | sign switches exactly at the congress date |
| 10.3 | BVA | date_points | steps down at the early (18mo) and late (30mo) cutoffs |
| 10.4 | BVA | no_text_penalty | fires only with neither title nor abstract evidence |
| 10.5 | semantic | cross-stage | scoring and survival agree on "published on the day" |
| 10.6 | semantic | total | equals the components the same call reports |
| 10.7 | semantic | journal_points | rewards an in-scope journal over an unrelated one |
| 10.8 | adversarial | determinism | same pair scored twice is identical |
| 10.9 | adversarial | sparse candidate | missing date/journal/authors degrades, never crashes |
| 10.10 | adversarial | empty title | earns no title credit |

**Result: 10/10 pass. No implementation defects, and no defects in my own tests
this cycle** (the first cycle with neither).

### Recorded, not failing: two stages disagree on a same-day publication
`utils_scoring.R` tests `months_diff < 0`, strictly, so a candidate published ON
the congress date is treated as post-conference and earns the full early-window
point (verified by 10.2). `06_analyze_results.R` builds the survival set with
`filter(time > 0)`, which is exclusive and drops `months_to_pub == 0` without
censoring it (cycle 2, test 2.1).

The scorer would credit such an abstract; the survival stage would silently
discard it. No such abstract exists today, so 10.5 asserts the scorer's side and
leans on 2.1 to assert the survival side currently has nothing to drop. Both go
red together if one ever appears. Not fixed here: choosing which stage is right
is a methodological call.

### Confirmed by 10.9
An unparseable publication date scores `date_points = 0`, not the -3
pre-conference penalty. That is the correct behaviour and is now locked: the
alternative would push legitimate candidates below the match threshold purely
for having a coarse date, which is the same date-granularity trap recorded in
technical appendix A13.6.

**Suite after cycle 10:** 30 files, 959 passed (+33).

### Gate red from concurrent work, not from this cycle
The gate reports `test-pipeline_semantics.R :: PH assumption holds` as
UNEXPECTED. That is the concurrent agent's in-flight work, verified as such:
they have uncommitted edits to `R/06_analyze_results.R`,
`tests/testthat/test-pipeline_semantics.R` and `tests/expected_failures.yaml`
(the PH entry removed, which is what the manifest's own rule requires once a
decision is taken), plus new stratified and time-varying Cox artifacts. They are
implementing the fix the entry was waiting on.

Category (d) under the protocol: pre-existing, external, unrelated to this
cycle. Not touched, and deliberately NOT re-added to the manifest, since that
would undo their removal. All three of my registered entries still behave.

---

## Commit pollution: resolved by annotation, not rewrite (2026-09-04)

Three commits carry messages describing only my test work while also containing
files authored by the concurrent agent, because I staged them with
`git add -A` in a tree I did not have exclusively.

**Why history was not rewritten.** All three are ancestors of `origin/main`,
which is the default branch, and four of the other agent's commits sit on top of
them. Rewriting would mean force-pushing `main`, invalidating their clone and
any other checkout. That is a destructive fix for a descriptive problem.

**What was done instead.** A `git notes` annotation is attached to each commit
naming, file by file, which paths belong to the stated work and which were swept
in. Notes are additive, travel with the repository, and appear in `git log`
without altering any commit.

| Commit | Stated work | Files actually mine | Swept in |
|---|---|---|---|
| `0bd4541` | Cycle 3 | 4 | 8 |
| `238651e` | Cycle 6 | 2 | 8 |
| `c50f3de` | Cycle 7 | 4 | 11 |

Two entries matter beyond attribution, because the commit messages give no hint
that the data moved: `238651e` changed `config.yml` and
`data/processed/abstracts_cleaned.csv`, and `c50f3de` changed
`output/abstracts_with_matches.csv`. Anyone bisecting a change in the cohort or
the analytical dataset would not find it from those subject lines.

**Reading the notes.** They are on `refs/notes/commits` and are not fetched by
default:

```
git fetch origin refs/notes/commits:refs/notes/commits
git log --notes 0bd4541 238651e c50f3de
```

**Prevention.** From cycle 9 onward every commit stages named paths. The loop
prompt carries the instruction so it survives a session restart.

## Cycle 11 - 2026-09-04

Mix required: 3 BVA / 4 semantic / 3 adversarial. File:
`tests/testthat/test-cycle11_authors_and_queue.R` (14 assertions).
Targets: author-list truncation and the manual review queue. Neither touched by
cycles 1-10 or by the concurrent remediation suite.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 11.1 | BVA | author_count | zero authors only for withdrawn abstracts |
| 11.2 | BVA | author variables | no hard ceiling with mass piled on it |
| 11.3 | BVA | review queue | every queued abstract is probable, possible or tied |
| 11.4 | semantic | n_authors | the team-size predictor spans a usable range |
| 11.5 | semantic | authors_truncated | the flag survives adjudication |
| 11.6 | semantic | last-author guard | withheld exactly when the list was truncated |
| 11.7 | semantic | review queue | contents match the stated rule |
| 11.8 | adversarial | queue | no_match reaches it only through a tie |
| 11.9 | adversarial | queue | no duplicates, subset of the cohort |
| 11.10 | adversarial | first_author_normalized | present wherever authors parsed |

### Defect fixed - the truncation flag was dropped at adjudication
`02_clean_abstracts.R:54` computes `authors_truncated` by detecting the ellipsis
ScienceDirect inserts, and correctly uses it to suppress last-author credit so a
truncated list cannot award credit to whoever happens to be visible last (11.6
now locks that guard).

`R/05_adjudicate.R:91` then dropped the flag: the select is an explicit column
list and `authors_truncated` was not on it. Nothing downstream could tell a
censored author list from a genuinely short one.

This is the **second** documented failure caused by omission from that same
select. The file's own comment records the first: `result_positivity` was
dropped the same way, which gated off the Aim 5 publication-bias block in
`06_analyze_results.R` and left `aim5_publication_bias.csv` stale since
2026-04-17 (FAILURE_MODES F15).

Added `authors_truncated` to the select and regenerated stage 05. Verified the
regenerated file differs from the committed one by exactly that one column, with
no value changes in any of the other 87. Also confirmed stage 05 is
byte-idempotent across consecutive runs, so the earlier md5 movement was a stale
file on disk rather than non-determinism.

### PENDING REGISTRATION - two findings, manifest not editable right now
`tests/expected_failures.yaml` currently carries the concurrent agent's
uncommitted 13-line deletion (removing the PH entry). Committing the manifest
would take their in-flight change with it, so these two entries are recorded
here and must be added once that edit lands.

**11.2 and 11.4 - the team-size predictor is censored.**

    n_authors     max 5, 532 of 1,106 rows (48.1%) sitting exactly at the cap
    author_count  max 5, 197 of 1,106 rows (17.8%) at the cap

Half the mass on the maximum is the signature of a display cap, not a
distribution. `authors_truncated` confirms the mechanism: the ScienceDirect
listing elides long author lists and the parser counts only what is visible.

This is not a cosmetic problem. `aim3_logistic_regression.csv` reports
`n_authors` at OR 1.325 per author, p < 0.001, and the draft abstract states
that "the likelihood of publication increased significantly with team size
(OR 1.32 per additional author; p<0.001)". That coefficient is estimated over a
variable that cannot exceed 5, with 48% of observations at the boundary. "Per
additional author" has no meaning past the cap.

Unlike the funding term in cycle 3, this one is **significant and reported as a
headline finding**. Decision needed: recover full author lists at ingestion, or
model team size as censored, or restate the claim. All three change what the
manuscript may say.

**Result:** 11/14 assertions passed on the first run, 12/14 after the fix. Two
failures are the finding above.

## Cycle 12 - 2026-09-04

Mix required: 3 BVA / 3 semantic / 4 adversarial. File:
`tests/testthat/test-cycle12_covariate_integrity.R` (14 assertions).

Cycle 11 established that one model covariate, `n_authors`, is censored at a
display cap while being reported as a significant predictor. This cycle asks the
same question of every other term in aim3: is the variable behind the
coefficient what the coefficient claims it is.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 12.1 | BVA | sample_size | positive, finite, integral, safe to log |
| 12.2 | BVA | binary covariates | minority cell large enough to estimate |
| 12.3 | BVA | gender_unified | closed vocabulary, usable coverage |
| 12.4 | semantic | aim3 terms | every term maps to an exported column |
| 12.5 | semantic | is_us_based | agrees with first_author_country |
| 12.6 | semantic | is_rct | agrees with study_design |
| 12.7 | adversarial | missingness | does not track congress year |
| 12.8 | adversarial | sample_size | magnitudes plausible, not parsed years |
| 12.9 | adversarial | attrition | explained by the reported terms |
| 12.10 | adversarial | covariates | none effectively constant |

### THREE FINDINGS - pending registration, manifest still mid-edit
`tests/expected_failures.yaml` still carries the concurrent agent's uncommitted
deletion, so these are recorded here rather than registered.

**12.7 - `sample_size` missingness ranges from 13% to 93% ACROSS CONGRESS YEARS.**

    2013  13.3%     2017  86.7%
    2012  14.9%     2018  92.6%
    2015  16.3%     2014  42.2%

The logistic model deletes rows with any missing covariate and `log_sample_size`
is a term, so 2017 and 2018 are almost entirely absent from the model that
estimates the predictors. 2018 is also the year carrying the highest reported
publication rate in `aim1_by_congress_year.csv` (27.4%). Any predictor estimate
is fitted on a cohort whose congress-year composition is set by data
availability rather than by design. This compounds, independently, the human
review coverage gap already recorded at cycle 0.

**12.4 - `log_sample_size` is a reported model term with no column in the
exported dataset.** It is derived inside `06_analyze_results.R` and never
written out, so nobody holding `final_analytical_dataset.csv` can reproduce,
check or correct the model. Not fixed here because `06_analyze_results.R` is
mid-edit by the concurrent agent.

**12.5 - `first_author_country` contains US states.** 161 of 178 rows with both
fields disagree with `is_us_based`, and the country column holds values like
`"Arizona."`, `"Illinois."`, `"Massachusetts."` alongside `"Canada."` and
`"Italy."`, all with a trailing period. The parser is writing a state into a
country field. `is_us_based` is the term the model actually uses, so the
regression is not directly affected, but any US-versus-international description
drawn from `first_author_country` is wrong.

### Collateral damage I caused and repaired
Cycle 11 added `authors_truncated` to `output/abstracts_with_matches.csv`, which
is a Shiny bundle source. That broke two of the concurrent agent's tests in
`test-shiny_bundle_currency.R`, which assert every bundle file is byte-identical
to its source. Refreshed the bundle with `deploy.R` (deployment is opt-in behind
`SHINY_DEPLOY`, so this only rebuilt `bundle/` and regenerated
`bundle_manifest.csv`). Their 55 assertions pass again. The manifest diff is one
md5, one byte count and the timestamps, which is exactly what a single added
column should produce.

Lesson recorded: changing a pipeline output is not a local act in this
repository. Outputs are committed, bundled and asserted against.

### Test defect of my own
12.2 flagged `has_funding` as too thin to estimate. It is thin, but the
concurrent agent's variable screen has since dropped it from the model, so
flagging it is noise rather than a finding. Scoped the test to covariates that
appear in `aim3_logistic_regression.csv`. Also collapsed five `expect_*` plus
`fail()` pairs that were each reporting one finding twice.

**Result:** 10/17 assertions passed on the first run, 11/14 after correcting my
own test. Three failures are the findings above.

### Fix applied on request: first_author_country contained US states

**Root cause.** `R/09_enrich_authors.R:96`, inside `parse_affiliation()`:

```r
country <- if (length(parts) >= 1) tail(parts, 1) else NA_character_
```

The last comma-delimited token of an affiliation is not the country. US
addresses end in a state, so "Department of OB/GYN, Mayo Clinic, Phoenix,
Arizona." produced `"Arizona."`, trailing period included.

`parse_country()` in `R/utils_states.R:308` already solves this properly: it
resolves US signals to `"USA"` first, then matches a canonical country list, and
returns NA when neither applies. A second, weaker rule had been written beside
it. Replaced the tail-token grab with a `parse_country()` call.

**Correcting the shipped data without a network re-run.** Stage 09 uses
`rentrez` against PubMed and caches no XML, so re-running it would mean ~1,100
network calls and would regenerate data the concurrent agent may be mid-analysis
on. Instead `R/09c_author_characteristics.R` now re-derives the column from the
affiliation text it already holds, using the same canonical parser it already
calls a few lines later for ACOG district.

**Result.**

    before   178 non-NA values, 161 of 178 disagreeing with is_us_based,
             values including "Arizona.", "Illinois.", "Massachusetts."
    after    976 non-NA values in author_characteristics.csv, zero states,
             zero trailing periods
             USA 438, China 61, Italy 51, United Kingdom 44, Canada 42

Coverage rose because the canonical parser recognises country names the
tail-token rule missed, not only because bad values were removed.

**Propagation.** Ran `09c` then `10e_merge_demographics.R`, both local. The
correction now reaches `author_characteristics.csv` (0 states) and
`abstracts_with_matches.csv` (0 states). `final_analytical_dataset.csv` still
carries 18 states because it is written by `06_analyze_results.R`, which is
mid-edit by the concurrent agent and must not be run. Test 12.5 now verifies the
fix at the stage it landed and keeps asserting on the final dataset, so it goes
green on the next full run.

Bundle refreshed again, since `abstracts_with_matches.csv` changed. Their 55
bundle-currency assertions pass.

## Cycle 13 - 2026-09-04

Mix required: 4 BVA / 3 semantic / 3 adversarial. File:
`tests/testthat/test-cycle13_enrichment_quality.R` (15 assertions).
Target: the enrichment layer producing the demographic covariates. Cycle 12
showed one of them was populated by a wrong rule; this cycle asks whether the
others are doing anything at all, or are present-but-inert.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 13.1 | BVA | npi_match_score | non-negative, ordered by its own confidence tier |
| 13.2 | BVA | cited_by_count | non-negative integral count |
| 13.3 | BVA | journal_impact_proxy | non-negative and finite |
| 13.4 | BVA | classifier vocabularies | only documented values, in both artifacts |
| 13.5 | semantic | npi_number | assigned only at high confidence |
| 13.6 | semantic | practice_type | reaches every class its rules document |
| 13.7 | semantic | orcid_false_positive | a live flag, not a constant |
| 13.8 | adversarial | career_stage | resolves for a usable share |
| 13.9 | adversarial | enrichment coverage | does not concentrate by congress year |
| 13.10 | adversarial | enrichment columns | none wholly missing or single-valued |

### The country fix cascaded further than expected
The `community` branch at `utils_affiliation.R:173` is gated on

```r
is_us <- is.na(country) || str_detect(tolower(country), "^usa$|united states|...")
```

With the country field holding `"Arizona."`, `is_us` was FALSE for every US
affiliation, so the branch never fired and US community hospitals were being
labelled academic. Fixing `parse_country()` in the previous step repaired it,
and the effect on the shipped classifiers is large:

    practice_type   193 -> 949 rows classified   community 0 -> 20
                    NA 82.5% -> 14.2%
    acog_district   missing 82.2% -> 11.9%
    subspecialty    missing 83.8% -> 46.7%

`is_academic` is a model covariate. The draft abstract reports practice type as
a non-significant predictor; that was estimated on 17% coverage and there is now
86%. The claim needs re-estimating rather than restating.

### Findings registered on the manifest
`tests/expected_failures.yaml` was clean this cycle, so the backlog from cycles
11-13 was registered properly: ten entries, each with a reason and the decision
it waits on. Two are explicitly labelled STALENESS TRACKERS rather than open
questions (12.5 and 13.6), because the fix is already in and only
`06_analyze_results.R` has yet to re-run. They go red when they start passing,
which is exactly what the manifest's own rule intends.

New findings this cycle:

- **13.4** `career_stage` emits `faculty_senior` while `orcid_career_stage`
  emits `senior_faculty`. Two vocabularies for one concept.
- **13.7 / 13.10** `orcid_false_positive` is FALSE on all 1,102 rows it covers,
  and `orcid_subspecialty` is the constant `"obstetrics"`. Neither carries
  information, but both ship as though they do.
- **13.8** `career_stage` resolves 15 of 1,106 rows (1.4%) even after the
  country fix lifted every other classifier. The input is not the limit.

### Test defects of my own
13.4 originally read only the analytical dataset, which is stale, so the
vocabulary drift was invisible. Extended to check `author_characteristics.csv`
as well, after which it caught it. Also collapsed two more `expect_*` plus
`fail()` pairs that double-reported; that pattern has now appeared in four
cycles and is worth avoiding by default.

**Result:** 10/15 assertions passed. Five failures are the findings above.
**Gate: green — 13 failures, all on the manifest.**

## Cycle 14 - 2026-09-04

Mix required: 3 BVA / 4 semantic / 3 adversarial. File:
`tests/testthat/test-cycle14_text_flags_and_tables.R` (11 assertions).
Target: the binary flags extracted from abstract text, the two text
classifiers, and the derived tables. The concurrent suite's F3 test checks no
text-derived covariate is structurally zero for a whole congress; the angles
here are internal logical consistency between flags, and whether a flag can fire
on an abstract that has no text.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 14.1 | BVA | abstract_word_count | zero only where there is genuinely no abstract |
| 14.2 | BVA | body-only flags | cannot fire without an abstract body |
| 14.3 | BVA | text classifiers | closed vocabularies |
| 14.4 | semantic | stat_sig_reported | implies has_numeric_results |
| 14.5 | semantic | has_trial_registration | concentrated in trial-like designs |
| 14.6 | semantic | is_database_study | carries a database-scale sample |
| 14.7 | semantic | derived tables | reconcile with the cohort |
| 14.8 | adversarial | flag prevalence | not an artefact of abstract length |
| 14.9 | adversarial | text flags | none constant |
| 14.10 | adversarial | primary_procedure | coverage not concentrated by congress |

### FINDING - a quarter of the cohort has no abstract text
280 of 1,106 abstracts (25.3%) carry no text at all: `abstract_text`,
`abstract_objective` and `abstract_conclusion` are all empty. Only 4 are the
withdrawn abstracts. The loss concentrates in two congresses:

    2017   97 of 90 evaluated       2014   38
    2018   95 of 95                 2015   16
                                    2016   13
                                    2012   11
                                    2013   10

Consequences: every text-derived flag on those rows is a false negative rather
than a measurement, `abstract_pts` can never contribute to their match score,
and this is the mechanism behind the `sample_size` missingness recorded at cycle
12 (86.7% in 2017, 92.6% in 2018). Two findings, one root cause.

**Checked and NOT supported.** I expected this to explain the year-over-year
publication rate pattern that has been open since April. It does not. The
correlation between percent-no-text and publication rate is **-0.17**, and the
two fully text-free congresses sit at opposite extremes: 2017 at 5.6%, the
lowest in the study, and 2018 at 27.4%, the highest of the older years. Recorded
because a plausible mechanism that the data does not support is worth knowing
about explicitly, so nobody reaches for it later.

### FINDING - two text extractors disagree
11 abstracts are flagged as reporting statistical significance while also being
flagged as carrying no numeric results. An abstract cannot claim significance
without presenting a number, so at least one extractor is wrong on those rows.

### Three test defects of my own
- **14.2** treated every flag as requiring an abstract body. `is_database_study`
  and `has_industry` can legitimately be read from a title ("... Using the
  National Inpatient Sample"). Narrowed to the four that genuinely need the
  body: significance, numeric results, IRB statement, trial registration.
- **14.3** carried an incomplete vocabulary. `cerclage` and `ectopic_pregnancy`
  are real `primary_procedure` classes; I had guessed the list rather than
  reading it.
- **14.7** bounded every table by the cohort. `table4_search_strategies.csv`
  counts search QUERIES, not abstracts, and the video abstracts were searched
  before exclusion, so 1,742 legitimately exceeds 1,106.

Following the new rule, this cycle used a single `expect_true` with an
informative label throughout rather than `expect_*` plus `fail()`; no failure
double-reported.

**Result:** 9/11 assertions passed after correcting my own premises. Both
remaining failures are registered.
**Gate: green — 15 failures, all on the manifest.**

## Cycle 15 - 2026-09-04

Mix required: 3 BVA / 3 semantic / 4 adversarial. File:
`tests/testthat/test-cycle15_backfill_contract.R` (14 assertions).
Target: `R/02b_backfill_abstract_text.R`, the stage that exists specifically to
repair the gap cycle 14 measured. Its header states the problem: ScienceDirect
paywalls the individual article pages for older supplement issues, so the
scraper could not retrieve abstract text for 2012-2018.

| # | Category | Target | Assumption challenged |
|---|---|---|---|
| 15.1 | BVA | cache key | derivation matches the fetcher exactly |
| 15.2 | BVA | cached files | clear the >100 byte floor the fetcher requires |
| 15.3 | BVA | eligibility | every text-free abstract has the DOI it needs |
| 15.4 | semantic | backfill | fills a row or leaves evidence it tried |
| 15.5 | semantic | re-run safety | rows with text are excluded from the filter |
| 15.6 | semantic | word count | agrees with text presence upstream |
| 15.7 | adversarial | cache | every file belongs to a known key space |
| 15.8 | adversarial | cache | cached XML parses |
| 15.9 | adversarial | abstract_text | no PubMed XML markup leaked through |
| 15.10 | adversarial | text loss | confined to the documented 2012-2018 window |

### FINDING - the backfill stopped at 5.4% of its own workload
All 280 text-free abstracts are eligible: every one has a usable DOI, so none is
permanently unreachable (15.3 passes). Only **15 were ever fetched**. The other
**265** have no cached XML under the DOI-derived key the fetcher uses.

Worth knowing before anyone finishes it: **none of the 15 that were fetched
returned an `<AbstractText>` element**. PubMed may simply not hold abstract
bodies for these supplement entries, in which case completing the backfill
recovers little. That is an argument for running it and finding out, not for
leaving 265 rows in an unknown state.

### I nearly published a false finding, and the rule caught it
My first pass rebuilt the cache key without stripping the `https://doi.org/`
prefix that `fetch_pubmed_by_doi()` strips at `:45`. Every reconstructed
filename was wrong, so the check reported **0 of 280 fetched** and I was one step
from recording that the backfill had never run at all. The giveaway was that the
same derivation also reported 0 of 826 for rows that demonstrably HAVE text.

15.1 now asserts the key derivation directly, against the three input shapes the
script handles, so the same mistake fails loudly instead of producing a
confident wrong number.

A second premise was wrong the same way: 15.7 originally demanded every cache
file match a cohort DOI and flagged 1,472 of 1,566 as orphans. They are
**PMID-keyed entries** from another stage sharing the same directory. Corrected
to accept either key space and fail only on files belonging to neither.

Both errors were the same shape as the ones the loop prompt now warns about:
reconstructing a contract instead of reading it.

**Result:** 13/14 assertions pass. The single failure is the finding above.
**Gate: green — 16 failures, all on the manifest.**

## Cycle 16 - 2026-09-04

Mix required: 4 BVA / 3 semantic / 3 adversarial. File:
`tests/testthat/test-cycle16_session_type.R` (17 assertions).
Target: `R/01d_tag_session_type.R`. Session type is the first filter in the
denominator chain (1,154 parsed minus 48 video gives the 1,106 cohort), and
nothing had tested it.

**Result: 17/17 pass. No implementation defects and no defects in my own tests.**
Second clean cycle of the sixteen.

Contracts asserted rather than assumed: the case_when mapping at `:72` including
the 2022 "Video Sessions" wording and case-insensitivity; that `current_section`
starts NA so an item before the first `h3.section-title` would be tagged NA and
escape the video filter; that the Oral subset equals the cleaned cohort exactly;
that the dropped rows are exactly the Video rows.

16.7 records why no `Poster` row exists even though the mapping can produce one:
technical appendix A14 shows ingestion captures a contiguous prefix stopping
between S26 and S60 while supplements run to S141-S286, so the poster sections
were never reached. If posters ever appear, the exclusion filter at
`02_clean_abstracts.R:34` drops Video only and posters would enter the
denominator.

16.10 checks the tagger reads TOC section headings rather than titles: abstracts
whose title mentions "video" are not predominantly tagged Video.

**Gate: green — 16 failures, all on the manifest.**

---

# LOOP STOPPED AFTER CYCLE 16

Stopped on user instruction, redirected to remediation PR work. 16 of 24 cycles
completed: 160 tests added, 8 real defects fixed, 16 findings registered on the
expected-failure manifest, and roughly a dozen defects in my own tests caught and
corrected along the way.
