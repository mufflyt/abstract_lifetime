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
