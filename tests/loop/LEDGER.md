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
