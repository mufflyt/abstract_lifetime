# 24-Cycle Test Generation Protocol

Cycle N of 24. Ten NEW tests per cycle, on ~30-minute intervals.

## Mix (rotates every 3 cycles)
- Cycle 1,4,7,10,13,16,19,22: 4 BVA / 3 semantic / 3 adversarial
- Cycle 2,5,8,11,14,17,20,23: 3 BVA / 4 semantic / 3 adversarial
- Cycle 3,6,9,12,15,18,21,24: 3 BVA / 3 semantic / 4 adversarial

## Categories
**BVA** min/max valid, just below/above boundaries, zero, one, empty, NA, Inf,
date/year boundaries, denominator and count boundaries, floating-point/rounding.

**Semantic/contract** functions mean what their name and docs claim; units;
denominators; cohorts; signs and directions; labels match the quantity actually
computed; joins do not change the estimand; defaults scientifically defensible;
equivalent inputs give equivalent answers; distinct concepts not conflated.

**Adversarial** malformed but plausible input, duplicated/missing/reordered rows,
duplicate or conflicting ids, missing years, unexpected types, extreme values,
sparse and empty subsets, stale artifacts, mismatched vintages, inconsistent
metadata, file ordering, RNG/session-state dependence, hidden cwd/env
dependence, assumptions true only of the current fixture.

## Priority targets in this repo
joins and denominators; cohort assembly; adjudication precedence; year/date
progression and congress-date logic; publication-rate and time-to-publication
estimands; survival censoring; scoring thresholds; public-facing tables and
figures; scenario/config parameters; reproducibility and session-state.

Deprioritize cosmetic code unless it can cause scientific misinterpretation.

## Per-cycle procedure
A. Inventory: read LEDGER.md, list existing tests, pick 10 distinct targets.
B. Design: for each, record category, target, assumption challenged, expected
   behavior, and why existing tests do not cover it. Reject duplicates.
C. Write exactly 10 new tests in tests/testthat/test-cycleNN_*.R
D. Run: new tests, then related files, then the full suite.
E. Failures: classify as (a) real defect (b) wrong test (c) scientific ambiguity
   (d) pre-existing unrelated. Fix defects with the smallest defensible change.
   Never weaken a test to get green. For genuine estimand ambiguity, PRESERVE
   the failing test, document the decision needed, and do not silently choose.
F. Anti-cheating: no deleting/skipping meaningful tests, no broadened tolerances,
   no vague assertions, no hard-coded fixture answers in production code, no
   suppressed warnings, no removed validation, no changed estimand without an
   explicit note, no test data changed just to pass.
G. Record: append a cycle entry to LEDGER.md.
H. Wait for the next 30-minute boundary. Do not idle while a failure is open.

## Final audit after cycle 24
Full suite; review all 240 tests for duplicates, flakiness, over-specificity,
contradictions, order dependence; rerun stochastic tests in a fresh session;
summarize counts by category, defects found/fixed/unresolved, unresolved
estimand decisions, pre-existing failures, files changed, and the most
consequential defects.
