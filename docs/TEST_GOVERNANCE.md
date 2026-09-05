# Test governance

How this repository decides what "green" means, and why a passing suite here is
a stronger claim than it was.

`docs/VALIDATION.md` is the inventory of what each test file covers. This
document is about the machinery around them: the gate, the two manifests, and
the rules that stop a guard quietly ceasing to guard anything.

## The problem this exists to solve

A test suite reports three outcomes, and only two of them are usually watched.
A test can pass, it can fail, or it can **not run at all**. The third is the
dangerous one, because a skipped test occupies the place where coverage is
supposed to be while asserting nothing, and every summary line counts it on the
good side of the ledger.

Two real defects lived in that blind spot in this repository:

- **The backfill coverage check.** `test-cycle15_backfill_contract.R` asserted
  that every eligible abstract had at least been attempted by the PubMed
  backfill, by reading `data/cache/pubmed_xml`. That directory is gitignored, so
  in CI the test skipped, the gate counted the skip as a pass, and it then
  reported the test's expected-failure entry as *stale*. `main` was red for two
  days with no correct fix available, because the finding was real and the gate
  was insisting it had been fixed.

- **The Shiny bundle currency checks.** Forty-five assertions in
  `test-shiny_bundle_currency.R` read `shiny/adjudication_app/bundle/`, which is
  gitignored and 47 MB. They skipped in CI for their whole life, guarding
  against exactly the defect that had already happened: a deploy bundle 135 days
  behind the analysis, with reviewers adjudicating a pre-denominator-fix cohort.

Both were found by accident. Nothing would have caught the third.

## The three-gate pipeline

`.github/workflows/tests.yaml` runs gates in order, cheapest and most
diagnostic first, and stops at the first failure. `config/ci_contract.yml`
declares them as data, and `tests/testthat/test-ci_contract.R` fails if the
declaration and the reality drift apart.

| # | Gate | Why it runs where it does |
|---|---|---|
| 1 | Decision precedence and denominator contracts (BVA) | These encode defects that actually reached the analysis outputs |
| 2 | Mutation tests | A surviving planted defect means the suite stopped detecting something it was written to catch, which a passing run cannot show |
| 3, 4 | Identity guards: reviewer pseudonyms, operator paths | A regression republishes a real person's identity, and no later green run undoes that |
| 5 | Full suite, measured against both manifests | Everything else |

The contract is checked in both directions. A declared workflow that does not
exist fails, and, since a workflow was once added without being declared, a
workflow on disk that is *not* declared fails too. A gate declared in the
contract that no workflow actually invokes also fails, so the contract cannot
promise something nothing keeps.

## Manifest 1: tests that are expected to fail

`tests/expected_failures.yaml`.

This repository deliberately keeps tests red. Each one marks a question that
code cannot answer: resolving it would change the estimand, the cohort, or an
adjudication a human already recorded. Weakening the assertion would hide the
question, so the assertion stays and the failure is registered.

Every entry carries `file`, `test`, `reason` and `decision_needed`. A bare test
name is rejected, because a manifest without reasons decays into a list of
excuses.

The gate fails when:

- a test fails that is **not** listed (a regression),
- a listed test **passes** (the decision was taken and the entry outlived its
  reason), or
- an entry names a test that **never ran** (renamed or deleted, leaving its
  excuse behind).

The second rule matters as much as the first. It is what stops the list rotting.

`docs/DECISIONS_PENDING.md` is generated from this file by
`R/generate_decisions_pending.R`, so the open questions reach the person who has
to answer them rather than living only in a YAML file read by CI. A currency
test fails if the document and the manifest disagree.

### The ceiling

`manifest.max_entries` is a ratchet, not a quota. It was raised from 20 to 24
during cycles 17-24 of the test-generation loop, deliberately and with the
reason recorded in the contract: every entry added in those cycles names a
specific artefact, a specific count and a decision that belongs to the author.
The loop surfaced decisions faster than they were answered. Refusing to register
a real finding in order to stay under a ceiling would be the ceiling defeating
its own purpose.

**It should come back down as decisions close.** It is not a new normal.

## Manifest 2: tests that are approved to skip

`tests/expected_skips.yaml`.

A skipped test is not a passing test. The gate now fails when a test skips
**without** an entry here, and prints every skip with its recorded reason, so an
intentional gap reads as a gap rather than blending into the pass count.

Every entry carries `reason` (why it cannot run here) and `to_enable` (what
would actually make it run, or "cannot" with the cost).

### Enforced in one direction only

An **unapproved skip fails**. An **approved skip that runs anyway does not**.

This asymmetry is deliberate. The skip set is genuinely environment-dependent:
the 47 MB deploy bundle, the 26 MB PubMed XML cache and the 130 MB candidate
pool exist on a machine that has run the pipeline and not in a clean checkout,
so the same suite legitimately skips a dozen tests in CI and none locally.
Failing on that difference would make the guard unusable rather than strict.
Entries that ran anyway are reported instead, so the list can still be pruned.

### It is a backlog, not a set of exemptions

Most entries were closed rather than accepted. The list went from 20 to 13 by
making the tests runnable:

| Fix | Skips converted |
|---|---|
| Verify the bundle from its committed `bundle_manifest.csv` instead of the 47 MB bundle | 4 |
| Commit `output/candidate_pool_index.csv`, a 1.4 MB `abstract_id`/`pmid` projection of the 130 MB pool | 7 |
| Track `output/tables/table1_column_ns.csv` | 1 |

**Measured effect: 75 assertions that ran only on a developer machine, down to
23.** Fifty-two are now enforced in CI, including `F2: every winning PMID
resolves in the candidate pool`, a central pipeline invariant that had never
once run there.

The 23 that remain are genuinely environment-bound: three assert properties *of*
the PubMed cache, two boot a Shiny server against the bundle, and the rest need
either the bundle or score columns the index deliberately omits.

## Verify in a clean worktree, not in the working tree

The most useful process lesson from this work, learned the hard way.

Cycle 20's gender-conflict test passed on every local run and failed the instant
the gate ran in a clean checkout. The working tree contained a regenerated
artefact that was a concurrent agent's uncommitted change; committed state was
inconsistent and every local run had been reading the fix.

```sh
git worktree add --detach /tmp/verify HEAD
cd /tmp/verify && Rscript tests/run_suite_gate.R
git worktree remove --force /tmp/verify
```

A worktree checkout holds only tracked files, which is exactly what CI sees. Any
gate result quoted as authoritative in this repository should come from one.

## The browser suite

`test-shiny_e2e.R` is opt-in and off by default. It previously guarded itself
with `skip_if_not_installed("shinytest2")`, and since `shinytest2` is installed
nowhere, 332 lines of browser tests contributed zero assertions everywhere while
still appearing in the suite as a test file.

It now requires `RUN_SHINY_E2E=true`, and a floor assertion runs in **every**
environment and fails if the exclusion is not registered in the skip manifest,
so the gap cannot go unrecorded. Enabling it in CI would not produce coverage:
it needs a browser and the 130 MB candidate pool, so it would launch Chrome
against an app with nothing to render.

```sh
install.packages("shinytest2")
RUN_SHINY_E2E=true Rscript -e 'testthat::test_file("tests/testthat/test-shiny_e2e.R")'
```
