# Statistical Analysis

Everything implemented in `R/06_analyze_results.R`. Formulae are transcribed
from the fitted model objects in `data/processed/*.rds`, not from the source
text, so what is written here is what actually ran.

All models are fitted on `output/abstracts_with_matches.csv` joined to
`output/manual_review_decisions.csv`; the joined frame is exported unchanged as
`output/final_analytical_dataset.csv` **before** any analysis
(`R/06_analyze_results.R:43`).

---

## 0. Common definitions

| Term | Value |
|---|---|
| **Cohort** | 1,106 eligible oral presentations |
| **Denominator (evaluated)** | 1,051 = cohort − 55 with `final_published == NA` |
| **Outcome** | `final_published`, see [OUTCOME_DEFINITION.md](OUTCOME_DEFINITION.md) |
| **Origin date** | `conference_date_for(congress_year, cfg)` — the congress start date in `config.yml` (2012-11-06 … 2023-11-07) |
| **Administrative censoring date** | `cfg$pubmed$date_end` = **2026-04-01**, the end of the PubMed search window |
| **Time scale** | months, days ÷ 30.44 |
| **Missing-data handling** | complete-case throughout. No imputation anywhere in the pipeline. Missingness is now *reported* rather than only stated — see the section below. |
| **Seed** | `set.seed(42)` in `00_run_all.R`; no analysis step is stochastic. |

---

## Aim 1 — Publication proportion

**Estimand.** The proportion of AAGL oral presentations, 2012–2023, that reached
full peer-reviewed publication by 2026-04-01, among presentations whose
publication status could be determined.

**Population.** The 1,051 abstracts with a non-`NA` `final_published`.

**Numerator.** `sum(results$final_published, na.rm = TRUE)` = **178**.
**Denominator.** `n_evaluated <- n_total - n_pending` = 1,106 − 55 = **1,051**.

**Point estimate.** 178 / 1,051 = **16.94%**, reported as **16.9%**.

**Interval.** `prop.test(178, 1051, correct = FALSE)` — the **Wilson score
interval without continuity correction**, not an exact binomial interval.
**95% CI 14.8% – 19.3%.**

**The 55 excluded from the denominator** are all `probable`/`possible` with a
reviewer `skip`. Removing them is an available-case analysis that assumes they
are missing at random with respect to publication. Bounding: 178/1,106 = 16.1%
if all were unpublished, 233/1,106 = 21.1% if all were published.

**Stratified outputs** (all restricted to `!is.na(final_published)`):
`aim1_by_congress_year.csv`, `aim1_by_practice_type.csv`,
`aim1_by_subspecialty.csv`. `aim1_by_pub_type.csv` counts publication types
among the published only. Each is written only if the stratifying column exists
in the input — if `10e_merge_demographics.R` has not run, the practice-type and
subspecialty files are silently not produced.

---

## Aim 2 — Time to publication

**Estimand.** Time from congress presentation to full publication.

**Origin.** Congress start date.
**Event date.** First day of the matched publication's PubMed issue month
(see [MATCHING_ALGORITHM.md](MATCHING_ALGORITHM.md) §5).
**Event time.** `months_to_pub`.
**Censoring date.** 2026-04-01; `censor_time = (2026-04-01 − congress_date)/30.44`,
which ranges from 28.9 months (2023) to 160.9 months (2012).

### Descriptive statistics (`output/aim2_time_to_pub.csv`)

Computed on `published |> filter(!is.na(months_to_pub))` — an unweighted
empirical median, **not** a Kaplan–Meier median:

| metric | value |
|---|---:|
| n_published | 170 |
| n_with_dates | **170** |
| n_pre_congress | 7 |
| n_undated | 0 |
| median_months | **13.6** |
| q1_months | 5.7 |
| q3_months | 22.4 |
| mean_months | 16.7 |
| min_months | 0.5 |
| max_months | 149.8 |

**All 178 published abstracts now carry a publication date** (it was 104 until
2026-09-03, when `scripts/rebuild_candidate_pool.R` repaired the candidate pool
— [FAILURE_MODES.md](FAILURE_MODES.md) F2 — and `06` began re-joining on
`final_pmid` rather than `best_pmid`, F12).

The median is computed on the **170** whose publication follows their congress.
Seven confirmed publications appeared *before* their meeting: four are
pre-conference candidates a reviewer confirmed anyway, one is a `definite`
online-first paper two weeks ahead of the 2015 congress, and two are 2018
reviewer-supplied PMIDs resolved for the first time. They belong in the
numerator — a reviewer ruled they are the abstract's publication — but a
negative interval is not a time to publication. The four counts above partition
the published set exactly, which
`tests/testthat/test-remediation_invariants.R` asserts.

### Kaplan–Meier

```r
km_data <- results |>
  filter(!is.na(final_published)) |>
  mutate(
    censor_time = (as.Date("2026/04/01") - conference_date_for(congress_year)) / 30.44,
    time = case_when(
      final_published & !is.na(months_to_pub) ~ months_to_pub,   # event
      !final_published                        ~ censor_time,      # censored
      TRUE                                    ~ NA_real_          # dropped
    ),
    event = as.integer(final_published)
  ) |>
  filter(!is.na(time), time > 0)

km_fit <- survfit(Surv(time, event) ~ 1, data = km_data)
```

**Fitted object**: n = **1,051**, events = **170**.

Right censoring is necessary because the congresses have unequal follow-up: a
2023 abstract has had 28.9 months to publish and a 2012 abstract 160.9. Treating
"not yet published" as "will never publish" would confound the publication rate
with congress recency, which is exactly the artefact visible in the crude rate
by year.

**The exclusion that used to matter no longer bites.** The code drops published
abstracts with no event time rather than censoring them, on the grounds that they
are known events. That was removing 74 of 178 events until the candidate pool was
repaired; it now removes none. The seven pre-congress publications are still
dropped by `filter(time > 0)`, which is correct — they have no positive time at
risk.

`survfit` uses the Kaplan–Meier product-limit estimator with the default
`conf.type = "log"`.

### Cox proportional hazards

Variables are selected **automatically at run time**
(`R/06_analyze_results.R:195-209`) from the candidate set

```
is_rct, log_sample_size, is_academic, is_us_based, session_type,
n_authors, gender_unified, practice_type, is_multicenter, has_funding
```

keeping only those that (a) exist as a column in `km_data`, (b) have < 50%
missing, and (c) have ≥ 2 distinct non-missing values. The model is fitted only
if ≥ 2 variables survive and ≥ 30 complete cases remain.

**This is a data-dependent specification**, and since 2026-09-04 it is at least
a *recorded* one. `screen_model_vars()` in `R/06_analyze_results.R` applies one
rule to both models and writes every decision to
`output/model_variable_screen.csv`, so the specification can be reconstructed
from the outputs alone rather than inferred from the code plus the data.

Three criteria, in order: more than 50% missing; fewer than 2 distinct values;
**near-zero variance** (the conventional rule — frequency ratio of the most to
second-most common value above 19, and distinct values below 10% of rows).
A candidate that does not exist in the model frame is recorded as `absent`
rather than silently removed.

| model | variable | kept | reason |
|---|---|---|---|
| cox | `is_rct`, `is_academic`, `is_us_based`, `n_authors`, `gender_unified`, `is_multicenter` | ✅ | kept |
| cox | `log_sample_size` | ❌ | **absent from the model frame** — listed as a Cox candidate but only ever created inside the Aim 3 block, so it has never entered the Cox model |
| cox | `session_type` | ❌ | fewer than 2 distinct values |
| cox | `practice_type` | ❌ | 84.2% missing |
| cox | `has_funding` | ❌ | **near-zero variance** |
| logistic | `n_authors`, `gender_unified`, `is_multicenter` | ✅ | kept |
| logistic | `session_type`, `practice_type` (84.2%), `subspecialty` (85.3%), `has_funding` | ❌ | as above |

The near-zero-variance criterion is new and it changed both models:
`has_funding` is TRUE for 7 of 1,051 evaluated abstracts, a frequency ratio of
about 149:1, and is now excluded by rule rather than reported with an interval
spanning an order of magnitude. Every other coefficient moved by less than 0.03
(Cox `is_rct` 2.205 → 2.227; logistic `is_rct` 2.552 → 2.563).

> **Historical, 2026-09-04.** The two paragraphs above record what the
> near-zero-variance screen did on the day it was introduced. The coefficients
> quoted are the values of that moment, not the current fit — every model term
> moved again on 2026-09-05 when the author-gender waterfall was corrected. The
> current numbers are in the results table below.

**One consequence to note.** The Cox proportional-hazards global test moved from
p = 0.056 to **p = 0.043** when the term was dropped, so the assumption became
formally violated at α = 0.05. That was an open question until 2026-09-04 and
is now resolved: the violation is confined to `n_authors` and is modelled
explicitly. See *Proportional hazards* below.

**The formula that actually ran** (read back from
`data/processed/cox_model.rds`):

```r
Surv(time, event) ~ is_rct + is_academic + is_us_based + n_authors +
                    gender_unified + is_multicenter
```

`has_funding` is no longer a term: the near-zero-variance screen above removes
it before the formula is built. (This document listed it here until 2026-09-04,
which contradicted the screen table two sections up. The formula shown is now
the one read back from `data/processed/cox_model.rds`.)

`cox_data <- km_data |> drop_na(all_of(cox_formula_parts))` → **n = 1,018,
events = 170**. Reference categories are the `FALSE` level for each logical and
`female` for `gender_unified` (alphabetical, R default). `n_authors` enters as a
continuous count.

Results, `output/aim2b_cox_regression.csv`, exponentiated with profile-likelihood
confidence intervals from `broom::tidy(exponentiate = TRUE, conf.int = TRUE)`:

| term | HR | 95% CI | p |
|---|---:|---|---:|
| `is_rctTRUE` | 2.002 | 1.336 – 3.001 | 0.001 |
| `is_academicTRUE` | 1.097 | 0.782 – 1.539 | 0.587 |
| `is_us_basedTRUE` | 0.787 | 0.575 – 1.078 | 0.136 |
| `n_authors` | 1.218 | 1.058 – 1.402 | 0.006 |
| `gender_unifiedmale` | 0.746 | 0.549 – 1.014 | 0.066 |
| `is_multicenterTRUE` | 1.361 | 0.793 – 2.336 | 0.265 |

The `n_authors` row is a hazard ratio **averaged over follow-up** and should not
be quoted on its own; that term violates proportional hazards and the effect it
summarises is not constant. The time-specific estimates are below.

These moved substantially on 2026-09-03. The event count rose from 104 to 171
(F2) and five covariates were re-derived from text that had not been available
when they were first computed (F3). `is_academic` went from HR 0.87 (p = 0.62)
to HR 0.62 (p = 0.007) — academic affiliation now looks associated with *slower*
publication — while `is_us_based` and `gender_unified` lost significance. Both
changes are consequences of measurement corrections, not of new data, and the
`is_academic` estimate in particular rests on a variable whose ascertainment
just tripled (148 → 371 TRUE). Treat it as provisional.

### Proportional hazards

**Resolved 2026-09-04.** This was an open methodological decision, recorded as a
preserved failing test, from 2026-09-03 until the diagnosis below was run.

The global Schoenfeld test is significant (`cox.zph`, global χ² = 12.99 on 6 df,
**p = 0.043**), but a global test cannot say which covariate is responsible and
the remedy depends entirely on that answer. `R/06_analyze_results.R` now writes
the per-term tests to `output/cox_ph_terms.csv`:

| term | χ² | df | p |
|---|---:|---:|---:|
| `is_rct` | 2.121 | 1 | 0.145 |
| `is_academic` | 3.383 | 1 | 0.066 |
| **`is_us_based`** | **8.028** | 1 | **0.005** |
| **`n_authors`** | **8.756** | 1 | **0.003** |
| `gender_unified` | 0.002 | 1 | 0.962 |
| `is_multicenter` | 0.297 | 1 | 0.586 |
| GLOBAL | 16.735 | 6 | 0.010 |

The violation is **entirely attributable to `n_authors`**. Every other term sits
between p = 0.13 and p = 0.96, and refitting without `n_authors` returns the
global test to **p = 0.48**. The Schoenfeld residuals for `n_authors` correlate
positively with time (Spearman ρ = +0.253): the effect of team size *grows* over
follow-up rather than being constant.

**What was chosen, and why.** Three responses were available and all three were
fitted before choosing.

| option | global p after | consequence |
|---|---:|---|
| Report the HRs as averages over follow-up | 0.043 (unchanged) | Honest but uninformative — it names the problem without measuring it, and leaves `n_authors` = 1.266 to be read as a constant anyway |
| Stratify on both violators | 0.607 | Restores the assumption but **discards the estimate**. `n_authors` is one of only two predictors that survive bootstrap resampling (88.6% retention, `R/06d_model_stability.R`); stratifying it away deletes a real finding to fix a diagnostic |
| **Time-varying coefficient** (chosen) | — | Keeps the term and estimates its drift. AIC 2286.3 vs 2294.1 for the PH fit; the log-time interaction is significant at p = 0.001 |

The model is `coxph(..., + tt(n_authors), tt = function(x, t, ...) x * log(t))`,
written to `data/processed/cox_model_timevarying.rds` and
`output/aim2b_cox_regression_timevarying.csv`. The log-time form was chosen
because it is the transform the Schoenfeld residuals are plotted against by
default in `cox.zph`, so the remedy tests the same alternative the diagnostic
raised, rather than a different one chosen after the fact.

**What the constant hazard ratio was hiding**
(`output/cox_time_varying_hr.csv`, HR per additional author):

| follow-up | HR | 95% CI |
|---:|---:|---|
| 3 months | 1.004 | 0.835 – 1.209 |
| 6 months | 1.150 | 0.994 – 1.330 |
| 12 months | 1.316 | 1.134 – 1.528 |
| 24 months | 1.507 | 1.240 – 1.830 |
| 36 months | 1.631 | 1.294 – 2.056 |
| 48 months | 1.725 | 1.330 – 2.238 |

Team size has **no detectable effect on early publication** and a substantial
one later. The single PH estimate of 1.252 was averaging a null first three months
against an effect that reaches ~1.75 by four years. Substantively this is a
statement about persistence rather than speed: larger teams are not faster to
first publication, they are the ones still converting abstracts to papers years
afterwards, while small-team abstracts go dead.

**Sensitivity.** `output/aim2b_cox_regression_stratified.csv` stratifies on
`n_authors` instead, which restores the assumption (global p = 0.607) and lets
the other five hazard ratios be read as constants. They barely move — every one
is within 2% of the primary fit:

| term | primary (PH) | stratified | time-varying |
|---|---:|---:|---:|
| `is_rctTRUE` | 2.146 | 2.183 | 2.138 |
| `is_academicTRUE` | 0.637 | 0.624 | 0.632 |
| `is_us_basedTRUE` | 1.374 | 1.373 | 1.372 |
| `gender_unifiedmale` | 0.779 | 0.782 | 0.776 |
| `is_multicenterTRUE` | 1.372 | 1.354 | 1.355 |

So the reported covariate effects do not depend on how `n_authors` is handled.
`tests/testthat/test-pipeline_semantics.R` asserts this as a contract: any
non-violating hazard ratio that moved by more than 15% under stratification, or
changed direction, fails the suite.

**Limitation, stated plainly.** `n_authors` is **censored at 5** — 506 of the
1,027 abstracts in the model frame (49.3%) sit at that ceiling, an artefact of
the ScienceDirect ingest recording at most five authors. The time-varying
pattern is therefore estimated on a compressed covariate, which biases the
effect toward the null rather than creating it, but the *magnitude* of the
per-author hazard ratio should not be read as if the count were complete.

---

## Aim 3 — Predictors of publication (logistic regression)

**Population.** `results |> filter(!is.na(final_published))`, then
`drop_na()` on the model terms → **n = 1,010** (reported as `n_obs` in the
output file since 2026-09-03). Forty-one of the 1,051 evaluated abstracts leave
through complete-case deletion.

**Outcome.** `published_int = as.integer(final_published)`.

**Specification.** A fixed core (`is_rct + log_sample_size + is_academic +
is_us_based`) plus any of `session_type, n_authors, gender_unified,
practice_type, is_multicenter, has_funding, subspecialty` that pass the same
< 50% missing and ≥ 2 levels screen. **The formula that actually ran** (read
back from `data/processed/logistic_model.rds`):

```r
published_int ~ is_rct + log_sample_size + is_academic + is_us_based +
                n_authors + gender_unified + is_multicenter + has_funding
```

`family = binomial(link = "logit")`. `log_sample_size = log1p(coalesce(sample_size, 0))`
— **note that a missing sample size is coalesced to 0, not dropped**, so the 741 of
1,106 abstracts with no extractable sample size contribute `log1p(0) = 0` and are pooled with genuine
zero-size studies. That is an implicit and undeclared imputation.

Results, `output/aim3_logistic_regression.csv` (odds ratios, profile-likelihood
CIs):

| term | OR | 95% CI | p |
|---|---:|---|---:|
| `(Intercept)` | 0.056 | 0.026 – 0.114 | <0.001 |
| `is_rctTRUE` | 2.556 | 1.551 – 4.156 | <0.001 |
| `log_sample_size` | 0.952 | 0.884 – 1.024 | 0.190 |
| `is_academicTRUE` | **0.603** | 0.405 – 0.887 | **0.011** |
| `is_us_basedTRUE` | 1.621 | 0.964 – 2.808 | 0.075 |
| `n_authors` | 1.336 | 1.154 – 1.560 | <0.001 |
| `gender_unifiedmale` | 0.835 | 0.594 – 1.171 | 0.298 |
| `is_multicenterTRUE` | 1.482 | 0.769 – 2.718 | 0.218 |
| `has_fundingTRUE` | 2.211 | 0.293 – 11.345 | 0.372 |

`broom::tidy(conf.int = TRUE)` is wrapped in `tryCatch`; if the profile
likelihood fails the script silently falls back to point estimates without
intervals. It did not fail here.

---

## Aim 4 — Search-strategy performance

For each PubMed strategy: `n_searched`, `n_with_hits`, `yield_pct` from
`pubmed_strategy_results.csv`; and `n_found_correct` from an `inner_join` of the
confirmed `final_pmid` values against `pubmed_candidates.csv`, unnested on the
semicolon-separated `strategies` column. The four supplementary sources get a
coarser treatment: `n_with_hits` is the count of distinct abstracts with any
candidate.

**Both inputs are stale**, so this table understates attribution and should not
be quoted. See [FAILURE_MODES.md](FAILURE_MODES.md) F2.

---

## Aim 5 — Publication bias by result direction

`result_positivity` was silently absent from `R/05_adjudicate.R`'s `select()`,
so this block was gated off from 2026-04-17 until 2026-09-03. It now runs.

| result_positivity | n | n_published | rate |
|---|---:|---:|---:|
| negative | 48 | 15 | 31.2% |
| neutral | 119 | 17 | 14.3% |
| positive | 248 | 50 | 20.2% |
| **positive vs negative, OR** | 296 | — | **0.556** |

The direction is the opposite of the classic file-drawer prediction: abstracts
classified as reporting a negative result published at a *higher* rate than
positive ones. Read it with care — `classify_result_positivity()` returns
`unclear` for 666 of the 1,106 abstracts, so this table covers 415 of them, and
the classifier operates on whatever text is available, which is much less for
the earlier congresses.

---

## Sensitivity analyses

`output/sensitivity_analyses.csv`:

| scenario | denominator | n | n_published | rate |
|---|---|---:|---:|---:|
| Definite only | cohort | 1,106 | 131 | 11.8% |
| Definite + probable | cohort | 1,106 | 212 | 19.2% |
| Definite + reviewer-confirmed | evaluated | 1,051 | 178 | 16.9% |
| Published within 12 months | evaluated with sufficient follow-up | 1,051 | 75 | 7.1% |
| Published within 24 months | evaluated with sufficient follow-up | 1,051 | 133 | 12.7% |
| Published within 36 months | evaluated with sufficient follow-up | 991 | 136 | 13.7% |
| Published within 48 months | evaluated with sufficient follow-up | 901 | 118 | 13.1% |

The `denominator` column was added on 2026-09-03: rows 1–2 are decidable without
a reviewer and divide by the cohort, row 3 onward require an adjudicated outcome
and divide by the evaluated set. A reader comparing 11.8% with 16.9% was
otherwise comparing two different populations with nothing in the file to say so.

The follow-up-window rows also moved sharply (12 months 4.1% → 7.1%, 24 months
7.3% → 12.7%) because they counted only publications with a known date and
therefore inherited the 104-of-178 problem. The apparent decline from 36 to 48
months is now much smaller and reflects the shrinking denominator rather than
missing dates.

There is still **no** sensitivity analysis for the two decisions that matter
most: the branch order in `assign_final_published()` (48 abstracts) and the
treatment of the 55 unresolved.

The follow-up-window rows are the closest thing to a lead-time sensitivity
analysis. ---

## Model stability

`R/06d_model_stability.R`. Neither model previously carried any influence,
leave-one-out or stability diagnostic, and `is_academic` was described as
"provisional" on the strength of a judgement rather than a number.

### Bootstrap predictor retention

500 resamples of the logistic model's complete-case frame (n = 1,027), counting
how often each term is retained at p < 0.05
(`output/model_predictor_stability.csv`):

| predictor | retained | reading |
|---|---:|---|
| `is_rct` | **93.2%** | robust |
| `n_authors` | **88.6%** | robust |
| `is_academic` | 5.6% | unstable |
| `is_us_based` | 37.4% | unstable |
| `log_sample_size` | 27.4% | unstable |
| `is_multicenter` | 23.6% | unstable |
| `gender_unified` | 22.2% | unstable |

**Only two terms are robust to resampling.** `is_academic` reaches p = 0.012 in
the fitted model but survives in barely two-thirds of resamples, which is the
quantitative form of the caution already attached to it.

### Leave-one-congress-out

The model refitted twelve times, dropping each congress in turn, for all seven
terms — 84 refits, all converged
(`output/model_leave_one_congress_out.csv`):

| term | ratio range | significant in |
|---|---|---:|
| `is_rctTRUE` | 2.09 – 3.12 | 12 / 12 |
| `n_authors` | 1.28 – 1.39 | 12 / 12 |
| `is_academicTRUE` | 0.58 – 0.66 | 11 / 12 |
| `is_us_basedTRUE` | 1.15 – 5.15 | 1 / 12 |
| `is_multicenterTRUE` | 1.26 – 1.79 | 0 / 12 |
| `gender_unifiedmale` | 0.75 – 0.86 | 0 / 12 |
| `log_sample_size` | 0.93 – 0.97 | 1 / 12 |

No term changes direction when a congress is dropped. This matters
specifically because 2017 and 2018 have no recoverable abstract text, so their
covariates are near-constant; a finding resting on either would be an artefact.
None does.

### Reading the two together

They disagree about `is_academic`, and the disagreement is informative rather
than contradictory. It is significant in **eleven of twelve** leave-one-congress-out
refits — so it is close to congress-independent, though no longer entirely so —
but survives only **5.6%** of bootstrap resamples — so it is sensitive to which *abstracts* are
drawn. The two diagnostics test different things, and the honest summary is
that `is_academic` is not congress-driven but is not sampling-robust either. It
should be reported with that qualification, not as a headline.

`is_rct` and `n_authors` pass both.

---

## Missing data

`R/06b_missingness.R` produces what the Methods previously asserted without
evidence.

**Item-level missingness** (`output/missingness_by_variable.csv`), highest first:

| variable | % missing |
|---|---:|
| `subspecialty` | 83.8 |
| `practice_type` | 82.5 |
| `cited_by_count`, `journal_impact_proxy` | 81.4 |
| `first_author_first` | 80.9 |
| `months_to_pub` | 75.7 |
| `state_unified` | 69.6 |
| `subspecialty_unified` | 65.0 |
| `sample_size` | 36.8 |
| `gender_unified` | 3.6 |

Most of these are structural rather than accidental: a citation count or a
practice type can only exist for an abstract with a matched publication.

**Little's MCAR test** (`output/missingness_mcar.csv`): chi-square 28.8, df 7,
**p = 0.00015** — MCAR is rejected. Two caveats travel with that number and are
recorded in the output file rather than left implicit. It covers only the
**numeric block** (`sample_size`, `months_to_pub`, `cited_by_count`,
`journal_impact_proxy`); the six categorical variables were described but not
tested. And at N = 1,106 the test is highly powered and rejects on minor
deviations, so rejection here is consistent with weak, non-systematic item
missingness rather than evidence of substantial bias.

### The assumption the denominator rests on

The publication rate divides by the 1,051 evaluated abstracts and drops the 55
whose adjudication never resolved. That is an available-case analysis assuming
the 55 do not differ systematically. `output/unresolved_vs_evaluated.csv` tests
it across every model covariate:

| covariate | unresolved | evaluated | p | reading |
|---|---:|---:|---:|---|
| `best_score` | 4.72 | 3.23 | <0.001 | **definitional** — the unresolved are the mid-score band by construction, and the comparison group contains 709 `no_match` abstracts |
| `study_design` | — | — | 0.0004 | **substantive** |
| `n_authors` | 3.51 | 3.90 | 0.013 | **substantive** |
| `sample_size` | 332 | 3,082 | 0.13 | ns |
| `is_rct` | 3.6% | 9.1% | 0.16 | ns |
| `congress_year` | 2017.9 | 2017.3 | 0.19 | ns |
| `n_candidates` | 94.6 | 56.6 | 0.22 | definitional |
| everything else | | | > 0.4 | ns |

**The 55 are not missing completely at random.** Two substantive covariates
differ, and `n_authors` is a significant predictor in both models. Dropping them
therefore assumes missing-at-random *given the observed data*, which is weaker
and untestable. The bounds in [COHORT_ASSEMBLY.md](COHORT_ASSEMBLY.md) §8 —
16.1% if all were unpublished, 21.1% if all were published — remain the honest
envelope, and this result is the reason to quote them.

`output/missingness_interpretation.txt` carries a generated paragraph suitable
for adaptation into Methods.

---

## Diagnostics

| Check | Implemented | Result |
|---|---|---|
| Proportional hazards | `cox.zph(cox_model)`; global test and remediation in `output/cox_ph_assumption.csv`, per-term tests in `output/cox_ph_terms.csv` | global p = **0.010**, violated by two terms. **Diagnosed and remediated 2026-09-04**: the violation is confined to `n_authors` (p = 0.002; every other term p ≥ 0.13), which now carries a log-time interaction. Stratifying on it instead restores global p = **0.607** and moves no other HR by more than 2%. See *Proportional hazards* under Aim 2b. |
| Collinearity | **not implemented** | — |
| Predictor stability | `R/06d_model_stability.R`, 500 bootstrap refits | Only `is_rct` (93.2%) and `n_authors` (88.6%) are robust |
| Leave-one-congress-out | `R/06d_model_stability.R`, 84 refits | No term changes direction; `is_academic` significant in 11 of 12, `is_us_based` in only 1 of 12 |
| Goodness of fit (Hosmer–Lemeshow, calibration, AUC) | **not implemented** | — |
| Sparse-category handling | **Explicit** since 2026-09-04: `screen_model_vars()` applies a near-zero-variance rule and records every decision to `output/model_variable_screen.csv`. | `has_funding` is TRUE for 7 of 1,106 abstracts (it was 3 before `02d` re-derived the predictors from the backfilled text); its Cox CI spans 0.43–7.17 and its logistic CI 0.29–11.3. `mysterycall_remove_near_zero()` flags it automatically at a frequency ratio of 1044:7 |
| Influence / outliers | partially — leave-one-congress-out covers group-level influence | no single-observation diagnostic |
| Overdispersion | not applicable (binomial with n = 1 trials) | — |

`has_funding` (**7** abstracts TRUE cohort-wide) and `is_multicenter` (65 TRUE)
are sparse enough that their estimates are unstable; the funding CI spanning two
orders of magnitude is the visible symptom. Neither is dropped by the automatic
screen because the screen tests *missingness and level count*, not cell counts.

---

## Known threats to these estimates

1. **Residual differential predictor ascertainment, now confined to 2017–2018.**
   The covariates are re-derived from the backfilled text by
   `R/02d_rederive_predictors.R`, which removed the step change at 2018/2019.
   What remains is real: 96 of 97 abstracts from the 2017 congress and all 95
   from 2018 have no recoverable text, so every text-derived covariate is near
   zero for those two years. Congress year is **not** in either model, so those
   two congresses still act as a measurement-driven stratum. See
   [FAILURE_MODES.md](FAILURE_MODES.md) F3.
2. **`n_authors` is censored at 5** and is the second-strongest term in both
   models.
3. **`gender_unified` is only partly a registry value.** 267 of 1,066 come from
   NPPES or ABOG; the remaining 799 are inferred from a name and 287 of those
   from a single first initial. 231 abstracts carry a cross-source
   disagreement. Adding the NPPES tier on 2026-09-04 moved the Cox estimate
   from HR 0.783 (p = 0.120) to 0.811 (p = 0.181) — still not significant, and
   the movement is a reminder of how sensitive it is to the gender source.
4. **Proportional hazards is now only marginally supported** (global p = 0.052
   on 171 events, against 0.32 on 104). The constant-hazard-ratio reading of the
   Cox table should be checked with a stratified or time-varying model.
5. **`is_academic` changed direction and significance** when its ascertainment
   was corrected (148 → 371 TRUE). It is the single least stable estimate in
   either model.
