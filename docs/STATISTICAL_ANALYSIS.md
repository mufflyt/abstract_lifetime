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
| **Missing-data handling** | complete-case throughout. No imputation anywhere in the pipeline. |
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
| n_published | 178 |
| n_with_dates | **171** |
| n_pre_congress | 7 |
| n_undated | 0 |
| median_months | **13.7** |
| q1_months | 5.7 |
| q3_months | 22.6 |
| mean_months | 16.8 |
| min_months | 0.5 |
| max_months | 149.8 |

**All 178 published abstracts now carry a publication date** (it was 104 until
2026-09-03, when `scripts/rebuild_candidate_pool.R` repaired the candidate pool
— [FAILURE_MODES.md](FAILURE_MODES.md) F2 — and `06` began re-joining on
`final_pmid` rather than `best_pmid`, F12).

The median is computed on the **171** whose publication follows their congress.
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

**Fitted object**: n = **1,044**, events = **171**.

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

**This is a data-dependent specification.** `session_type` drops because it is
constant. `practice_type` (17.5%) and `subspecialty` drop on the missingness
rule. `log_sample_size` is never created in `km_data` — it is built inside the
Aim 3 block only — so it can never enter the Cox model despite being listed.
Rerunning on different data can silently change the model.

**The formula that actually ran** (read back from
`data/processed/cox_model.rds`):

```r
Surv(time, event) ~ is_rct + is_academic + is_us_based + n_authors +
                    gender_unified + is_multicenter + has_funding
```

`cox_data <- km_data |> drop_na(all_of(cox_formula_parts))` → **n = 1,004,
events = 170**. Reference categories are the `FALSE` level for each logical and
`female` for `gender_unified` (alphabetical, R default). `n_authors` enters as a
continuous count.

Results, `output/aim2b_cox_regression.csv`, exponentiated with profile-likelihood
confidence intervals from `broom::tidy(exponentiate = TRUE, conf.int = TRUE)`:

| term | HR | 95% CI | p |
|---|---:|---|---:|
| `is_rctTRUE` | 2.212 | 1.473 – 3.323 | <0.001 |
| `is_academicTRUE` | **0.621** | 0.440 – 0.876 | **0.007** |
| `is_us_basedTRUE` | 1.419 | 0.887 – 2.270 | 0.145 |
| `n_authors` | 1.257 | 1.093 – 1.445 | 0.001 |
| `gender_unifiedmale` | 0.783 | 0.575 – 1.066 | 0.120 |
| `is_multicenterTRUE` | 1.387 | 0.809 – 2.377 | 0.234 |
| `has_fundingTRUE` | 1.757 | 0.431 – 7.169 | 0.432 |

These moved substantially on 2026-09-03. The event count rose from 104 to 171
(F2) and five covariates were re-derived from text that had not been available
when they were first computed (F3). `is_academic` went from HR 0.87 (p = 0.62)
to HR 0.62 (p = 0.007) — academic affiliation now looks associated with *slower*
publication — while `is_us_based` and `gender_unified` lost significance. Both
changes are consequences of measurement corrections, not of new data, and the
`is_academic` estimate in particular rests on a variable whose ascertainment
just tripled (148 → 371 TRUE). Treat it as provisional.

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
| `gender_unifiedmale` | 0.804 | 0.571 – 1.127 | 0.208 |
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

## Diagnostics

| Check | Implemented | Result |
|---|---|---|
| Proportional hazards | `cox.zph(cox_model)`, global test written to `output/cox_ph_assumption.csv` | global p = **0.052**. It was 0.32 on 104 events; with 171 events the test is far better powered and the assumption is now only marginally supported. A stratified or time-varying specification is worth considering before the hazard ratios are reported as constant. |
| Collinearity | **not implemented** | — |
| Goodness of fit (Hosmer–Lemeshow, calibration, AUC) | **not implemented** | — |
| Sparse-category handling | Implicit only: the < 50% missing and ≥ 2 level screen. No minimum cell count. | `has_funding` is TRUE for 3 of 1,106 abstracts; its Cox CI spans 0.48–25.7 and its logistic CI 0.12–29.0 |
| Influence / outliers | **not implemented** | — |
| Overdispersion | not applicable (binomial with n = 1 trials) | — |

`has_funding` (**3** abstracts TRUE cohort-wide) and `is_multicenter` (38 TRUE)
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
3. **`gender_unified` is inferred**, 27% of it from a single initial, and 228
   abstracts carry a cross-source disagreement.
4. **Proportional hazards is now only marginally supported** (global p = 0.052
   on 171 events, against 0.32 on 104). The constant-hazard-ratio reading of the
   Cox table should be checked with a stratified or time-varying model.
5. **`is_academic` changed direction and significance** when its ascertainment
   was corrected (148 → 371 TRUE). It is the single least stable estimate in
   either model.
