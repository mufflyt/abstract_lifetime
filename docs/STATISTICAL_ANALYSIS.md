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
| n_with_dates | **104** |
| median_months | **13.8** |
| q1_months | 6.3 |
| q3_months | 25.0 |
| mean_months | 17.5 |
| min_months | 0.5 |
| max_months | 149.8 |

**104, not 178.** 74 of the 178 published abstracts have no publication date
because their winning PMID is absent from the stale candidate file
([FAILURE_MODES.md](FAILURE_MODES.md) F2). Every quantile above is computed on
58% of the events.

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

**Fitted object**: n = **977**, events = **104**.

Right censoring is necessary because the congresses have unequal follow-up: a
2023 abstract has had 28.9 months to publish and a 2012 abstract 160.9. Treating
"not yet published" as "will never publish" would confound the publication rate
with congress recency, which is exactly the artefact visible in the crude rate
by year.

**A caveat the code comment gets backwards.** The comment at
`R/06_analyze_results.R:169-172` says published abstracts without a date are
"excluded (not censored, as they are known events — censoring them would
negatively bias estimates)". Excluding them is not neutral either: 74 known
events are removed while their at-risk time is also removed, so the estimator is
fitted to a population in which the event rate is artificially low. 1,051
evaluated − 74 dropped = 977 at risk. The KM curve therefore understates
cumulative publication.

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

`cox_data <- km_data |> drop_na(all_of(cox_formula_parts))` → **n = 938,
events = 104**. Reference categories are the `FALSE` level for each logical and
`female` for `gender_unified` (alphabetical, R default). `n_authors` enters as a
continuous count.

Results, `output/aim2b_cox_regression.csv`, exponentiated with profile-likelihood
confidence intervals from `broom::tidy(exponentiate = TRUE, conf.int = TRUE)`:

| term | HR | 95% CI | p |
|---|---:|---|---:|
| `is_rctTRUE` | 2.295 | 1.294 – 4.071 | 0.005 |
| `is_academicTRUE` | 0.865 | 0.490 – 1.527 | 0.616 |
| `is_us_basedTRUE` | 1.712 | 1.091 – 2.687 | 0.019 |
| `n_authors` | 1.240 | 1.042 – 1.475 | 0.015 |
| `gender_unifiedmale` | 0.590 | 0.390 – 0.892 | 0.012 |
| `is_multicenterTRUE` | 1.132 | 0.413 – 3.105 | 0.809 |
| `has_fundingTRUE` | 3.516 | 0.480 – 25.741 | 0.216 |

---

## Aim 3 — Predictors of publication (logistic regression)

**Population.** `results |> filter(!is.na(final_published))`, then
`drop_na()` on the model terms → **n = 1,010**.

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
| `(Intercept)` | 0.058 | 0.028 – 0.112 | <0.001 |
| `is_rctTRUE` | 2.244 | 1.273 – 3.856 | 0.004 |
| `log_sample_size` | 0.993 | 0.913 – 1.076 | 0.861 |
| `is_academicTRUE` | 0.860 | 0.502 – 1.424 | 0.569 |
| `is_us_basedTRUE` | 1.313 | 0.893 – 1.941 | 0.169 |
| `n_authors` | 1.325 | 1.147 – 1.543 | <0.001 |
| `gender_unifiedmale` | 0.816 | 0.580 – 1.144 | 0.241 |
| `is_multicenterTRUE` | 1.884 | 0.861 – 3.881 | 0.096 |
| `has_fundingTRUE` | 2.609 | 0.117 – 29.040 | 0.445 |

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

The block is gated on `"result_positivity" %in% names(results)`.
`R/05_adjudicate.R` does not carry that column into
`output/abstracts_with_matches.csv`, so **the block has not run since
2026-04-17**. `output/aim5_publication_bias.csv` is a stale artefact of an
earlier data layout. `result_positivity` still exists upstream in
`data/processed/abstracts_cleaned.csv`.

---

## Sensitivity analyses

`output/sensitivity_analyses.csv`:

| scenario | n | n_published | rate |
|---|---:|---:|---:|
| Definite only | 1,106 | 131 | 11.8% |
| Definite + probable | 1,106 | 212 | 19.2% |
| Definite + reviewer-confirmed | 1,051 | 178 | 16.9% |
| Published within 12 months | 1,051 | 43 | 4.1% |
| Published within 24 months | 1,051 | 77 | 7.3% |
| Published within 36 months | 991 | 80 | 8.1% |
| Published within 48 months | 901 | 68 | 7.5% |

Three things to note before quoting this table:

1. **Rows 1–2 divide by the cohort (1,106); row 3 divides by the evaluated set
   (1,051).** They are not directly comparable to each other.
2. Rows 4–7 count only publications with a known date, so they inherit the
   104-of-178 problem. The apparent *decline* from 8.1% at 36 months to 7.5% at
   48 months is not a real decline — the denominator shrinks to congresses with
   ≥ 48 months of follow-up (2012–2021), which are precisely the years with the
   worst date coverage.
3. `tests/testthat/test-pipeline_semantics.R:306` asserts the window scenarios
   are monotonically ordered, and currently passes only because it compares the
   right pairs.

The follow-up-window rows are the closest thing to a lead-time sensitivity
analysis. There is **no** sensitivity analysis for the two decisions that matter
most: the branch order in `assign_final_published()` (48 abstracts) and the
treatment of the 55 unresolved.

---

## Diagnostics

| Check | Implemented | Result |
|---|---|---|
| Proportional hazards | `cox.zph(cox_model)`, global test written to `output/cox_ph_assumption.csv` | global p = **0.32** — no evidence against PH |
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

1. **Differential predictor ascertainment by congress year.** `is_rct`,
   `is_academic`, `is_us_based`, `sample_size` and `is_multicenter` are all
   derived from text that was unavailable for 2012–2018 when the derivation ran.
   Because the outcome also varies strongly by year (5.6% in 2017 to 33.3% in
   2023), every coefficient in both models is confounded by year through
   measurement. Congress year is **not** in either model. See
   [FAILURE_MODES.md](FAILURE_MODES.md) F3.
2. **`n_authors` is censored at 5** and is the second-strongest term in both
   models.
3. **`gender_unified` is inferred**, 27% of it from a single initial, and 228
   abstracts carry a cross-source disagreement.
4. **74 of 178 events lack a date**, removing them from every time-to-event
   analysis.
