<!-- GENERATED FILE. Do not edit by hand.
     Source: tests/expected_failures.yaml
     Regenerate: Rscript R/generate_decisions_pending.R -->

# Decisions pending

This repository keeps 16 tests failing on purpose. Each one below is a question that code cannot answer: resolving it changes the estimand, the cohort, or an adjudication that a human already recorded. None is a defect awaiting a fix.

CI is green while exactly these fail. If one of them starts passing, CI goes red until its entry is removed, so this list cannot quietly outlive its reasons.

## How to use this

Work an item by deciding the question in **Decision needed**, then either make the change and delete the entry from `tests/expected_failures.yaml`, or record why the current behaviour stands and leave it. Regenerate this file afterwards.

## test-cycle04_validation_sensitivity.R

### 1. no search strategy is silently contributing nothing

**What fails.** output/search_strategy_efficacy.csv still records the title strategy hitting 3 of 1,742 queries (0.2%). That is a pre-correction measurement: the April 2026 fix to the title phrase search invalidated it.

**Decision needed.** Regenerating it means re-running the whole search layer, which would change candidate sets and invalidate the human adjudication.

**Documented in.** docs/PUBLICATION_SEARCH.md section 8

## test-cycle06_scoring_composite.R

### 2. no scoring component is structurally dead

**What fails.** keyword_pts is 0 for all 1,106 abstracts, so the composite described as ten-component has nine live components. The abstract side holds TF tokens and the PubMed side holds MeSH-style phrases; they never intersect at the required threshold of three.

**Decision needed.** Repairing the component changes every composite score and therefore every classification, which invalidates the human adjudication recorded against the current scores.

**Documented in.** docs/MATCHING_ALGORITHM.md section 1

### 3. no PMID is counted as the publication of more than one abstract

**What fails.** Three PMIDs are each credited to two published abstracts, so six of the 178 numerator rows rest on three papers. One of the three is counted published against an explicit reviewer no_match.

**Decision needed.** Deciding which abstract owns each PMID is adjudication, not code. The evidence is in output/shared_publication_matches.csv.

**Documented in.** docs/FAILURE_MODES.md F17

## test-cycle11_authors_and_queue.R

### 4. no author variable piles up at a hard ceiling

**What fails.** n_authors caps at 5 with 532 of 1,106 rows (48.1%) sitting exactly at the cap; author_count caps at 5 with 197 (17.8%). Half the mass on the maximum is the signature of a display cap, not a distribution. authors_truncated confirms the mechanism: the ScienceDirect listing elides long author lists and the parser counts only what is visible.

**Decision needed.** Recover full author lists at ingestion, model team size as censored, or stop reporting a per-author effect. All three change what the manuscript may claim.

**Documented in.** tests/loop/LEDGER.md cycle 11

### 5. the team-size predictor spans a usable range

**What fails.** Same censoring as above, stated against the reported coefficient. aim3_logistic_regression.csv reports n_authors at OR 1.325 per author, p < 0.001, and the draft abstract calls that a significant team-size effect. The coefficient is estimated over a variable that cannot exceed 5. Unlike the funding term, this one is significant and headline-reported.

**Decision needed.** Same decision as the entry above; both clear together.

**Documented in.** tests/loop/LEDGER.md cycle 11

## test-cycle12_covariate_integrity.R

### 6. every aim3 term maps to a column in the exported dataset

**What fails.** log_sample_size is a reported model term with no column in final_analytical_dataset.csv. It is derived inside 06_analyze_results.R and never written out, so nobody holding the published dataset can reproduce, check or correct the model.

**Decision needed.** Export the derived term, or drop it from the reported model. Not fixed by the loop because 06_analyze_results.R was mid-edit by another author.

**Documented in.** tests/loop/LEDGER.md cycle 12

### 7. is_us_based agrees with first_author_country

**What fails.** STALENESS TRACKER, not an open question. The root cause was fixed: parse_affiliation() took the last comma token of an affiliation as the country, yielding US states. It now calls parse_country(), and 09c re-derives the column. author_characteristics.csv and abstracts_with_matches.csv are clean; final_analytical_dataset.csv still carries 18 states because 06_analyze_results.R has not re-run.

**Decision needed.** None. Remove this entry once 06_analyze_results.R runs and the assertion starts passing.

**Documented in.** tests/loop/LEDGER.md cycle 12

### 8. model covariate missingness does not vary sharply by congress year

**What fails.** sample_size missingness ranges from 13.3% (2013) to 92.6% (2018) across congresses. The logistic model deletes rows with any missing covariate and log_sample_size is a term, so 2017 and 2018 are almost entirely absent from the model that estimates the predictors. 2018 also carries the highest reported publication rate.

**Decision needed.** Improve sample-size extraction for the affected congresses, drop the term, or report the model cohort's year composition as a limitation.

**Documented in.** tests/loop/LEDGER.md cycle 12

## test-cycle13_enrichment_quality.R

### 9. practice_type and career_stage emit only documented values

**What fails.** career_stage emits "faculty_senior" while orcid_career_stage emits "senior_faculty". Two vocabularies for one concept, so any code joining or comparing them treats them as different levels.

**Decision needed.** Pick one spelling and migrate the other.

**Documented in.** tests/loop/LEDGER.md cycle 13

### 10. orcid_false_positive is a live flag rather than a constant

**What fails.** orcid_false_positive is FALSE on all 1,102 rows it covers. A flag that never fires gives the same answer as no flag, so the ORCID false-positive check cannot currently distinguish anything.

**Decision needed.** Establish whether the detector is unreachable or simply never written, then either fix it or remove the column so it does not imply a check that is not happening.

**Documented in.** tests/loop/LEDGER.md cycle 13

### 11. career_stage resolves for a usable share of the cohort

**What fails.** career_stage resolves 15 of 1,106 rows (1.4%) even after the country fix improved every other classifier. subspecialty reaches 53% and ACOG district 88% on the same affiliations, so the input is not the limit.

**Decision needed.** Improve the classifier or drop the column. At 1.4% its presence implies a coverage it does not have.

**Documented in.** tests/loop/LEDGER.md cycle 13

### 12. no enrichment column is wholly missing or single-valued

**What fails.** orcid_subspecialty is the constant "obstetrics" on every row it covers, and orcid_false_positive is the constant FALSE. Neither carries information, but both appear in the published dataset as if they do.

**Decision needed.** Fix the two enrichment paths or remove the columns.

**Documented in.** tests/loop/LEDGER.md cycle 13

## test-cycle14_text_flags_and_tables.R

### 13. abstract_word_count is zero only for abstracts with no text

**What fails.** 280 of 1,106 abstracts (25.3%) carry no abstract text at all: abstract_text, abstract_objective and abstract_conclusion are all empty. Only 4 are the withdrawn abstracts. The loss concentrates almost entirely in two congresses, 2017 (97 of 90 evaluated) and 2018 (95 of 95), with the remainder spread thinly over 2012-2016. Every text-derived flag on those rows is a false negative rather than a measurement, abstract_pts can never contribute to their match score, and this is the mechanism behind the sample_size missingness recorded at cycle 12 (86.7% in 2017, 92.6% in 2018). Checked and NOT supported: this does not explain the year-over-year publication rate pattern. The correlation between percent-no-text and publication rate is -0.17, and the two fully text-free congresses sit at opposite extremes (2017 at 5.6%, 2018 at 27.4%).

**Decision needed.** Recover the 2017 and 2018 abstract bodies at ingestion, or exclude text-derived covariates for congresses where the text was never captured. Reporting them as measured FALSE is the one option that is not defensible.

**Documented in.** tests/loop/LEDGER.md cycle 14

### 14. stat_sig_reported implies has_numeric_results

**What fails.** 11 abstracts are flagged as reporting statistical significance while also being flagged as carrying no numeric results. An abstract cannot state that a result reached significance without presenting a number, so the two extractors disagree about the same text and at least one is wrong on those rows.

**Decision needed.** Determine which extractor is at fault and reconcile them, or document that the two flags are independently derived and may disagree.

**Documented in.** tests/loop/LEDGER.md cycle 14

## test-cycle15_backfill_contract.R

### 15. every eligible abstract was at least attempted by the backfill

**What fails.** R/02b_backfill_abstract_text.R exists to repair exactly the gap cycle 14 measured, and all 280 text-free abstracts are eligible for it: every one has a usable DOI. Only 15 were ever fetched. The other 265 have no cached XML under the DOI-derived key the fetcher uses, so they were never attempted. The stage stopped at 5.4% of its own workload. Worth knowing before anyone finishes it: none of the 15 that WERE fetched returned an <AbstractText> element, so PubMed may simply not hold abstract bodies for these supplement entries. Completing the backfill may recover little. That is an argument for running it and finding out, not for leaving 265 rows in an unknown state.

**Decision needed.** Re-run 02b to completion and record how many of the 280 PubMed can actually supply, or document that the text is unrecoverable and treat the text-derived flags for 2012-2018 as missing rather than FALSE.

**Documented in.** tests/loop/LEDGER.md cycle 15

## test-cycle17_search_layer.R

### 16. every search strategy the builder defines can actually be built

**What fails.** build_search_strategies() at R/utils_pubmed.R:429 defines six strategies. Only five ever ran. author_keywords is built only when abstract_row$keywords is non-empty, and abstracts_cleaned.csv has no keywords column at all, so the branch is unreachable for every abstract in the cohort and that strategy searched for nothing. This is the same dead keyword pathway cycle 6 found in the scoring composite, appearing here one stage earlier in the search.

**Decision needed.** Extracting keywords and enabling the strategy would widen every candidate set, which changes the scored pairs and therefore invalidates the human adjudication recorded against the current candidates. Whether the candidate pool is re-derived is the author's call, not the code's.

**Documented in.** tests/loop/LEDGER.md cycle 17

