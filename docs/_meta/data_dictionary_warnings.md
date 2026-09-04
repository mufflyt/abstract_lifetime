## Columns that carry a warning

| Variable | Problem |
|---|---|
| `author_count`, `n_authors`, `n_authors_aagl` | Censored at 5 by ScienceDirect's author-list truncation (336 of 1,106 lists end in an ellipsis). `n_authors` is nevertheless a predictor in both the Cox and logistic models. |
| `keyword_pts` | Zero for every scored abstract. The score is functionally nine-component. |
| `abstract_word_count` | Was zero for all 700 abstracts from 2012–2018; recomputed after the text backfill by `R/02d_rederive_predictors.R`. |
| `has_numeric_results`, `has_irb_statement`, `has_trial_registration` | Were zero for all of 2012–2018 because they read only the structured section columns. `02d` now falls back to `abstract_text`. Still zero for 2017 and 2018, which have no recoverable text at all. |
| `is_us_based`, `is_academic`, `sample_size`, `study_design`, `is_multicenter`, `is_rct` | Had a strong artefactual gradient by congress year; re-derived from the backfilled text by `02d`. A residual gradient remains for 2017 and 2018, whose abstracts have no text. See FAILURE_MODES.md F3. |
| `subspecialty_unified` | 13 levels representing about 8 concepts (`MIG` vs `MIGS`, `FPMRS` vs the spelled-out label, `general_OBGYN` vs `Generalist`). Two vocabularies coalesced without harmonisation. |
| `state_unified` | Same problem, 40 levels from two encodings. |
| `career_stage` (3 rows), `orcid_subspecialty` (2 rows), `orcid_career_stage` (28 rows) | Coverage too low to analyse. |
| `orcid_false_positive` | Single-valued across all 1,102 non-`NA` rows; carries no information as shipped. |
| `gender_unified` | Inferred from given names, never self-reported. 228 abstracts carry a cross-source disagreement in `gender_conflict`. |
| `pub_*`, `months_to_pub` | Now populated for all 178 published abstracts (was 104) after `scripts/rebuild_candidate_pool.R` restored the candidate pool. Seven carry a negative `months_to_pub`: confirmed publications that appeared before their congress. |
| `final_pmid` | Populated for 1,102 rows including abstracts where `final_published` is `FALSE`. It is the best candidate, not a confirmed publication. |
