# 06_analyze_results.R — Statistical analysis for all four research aims

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(survival)
library(broom)
library(cli)
library(config)
source(here("R", "utils_congresses.R"))
source(here("R", "utils_decisions.R"))

cfg <- config::get(file = here("config.yml"))

cli_h2("Statistical Analysis")

# Load adjudicated results (incorporate manual review if available)
results <- read_csv(here("output", "abstracts_with_matches.csv"), show_col_types = FALSE)

# Check for manual review decisions
manual_review_path <- here("output", "manual_review_decisions.csv")
if (file.exists(manual_review_path)) {
  decisions <- read_csv(manual_review_path, show_col_types = FALSE)
  cli_alert_info("Incorporating {nrow(decisions)} manual review decisions")

  # Precedence and status assignment live in R/utils_decisions.R so they can be
  # tested directly. See tests/testthat/test-decision_precedence_bva.R and
  # test-decision_mutation.R.
  decisions_deduped <- dedup_decisions_for_analysis(decisions)

  results <- assign_final_published(results, decisions_deduped)
} else {
  cli_alert_warning("No manual review decisions found — using auto-classification only")
  results <- results |>
    mutate(
      final_published = classification == "definite",
      final_pmid = best_pmid
    )
}

# ------------------------------------------------------------------
# Refresh the publication fields against final_pmid.
#
# R/05_adjudicate.R joins publication metadata on best_pmid, and it runs BEFORE
# the reviewer decisions are read. Two consequences it could not avoid:
#
#   1. Where a reviewer supplied a different PMID, the publication fields and
#      months_to_pub still described the algorithm's candidate, not the
#      reviewer's paper (docs/FAILURE_MODES.md F12).
#   2. Where a reviewer confirmed a match on an abstract classified `possible`
#      or `no_match`, 05 had already blanked the publication fields, so a
#      confirmed publication carried no date at all.
#
# Both are fixed here by re-joining on final_pmid. This is a file operation
# against the candidate pool - no network call - so 06 stays offline.
# ------------------------------------------------------------------
candidates_path <- here("data", "processed", "pubmed_candidates.csv")
if (file.exists(candidates_path)) {
  pool <- read_csv(candidates_path, show_col_types = FALSE,
                   col_types = cols(.default = col_character()))

  refreshed <- results |>
    transmute(abstract_id, .fp = as.character(final_pmid)) |>
    filter(!is.na(.fp)) |>
    left_join(
      pool |>
        transmute(abstract_id, .fp = as.character(pmid),
                  .pub_title = pub_title, .pub_journal = pub_journal,
                  .pub_year = pub_year, .pub_month = pub_month,
                  .pub_doi = pub_doi, .pub_first_author = pub_first_author) |>
        distinct(abstract_id, .fp, .keep_all = TRUE),
      by = c("abstract_id", ".fp")
    )

  results <- results |>
    left_join(select(refreshed, -.fp), by = "abstract_id") |>
    mutate(
      .pub_month_num = case_when(
        !is.na(suppressWarnings(as.integer(.pub_month))) ~ as.integer(.pub_month),
        .pub_month %in% month.abb ~ match(.pub_month, month.abb),
        TRUE ~ 1L
      ),
      .pub_date = if_else(!is.na(.pub_year),
                          as.Date(sprintf("%s-%02d-01", .pub_year, .pub_month_num)),
                          as.Date(NA)),
      # Only fill where the outcome is TRUE: a publication field on an abstract
      # counted unpublished would describe a rejected candidate.
      .fill = !is.na(final_published) & final_published & !is.na(.pub_year),
      pub_title        = if_else(.fill, .pub_title, pub_title),
      pub_journal      = if_else(.fill, .pub_journal, pub_journal),
      pub_doi          = if_else(.fill, .pub_doi, pub_doi),
      pub_first_author = if_else(.fill, .pub_first_author, pub_first_author),
      pub_year         = if_else(.fill, as.numeric(.pub_year), as.numeric(pub_year)),
      months_to_pub    = if_else(
        .fill,
        as.numeric(difftime(.pub_date, conference_date_for(congress_year, cfg),
                            units = "days")) / 30.44,
        months_to_pub
      )
    )

  n_pub   <- sum(results$final_published, na.rm = TRUE)
  n_dated <- sum(results$final_published & !is.na(results$months_to_pub), na.rm = TRUE)
  cli_alert_info("Publication dates available for {n_dated}/{n_pub} published abstracts")
  if (n_dated < n_pub) {
    cli_alert_warning("{n_pub - n_dated} published abstract{?s} still lack a \
                       publication date; time-to-event analyses use {n_dated} events")
  }
  results <- results |> select(-starts_with(".pub"), -.fill)
} else {
  cli_alert_warning("No candidate pool found - publication fields not refreshed")
}

# ------------------------------------------------------------------
# One publication credited to more than one abstract.
#
# The publication rate is a per-abstract quantity, and two abstracts from the
# same group can legitimately resolve to one paper - companion analyses merged
# before submission, or a preliminary and a final report of one study. Cochrane
# MR000005 counts these per abstract, so the numerator is NOT deduplicated here.
#
# But a shared PMID is also what a wrong match looks like, so it must be visible
# rather than buried. `final_pmid_shared` flags every published abstract whose
# credited publication is also credited to another, and the count is reported at
# run time. Found by tests/testthat/test-cycle06_scoring_composite.R.
# ------------------------------------------------------------------
shared_pmids <- results |>
  filter(!is.na(final_published), final_published, !is.na(final_pmid)) |>
  count(final_pmid, name = ".n") |>
  filter(.n > 1)

results <- results |>
  mutate(final_pmid_shared = !is.na(final_published) & final_published &
           !is.na(final_pmid) & final_pmid %in% shared_pmids$final_pmid)

if (nrow(shared_pmids) > 0) {
  n_rows <- sum(results$final_pmid_shared)
  cli_alert_warning(
    "{nrow(shared_pmids)} publication{?s} {?is/are} credited to {n_rows} \
     abstracts. The numerator is not deduplicated - see final_pmid_shared."
  )
  results |>
    filter(final_pmid_shared) |>
    select(abstract_id, congress_year, classification, manual_decision,
           final_pmid, title) |>
    arrange(final_pmid) |>
    write_csv(here("output", "shared_publication_matches.csv"))
  cli_alert_info("Detail: output/shared_publication_matches.csv")
}

# Export the fully unified dataset with all demographics + human decisions for external analysis
write_csv(results, here("output", "final_analytical_dataset.csv"))
cli_alert_success("Exported unified dataset to output/final_analytical_dataset.csv")


# ------------------------------------------------------------------
# Subgroup rates, with a guard against stratifiers that are themselves
# a function of the outcome.
#
# practice_type, subspecialty and career_stage are parsed from the MATCHED
# PUBLICATION's PubMed affiliation string, so they can only exist for an
# abstract that has a matched publication: 145 of 178 published abstracts carry
# a practice_type against 21 of 873 unpublished. Grouping the publication rate
# by such a variable returns ~90-100% in every stratum by construction. The
# tables were being written with no indication of this. See
# docs/FAILURE_MODES.md F4.
#
# The rate is still emitted, because it is the correct conditional quantity and
# the manuscript reads these files, but every row now carries the availability
# split that produced it and an explicit flag.
# ------------------------------------------------------------------
subgroup_rate <- function(data, var) {
  present <- !is.na(data[[var]])
  pub <- !is.na(data$final_published) & data$final_published
  unpub <- !is.na(data$final_published) & !data$final_published

  avail_pub   <- if (sum(pub) > 0) mean(present[pub]) else NA_real_
  avail_unpub <- if (sum(unpub) > 0) mean(present[unpub]) else NA_real_
  conditional <- !is.na(avail_pub) && !is.na(avail_unpub) &&
    avail_unpub > 0 && (avail_pub / avail_unpub) > 3

  if (isTRUE(conditional)) {
    cli_alert_warning(
      "{var} is available for {round(avail_pub * 100, 1)}% of published and \
       {round(avail_unpub * 100, 1)}% of unpublished abstracts. It is a \
       function of the outcome; the rates below are conditional on a match \
       having been found and are NOT publication rates."
    )
  }

  data |>
    filter(!is.na(final_published), !is.na(.data[[var]])) |>
    group_by(across(all_of(var))) |>
    summarise(n = n(), n_published = sum(final_published),
              rate = round(mean(final_published) * 100, 1), .groups = "drop") |>
    mutate(
      availability_among_published   = round(avail_pub * 100, 1),
      availability_among_unpublished = round(avail_unpub * 100, 1),
      outcome_conditional_stratifier = conditional
    ) |>
    arrange(desc(rate))
}

# ============================================================
# AIM 1: Publication rate
# ============================================================
cli_h3("Aim 1: Publication Rate")

n_total <- nrow(results)
n_published <- sum(results$final_published, na.rm = TRUE)
n_pending <- sum(is.na(results$final_published))

# Wilson score interval for proportion
n_evaluated <- n_total - n_pending
if (n_evaluated == 0) {
  cli_alert_warning("All abstracts still pending review — cannot compute publication rate")
  pub_rate <- NA_real_
  prop_test <- list(conf.int = c(NA_real_, NA_real_))
} else {
  prop_test <- prop.test(n_published, n_evaluated, correct = FALSE)
  pub_rate <- n_published / n_evaluated
}

aim1 <- tibble::tibble(
  metric = c("total_abstracts", "n_evaluated", "published", "not_published",
             "pending_review", "publication_rate", "ci_lower", "ci_upper"),
  value = c(n_total, n_evaluated, n_published, n_total - n_pending - n_published, n_pending,
            round(pub_rate * 100, 1),
            round(prop_test$conf.int[1] * 100, 1),
            round(prop_test$conf.int[2] * 100, 1))
)

cli_alert_info("Publication rate: {round(pub_rate*100,1)}% ({n_published}/{n_total - n_pending})")
cli_alert_info("95% CI: [{round(prop_test$conf.int[1]*100,1)}%, {round(prop_test$conf.int[2]*100,1)}%]")

# By category (if available)
if ("category" %in% names(results)) {
  aim1_by_cat <- results |>
    filter(!is.na(final_published)) |>
    group_by(category) |>
    summarise(
      n = n(),
      n_published = sum(final_published),
      rate = round(mean(final_published) * 100, 1),
      .groups = "drop"
    )
  write_csv(aim1_by_cat, here("output", "aim1_by_category.csv"))
}

# Publication-type breakdown of published abstracts
if ("pub_type_canonical" %in% names(results)) {
  aim1_by_pub_type <- results |>
    filter(isTRUE(final_published) | final_published == TRUE) |>
    count(pub_type_canonical, name = "n") |>
    arrange(desc(n))
  write_csv(aim1_by_pub_type, here("output", "aim1_by_pub_type.csv"))
  cli_alert_info("Publication-type breakdown of matched publications:")
  print(aim1_by_pub_type)
}

# Publication rate by practice type
if ("practice_type" %in% names(results)) {
  aim1_by_practice <- subgroup_rate(results, "practice_type")
  write_csv(aim1_by_practice, here("output", "aim1_by_practice_type.csv"))
  cli_alert_info("Publication rate by practice type:")
  print(aim1_by_practice)
}

# Publication rate by subspecialty
if ("subspecialty" %in% names(results)) {
  aim1_by_subspec <- subgroup_rate(results, "subspecialty")
  write_csv(aim1_by_subspec, here("output", "aim1_by_subspecialty.csv"))
  cli_alert_info("Publication rate by subspecialty:")
  print(aim1_by_subspec)
}

# Publication rate by congress year
if ("congress_year" %in% names(results)) {
  aim1_by_year <- results |>
    filter(!is.na(final_published)) |>
    group_by(congress_year) |>
    summarise(
      n = n(),
      n_published = sum(final_published),
      rate = round(mean(final_published) * 100, 1),
      .groups = "drop"
    )
  write_csv(aim1_by_year, here("output", "aim1_by_congress_year.csv"))
  cli_alert_info("Publication rate by congress year:")
  print(aim1_by_year)
}

# ============================================================
# AIM 2: Time to publication
# ============================================================
cli_h3("Aim 2: Time to Publication")

published <- results |> filter(final_published)

if (nrow(published) > 0 && "months_to_pub" %in% names(published)) {
  # Time to publication is only defined for a publication that follows the
  # congress. A handful of confirmed publications appeared shortly BEFORE their
  # meeting - four are pre-conference candidates a reviewer confirmed anyway,
  # and one is an online-first paper that appeared two weeks before the 2015
  # congress. They belong in the numerator, because a reviewer ruled they are
  # the abstract's publication, but a negative interval is not a time to
  # publication and must not enter the median or the IQR.
  dated  <- published$months_to_pub[!is.na(published$months_to_pub)]
  ttp    <- dated[dated > 0]
  n_pre  <- sum(dated <= 0)
  n_none <- sum(is.na(published$months_to_pub))

  if (n_pre > 0) {
    cli_alert_info("{n_pre} confirmed publication{?s} predate their congress and \
                    are excluded from the time-to-publication summary")
  }
  if (n_none > 0) {
    cli_alert_warning("{n_none} confirmed publication{?s} have no resolvable date")
  }

  aim2 <- tibble::tibble(
    metric = c("n_published", "n_with_dates", "n_pre_congress", "n_undated",
               "median_months", "q1_months", "q3_months",
               "mean_months", "min_months", "max_months"),
    value = c(nrow(published), length(ttp), n_pre, n_none,
              round(median(ttp), 1), round(quantile(ttp, 0.25), 1),
              round(quantile(ttp, 0.75), 1), round(mean(ttp), 1),
              round(min(ttp), 1), round(max(ttp), 1))
  )

  cli_alert_info("Median time to publication: {round(median(ttp), 1)} months (IQR: {round(quantile(ttp, 0.25), 1)}-{round(quantile(ttp, 0.75), 1)}) on {length(ttp)}/{nrow(published)} published")

  # Kaplan-Meier analysis
  # Create survival object: event = publication, time = months since conference
  # Censored = not published by end of search window
  # KM data: published abstracts WITH a date are events at their time.
  # Published abstracts WITHOUT a date are excluded (not censored, as they
  # are known events — censoring them would negatively bias estimates).
  # Unpublished abstracts are censored at end of search window.
  km_data <- results |>
    filter(!is.na(final_published)) |>
    mutate(
      censor_time = as.numeric(difftime(as.Date(cfg$pubmed$date_end, "%Y/%m/%d"),
                                        conference_date_for(congress_year, cfg),
                                        units = "days")) / 30.44,
      time = case_when(
        final_published & !is.na(months_to_pub) ~ months_to_pub,  # event with known time
        !final_published ~ censor_time,                            # censored
        TRUE ~ NA_real_                                            # published but no date — exclude
      ),
      event = as.integer(final_published)
    ) |>
    filter(!is.na(time), time > 0)

  if (nrow(km_data) > 0) {
    km_fit <- survfit(Surv(time, event) ~ 1, data = km_data)
    km_summary <- summary(km_fit)
    saveRDS(km_fit, here("data", "processed", "km_fit.rds"))
    cli_alert_success("Kaplan-Meier model saved")

    # Cox proportional hazards model (Cochrane MR000005 requirement)
    cox_vars <- intersect(
      c("is_rct", "log_sample_size", "is_academic", "is_us_based",
        "session_type", "n_authors", "gender_unified",
        "practice_type", "is_multicenter", "has_funding"),
      names(km_data)
    )
    # Only include variables with >=2 levels and <50% missing
    cox_formula_parts <- character()
    for (v in cox_vars) {
      vals <- km_data[[v]]
      if (is.null(vals)) next
      pct_na <- mean(is.na(vals))
      n_levels <- length(unique(vals[!is.na(vals)]))
      if (pct_na < 0.5 && n_levels >= 2) cox_formula_parts <- c(cox_formula_parts, v)
    }

    if (length(cox_formula_parts) >= 2) {
      cox_formula <- as.formula(paste("Surv(time, event) ~",
                                       paste(cox_formula_parts, collapse = " + ")))
      cox_data <- km_data |> tidyr::drop_na(all_of(cox_formula_parts))

      if (nrow(cox_data) >= 30) {
        cox_model <- tryCatch(
          coxph(cox_formula, data = cox_data),
          error = function(e) { cli_alert_warning("Cox PH failed: {e$message}"); NULL }
        )
        if (!is.null(cox_model)) {
          cox_tidy <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE) |>
            mutate(across(where(is.numeric), ~ round(.x, 3)))
          write_csv(cox_tidy, here("output", "aim2b_cox_regression.csv"))
          saveRDS(cox_model, here("data", "processed", "cox_model.rds"))
          cli_alert_success("Cox PH model saved ({nrow(cox_tidy)} terms)")

          # Test proportional hazards assumption
          ph_test <- tryCatch(cox.zph(cox_model), error = function(e) NULL)
          if (!is.null(ph_test)) {
            ph_global_p <- ph_test$table["GLOBAL", "p"]
            if (ph_global_p < 0.05) {
              cli_alert_warning("PH assumption may be violated (global p = {round(ph_global_p, 3)})")
            } else {
              cli_alert_success("PH assumption holds (global p = {round(ph_global_p, 3)})")
            }
            # Save PH test p-value for reproducible reporting
            write_csv(tibble(test = "cox_zph_global", p_value = round(ph_global_p, 3)),
                      here("output", "cox_ph_assumption.csv"))
          }
        }
      } else {
        cli_alert_warning("Too few complete cases ({nrow(cox_data)}) for Cox PH")
      }
    }
  }
} else {
  cli_alert_warning("No published articles with dates to analyze")
  aim2 <- tibble::tibble(metric = "n_with_dates", value = 0)
}

# ============================================================
# AIM 3: Predictors of publication
# ============================================================
cli_h3("Aim 3: Predictors of Publication")

model_data <- results |>
  filter(!is.na(final_published)) |>
  mutate(
    published_int = as.integer(final_published),
    log_sample_size = log1p(coalesce(sample_size, 0))
  )

if (nrow(model_data) >= 20 && length(unique(model_data$published_int)) >= 2) {
  # Expanded logistic regression (Cochrane MR000005: include presentation type,
  # author demographics, result direction, and temporal effects)
  extra_vars <- intersect(
    c("session_type", "n_authors", "gender_unified",
      "practice_type", "is_multicenter", "has_funding", "subspecialty"),
    names(model_data)
  )
  # Filter to variables with enough variation
  usable_extras <- character()
  for (v in extra_vars) {
    vals <- model_data[[v]]
    if (!is.null(vals) && mean(is.na(vals)) < 0.5 &&
        length(unique(vals[!is.na(vals)])) >= 2) usable_extras <- c(usable_extras, v)
  }
  logit_formula <- as.formula(paste(
    "published_int ~ is_rct + log_sample_size + is_academic + is_us_based",
    if (length(usable_extras) > 0) paste("+", paste(usable_extras, collapse = " + ")) else ""
  ))
  model_data_complete <- model_data |> tidyr::drop_na(all_of(c("is_rct", "is_academic", "is_us_based", usable_extras)))

  model <- glm(
    logit_formula,
    data = model_data_complete,
    family = binomial(link = "logit")
  )

  model_tidy <- tryCatch(
    tidy(model, exponentiate = TRUE, conf.int = TRUE),
    error = function(e) {
      cli_alert_warning("Confidence intervals failed ({conditionMessage(e)}); returning point estimates only")
      tidy(model, exponentiate = TRUE, conf.int = FALSE)
    }
  )
  # The model is complete-case, so its N is smaller than the publication-rate
  # denominator and cannot be recovered from a coefficient table alone. Carry it
  # on every row so an odds ratio is never reported without the sample it was
  # estimated from. Caught by tests/testthat/test-cycle03_model_contracts.R.
  aim3 <- model_tidy |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    mutate(n_obs = stats::nobs(model))

  write_csv(aim3, here("output", "aim3_logistic_regression.csv"))
  saveRDS(model, here("data", "processed", "logistic_model.rds"))

  cli_alert_success("Logistic regression complete")
  print(aim3)
} else {
  cli_alert_warning("Insufficient data or no outcome variation for logistic regression (n = {nrow(model_data)}, unique outcomes = {length(unique(model_data$published_int))})")
  aim3 <- tibble::tibble(term = "insufficient_data", estimate = NA)
}

# ============================================================
# AIM 4: Search strategy performance
# ============================================================
cli_h3("Aim 4: Search Strategy Performance")

# For accepted matches, which strategies/sources found the winning PMID?
accepted_pmids <- results |>
  filter(final_published) |>
  mutate(final_pmid = as.character(final_pmid)) |>
  select(abstract_id, final_pmid)

# --- PubMed strategy performance ---
strategy_path <- here("data", "processed", "pubmed_strategy_results.csv")
candidates_path <- here("data", "processed", "pubmed_candidates.csv")

aim4_pubmed <- NULL
if (file.exists(strategy_path) && file.exists(candidates_path)) {
  strategy_results <- read_csv(strategy_path, show_col_types = FALSE)
  candidates <- read_csv(candidates_path, show_col_types = FALSE)

  strategy_contribution <- candidates |>
    mutate(pmid = as.character(pmid)) |>
    inner_join(accepted_pmids, by = c("abstract_id", "pmid" = "final_pmid")) |>
    mutate(strategy_list = strsplit(strategies, ";\\s*")) |>
    unnest(strategy_list) |>
    count(strategy_list, name = "n_found_correct") |>
    rename(strategy = strategy_list)

  aim4_pubmed <- strategy_results |>
    group_by(strategy) |>
    summarise(
      n_searched = n(),
      n_with_hits = sum(n_results > 0),
      yield_pct = round(mean(n_results > 0) * 100, 1),
      .groups = "drop"
    ) |>
    left_join(
      strategy_contribution |>
        mutate(pct_found = round(n_found_correct / nrow(accepted_pmids) * 100, 1)) |>
        select(strategy, n_found_correct, pct_found),
      by = "strategy"
    ) |>
    mutate(source = "PubMed")
}

# --- Supplementary API performance ---
supp_apis <- list(
  list(file = "crossref_candidates.csv", name = "CrossRef", pmid_col = NULL, doi_col = "doi"),
  list(file = "europmc_candidates.csv", name = "Europe PMC", pmid_col = "pmid", doi_col = "doi"),
  list(file = "openalex_candidates.csv", name = "OpenAlex", pmid_col = "pmid", doi_col = "doi"),
  list(file = "semantic_scholar_candidates.csv", name = "Semantic Scholar", pmid_col = "pmid", doi_col = "doi")
)

aim4_supp <- purrr::map(supp_apis, function(api) {
  fpath <- here("data", "processed", api$file)
  if (!file.exists(fpath)) return(NULL)
  cands <- read_csv(fpath, show_col_types = FALSE)

  n_total <- nrow(results)
  n_abstracts_with_hits <- length(unique(cands$abstract_id))
  n_candidates <- nrow(cands)

  # Check if this source found the winning PMID for any accepted match
  n_found <- 0L
  if (!is.null(api$pmid_col) && api$pmid_col %in% names(cands)) {
    cands_pmid <- cands[[api$pmid_col]]
    if (!is.null(cands_pmid)) {
      matched <- cands |>
        mutate(.pmid = as.character(.data[[api$pmid_col]])) |>
        inner_join(accepted_pmids, by = c("abstract_id", ".pmid" = "final_pmid"))
      n_found <- nrow(matched)
    }
  }

  tibble::tibble(
    strategy = api$name,
    n_searched = n_total,
    n_with_hits = n_abstracts_with_hits,
    yield_pct = round(n_abstracts_with_hits / n_total * 100, 1),
    n_found_correct = if (n_found > 0) n_found else NA_integer_,
    pct_found = if (n_found > 0) round(n_found / nrow(accepted_pmids) * 100, 1) else NA_real_,
    source = "Supplementary"
  )
}) |> purrr::list_rbind()

aim4 <- bind_rows(aim4_pubmed, aim4_supp)
write_csv(aim4, here("output", "aim4_strategy_performance.csv"))
cli_alert_success("Strategy ablation analysis complete")

# ============================================================
# Save all aim summaries
# ============================================================
write_csv(aim1, here("output", "aim1_publication_rate.csv"))
write_csv(aim2, here("output", "aim2_time_to_pub.csv"))
if (exists("aim3")) write_csv(aim3, here("output", "aim3_logistic_regression.csv"))

# ============================================================
# AIM 5: Publication bias by result direction (Cochrane MR000005)
# ============================================================
cli_h3("Aim 5: Publication Bias by Result Direction")

if ("result_positivity" %in% names(results)) {
  bias_data <- results |>
    filter(!is.na(final_published), result_positivity %in% c("positive", "negative", "neutral")) |>
    mutate(result_positivity = factor(result_positivity, levels = c("negative", "neutral", "positive")))

  if (nrow(bias_data) >= 10 && length(unique(bias_data$result_positivity)) >= 2) {
    bias_tab <- bias_data |>
      group_by(result_positivity) |>
      summarise(
        n = n(),
        n_published = sum(final_published),
        rate = round(mean(final_published) * 100, 1),
        .groups = "drop"
      )

    # Positive vs negative OR
    pos_neg <- bias_data |> filter(result_positivity %in% c("positive", "negative"))
    if (nrow(pos_neg) >= 5 && length(unique(pos_neg$result_positivity)) == 2) {
      or_model <- tryCatch({
        glm(final_published ~ result_positivity, data = pos_neg, family = binomial)
      }, error = function(e) NULL)
      if (!is.null(or_model)) {
        or_tidy <- broom::tidy(or_model, exponentiate = TRUE, conf.int = TRUE) |>
          filter(term != "(Intercept)") |>
          mutate(across(where(is.numeric), ~ round(.x, 3)))
        bias_tab <- bind_rows(bias_tab, tibble(
          result_positivity = "positive_vs_negative_OR",
          n = nrow(pos_neg),
          n_published = NA_integer_,
          rate = or_tidy$estimate[1]
        ))
      }
    }

    write_csv(bias_tab, here("output", "aim5_publication_bias.csv"))
    cli_alert_success("Publication-bias analysis saved")
    print(bias_tab)
  } else {
    cli_alert_warning("Not enough result-positivity data for bias analysis")
  }
} else {
  cli_alert_warning("result_positivity column not found — run 02_clean_abstracts.R first")
}

# ============================================================
# SENSITIVITY ANALYSES (Cochrane MR000005 requirement)
# ============================================================
cli_h3("Sensitivity Analyses")

sens_rows <- list()

# Definite-only publication rate
n_def <- sum(results$classification == "definite", na.rm = TRUE)
sens_rows[[1]] <- tibble(scenario = "Definite only",
                         denominator = "cohort",
                         n = n_total, n_published = n_def,
                         rate = round(n_def / n_total * 100, 1))

# Definite + probable (treats all probable as published)
n_prob <- sum(results$classification %in% c("definite", "probable"), na.rm = TRUE)
sens_rows[[2]] <- tibble(scenario = "Definite + probable",
                         denominator = "cohort",
                         n = n_total, n_published = n_prob,
                         rate = round(n_prob / n_total * 100, 1))

# Definite + reviewer-confirmed
n_confirmed <- sum(results$final_published, na.rm = TRUE)
# Scenarios 1-2 are decidable without a reviewer, so they divide by the cohort.
# Scenario 3 onward require an adjudicated outcome and divide by the evaluated
# set. The `denominator` column states which, because a reader comparing 11.8%
# with 16.9% is otherwise comparing two different populations.
sens_rows[[3]] <- tibble(scenario = "Definite + reviewer-confirmed",
                         denominator = "evaluated",
                         n = n_total - sum(is.na(results$final_published)),
                         n_published = n_confirmed,
                         rate = round(n_confirmed / (n_total - sum(is.na(results$final_published))) * 100, 1))

# Follow-up window sensitivity (only for abstracts with enough follow-up)
for (window_mo in c(12, 24, 36, 48)) {
  sub <- results |>
    filter(!is.na(final_published)) |>
    mutate(
      has_window = as.numeric(difftime(as.Date(cfg$pubmed$date_end, "%Y/%m/%d"),
                                        conference_date_for(congress_year, cfg),
                                        units = "days")) / 30.44 >= window_mo,
      pub_in_window = final_published & !is.na(months_to_pub) & months_to_pub <= window_mo
    ) |>
    filter(has_window)
  if (nrow(sub) > 0) {
    sens_rows[[length(sens_rows) + 1]] <- tibble(
      scenario = paste0("Published within ", window_mo, " months"),
      denominator = "evaluated with sufficient follow-up",
      n = nrow(sub), n_published = sum(sub$pub_in_window),
      rate = round(mean(sub$pub_in_window) * 100, 1))
  }
}

sensitivity <- bind_rows(sens_rows)
write_csv(sensitivity, here("output", "sensitivity_analyses.csv"))
cli_alert_success("Sensitivity analyses saved ({nrow(sensitivity)} scenarios)")
print(sensitivity)

cli_alert_success("Analysis complete — results in output/")
