# validation_gold_standard.R — Gold standard validation of matching algorithm

library(here)
library(readr)
library(dplyr)
library(cli)
library(config)

cfg <- config::get(file = here("config.yml"))

cli_h2("Gold Standard Validation")

# Load gold standard
gs_path <- here("data", "validation", "gold_standard.csv")
if (!file.exists(gs_path)) {
  stop("Gold standard file not found: ", gs_path)
}
gold <- read_csv(gs_path, show_col_types = FALSE)

if (nrow(gold) == 0) {
  cli_alert_warning("Gold standard CSV is empty.")
  cli_alert_info("To use validation, manually label {cfg$validation$gold_standard_n} abstracts:")
  cli_alert_info("  1. Select ~{cfg$validation$gold_standard_n} abstracts (stratified by expected outcome)")
  cli_alert_info("  2. Search PubMed/Google Scholar manually for each")
  cli_alert_info("  3. Record results in data/validation/gold_standard.csv")
  cli_alert_info("  4. Fields: abstract_id, title, first_author, verified_published (TRUE/FALSE),")
  cli_alert_info("     verified_pmid, verified_doi, verified_journal, verified_pub_date, notes")
  cli_alert_info("  5. Re-run this script")
  stop("Gold standard needs manual annotation first.")
}

cli_alert_info("Gold standard: {nrow(gold)} abstracts")

# Load algorithm predictions
scores_path <- here("data", "processed", "match_scores.csv")
if (!file.exists(scores_path)) {
  stop("Match scores not found. Run 04_score_matches.R first.")
}
predictions <- read_csv(scores_path, show_col_types = FALSE)

# Join
validation <- gold |>
  left_join(predictions |> select(abstract_id, best_pmid, best_score, classification),
            by = "abstract_id")

if (nrow(validation) == 0) {
  stop("No matching abstract_ids between gold standard and predictions.")
}

# Binary classification
validation <- validation |>
  mutate(
    truth = verified_published,
    predicted = classification == "definite" |
      (classification %in% c("probable", "possible") & !is.na(best_pmid)),
    # For PMID-level accuracy
    correct_pmid = case_when(
      !truth & !predicted ~ TRUE,  # True negative
      # NA == NA returns NA in R, not TRUE — handle explicitly
      truth & predicted & is.na(best_pmid) & is.na(verified_pmid) ~ FALSE,
      truth & predicted & !is.na(best_pmid) & !is.na(verified_pmid) &
        best_pmid == verified_pmid ~ TRUE,  # Correct match
      truth & predicted & is.na(verified_pmid) ~ NA,  # Can't verify
      TRUE ~ FALSE
    )
  )

# Confusion matrix
tp <- sum(validation$truth & validation$predicted, na.rm = TRUE)
fp <- sum(!validation$truth & validation$predicted, na.rm = TRUE)
fn <- sum(validation$truth & !validation$predicted, na.rm = TRUE)
tn <- sum(!validation$truth & !validation$predicted, na.rm = TRUE)

sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
ppv <- tp / (tp + fp)
npv <- tn / (tn + fn)
accuracy <- (tp + tn) / nrow(validation)

metrics <- tibble::tibble(
  metric = c("n", "true_positives", "false_positives", "false_negatives", "true_negatives",
             "sensitivity", "specificity", "ppv", "npv", "accuracy"),
  value = c(nrow(validation), tp, fp, fn, tn,
            round(sensitivity, 3), round(specificity, 3),
            round(ppv, 3), round(npv, 3), round(accuracy, 3))
)

cli_h3("Validation Results")
cli_alert_info("Sensitivity (recall): {round(sensitivity * 100, 1)}%")
cli_alert_info("Specificity: {round(specificity * 100, 1)}%")
cli_alert_info("PPV (precision): {round(ppv * 100, 1)}%")
cli_alert_info("NPV: {round(npv * 100, 1)}%")
cli_alert_info("Accuracy: {round(accuracy * 100, 1)}%")

if (sensitivity >= cfg$validation$target_sensitivity) {
  cli_alert_success("Sensitivity meets target (>= {cfg$validation$target_sensitivity * 100}%)")
} else {
  cli_alert_danger("Sensitivity below target ({round(sensitivity * 100, 1)}% < {cfg$validation$target_sensitivity * 100}%)")
  cli_alert_info("Consider lowering auto-accept threshold or reviewing scoring weights")
}

# PMID-level accuracy
if (sum(!is.na(validation$correct_pmid)) > 0) {
  pmid_accuracy <- mean(validation$correct_pmid, na.rm = TRUE)
  cli_alert_info("PMID-level accuracy: {round(pmid_accuracy * 100, 1)}%")
}

# Save
write_csv(metrics, here("output", "validation_metrics.csv"))
write_csv(validation, here("data", "validation", "gold_standard_results.csv"))

# Threshold tuning suggestions
if (sensitivity < cfg$validation$target_sensitivity) {
  cli_h3("Threshold Tuning")
  # Test different thresholds
  thresholds <- seq(3, 10, by = 0.5)
  tuning <- purrr::map(thresholds, function(thresh) {
    pred <- validation$best_score >= thresh
    tp_t <- sum(validation$truth & pred, na.rm = TRUE)
    fn_t <- sum(validation$truth & !pred, na.rm = TRUE)
    fp_t <- sum(!validation$truth & pred, na.rm = TRUE)
    tn_t <- sum(!validation$truth & !pred, na.rm = TRUE)
    tibble::tibble(
      threshold = thresh,
      sensitivity = tp_t / max(tp_t + fn_t, 1),
      specificity = tn_t / max(tn_t + fp_t, 1),
      f1 = 2 * tp_t / max(2 * tp_t + fp_t + fn_t, 1)
    )
  }) |> purrr::list_rbind()

  best_threshold <- tuning |>
    filter(sensitivity >= cfg$validation$target_sensitivity) |>
    arrange(desc(f1)) |>
    slice(1)

  if (nrow(best_threshold) > 0) {
    cli_alert_info("Suggested auto-accept threshold: {best_threshold$threshold}")
    cli_alert_info("  Would give: sensitivity={round(best_threshold$sensitivity*100,1)}%, specificity={round(best_threshold$specificity*100,1)}%")
  }

  write_csv(tuning, here("output", "validation_threshold_tuning.csv"))
}

cli_alert_success("Validation complete")
