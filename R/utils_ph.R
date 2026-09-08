# utils_ph.R — proportional-hazards remediation for an arbitrary set of violators.
#
# The previous implementation carried a singleton assumption in the place that
# mattered least visibly. It built two sensitivity fits -- strata on every
# violator, and tt() on the numeric ones -- then reported the global p of the
# FIRST while labelling the remediation with a description of the SECOND. The
# committed artifact therefore said
#     remediation = "time_varying:n_authors;strata:is_us_based"
#     remediated_global_p = 0.607
# where 0.607 came from a model that stratified on both, including on a numeric
# covariate. The label and the number described different models, and nothing
# checked that they agreed.
#
# A constraint drives the design and is worth stating plainly: survival::cox.zph
# REFUSES to run on a model containing tt() terms ("function not defined for
# models with tt() terms"). So a tt()-remediated model cannot yield a
# post-remediation global PH p at all. Any implementation that reports one for a
# tt() model is reporting a number from a different fit. The production model is
# therefore the one whose PH is re-testable, and tt() fits are retained as
# sensitivity analyses carrying their own evidence -- the tt term's p-value,
# which is a direct test of non-proportionality -- plus AIC against the original.

PH_REMEDIATION_RULES <- list(
  numeric = list(
    production = "strata_binned",
    sensitivity = "time_varying",
    rationale = paste(
      "A numeric violator is stratified on its distinct values for the",
      "production fit so the PH assumption can be re-tested, and additionally",
      "fitted with a log-time interaction as a sensitivity analysis, which",
      "keeps the effect estimable but makes cox.zph undefined.")),
  logical = list(
    production = "strata", sensitivity = NA_character_,
    rationale = "A binary violator moves into strata(), absorbing its baseline hazard."),
  factor = list(
    production = "strata", sensitivity = NA_character_,
    rationale = "A categorical violator moves into strata(), absorbing its baseline hazard."),
  character = list(
    production = "strata", sensitivity = NA_character_,
    rationale = "A categorical violator moves into strata(), absorbing its baseline hazard.")
)

#' Classify a covariate for remediation purposes.
#' @param x A column.
#' @return One of the names of `PH_REMEDIATION_RULES`, or `"unsupported"`.
#' @export
ph_variable_type <- function(x) {
  if (is.logical(x)) return("logical")
  if (is.factor(x)) return("factor")
  if (is.character(x)) return("character")
  if (is.numeric(x)) return("numeric")
  "unsupported"
}

#' Identify every PH-violating covariate.
#'
#' @param zph A `cox.zph` object, or `NULL`.
#' @param candidates Character vector of model terms eligible to be violators.
#' @param alpha Numeric. Schoenfeld test level.
#' @return `list(global_p, violators, table)`. `violators` is `character(0)` when
#'   nothing violates, and the caller must distinguish that from `zph` missing,
#'   which returns `global_p = NA`.
#' @export
ph_identify_violators <- function(zph, candidates, alpha = 0.05) {
  if (is.null(zph)) {
    return(list(global_p = NA_real_, violators = character(0),
                table = data.frame(term = character(0), chisq = numeric(0),
                                   df = numeric(0), p = numeric(0),
                                   stringsAsFactors = FALSE)))
  }
  tab <- as.data.frame(zph$table)
  tab$term <- rownames(tab)
  gp <- suppressWarnings(as.numeric(tab$p[tab$term == "GLOBAL"]))
  if (!length(gp)) gp <- NA_real_
  v <- tab$term[tab$term != "GLOBAL" & !is.na(tab$p) & tab$p < alpha]
  list(global_p = gp,
       violators = intersect(v, candidates),
       table = tab[, c("term", "chisq", "df", "p")])
}

#' Plan remediation for every violator, or refuse.
#'
#' Fails closed: a violator whose type has no registered rule stops the run
#' rather than being dropped, because a silently unremediated violator is
#' indistinguishable in the output from one that never violated.
#'
#' @param violators Character vector.
#' @param data The model frame.
#' @param rules Named list of remediation rules.
#' @return A data frame with `variable`, `variable_type`, `remediation`,
#'   `sensitivity`, `rationale`.
#' @export
ph_plan_remediation <- function(violators, data, rules = PH_REMEDIATION_RULES) {
  if (!length(violators))
    return(data.frame(variable = character(0), variable_type = character(0),
                      remediation = character(0), sensitivity = character(0),
                      rationale = character(0), stringsAsFactors = FALSE))

  types <- vapply(violators, function(v) {
    if (!v %in% names(data)) return("unsupported")
    ph_variable_type(data[[v]])
  }, character(1))

  unknown <- violators[!types %in% names(rules)]
  if (length(unknown))
    stop("No registered PH remediation rule for: ",
         paste(sprintf("%s (%s)", unknown, types[violators %in% unknown]),
               collapse = ", "),
         ". Register a rule in PH_REMEDIATION_RULES or remove the term; a ",
         "violator with no rule must not pass silently.", call. = FALSE)

  data.frame(
    variable = unname(violators),
    variable_type = unname(types),
    remediation = vapply(types, function(t) rules[[t]]$production, character(1),
                         USE.NAMES = FALSE),
    sensitivity = vapply(types, function(t) rules[[t]]$sensitivity %||% NA_character_,
                         character(1), USE.NAMES = FALSE),
    rationale = vapply(types, function(t) rules[[t]]$rationale, character(1),
                       USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
}

#' Build the production remediated formula.
#'
#' Every violator is remediated according to its plan; non-violating terms are
#' carried through unchanged. Works for zero, one or many violators.
#'
#' @param all_terms Character vector of model terms.
#' @param plan A data frame from [ph_plan_remediation()].
#' @param surv_lhs Left-hand side of the formula.
#' @return A formula.
#' @export
ph_production_formula <- function(all_terms, plan, surv_lhs = "Surv(time, event)") {
  strat <- plan$variable[plan$remediation %in% c("strata", "strata_binned")]
  keep  <- setdiff(all_terms, strat)
  # A right-hand side of nothing but strata() terms is degenerate: strata
  # absorbs baseline hazard and estimates no coefficient, so such a fit has no
  # covariates at all. That is a different failure from an empty formula and
  # would otherwise produce a model with nothing to report.
  if (!length(keep))
    stop("Remediation removed every covariate from the model: ",
         "all of (", paste(all_terms, collapse = ", "), ") would be stratified. ",
         "A fit of only strata() terms estimates nothing.", call. = FALSE)
  rhs <- c(keep, sprintf("strata(%s)", strat))
  stats::as.formula(paste(surv_lhs, "~", paste(rhs, collapse = " + ")))
}

#' Build the time-varying sensitivity formula for numeric violators.
#' @inheritParams ph_production_formula
#' @return A formula, or `NULL` when no violator takes a time-varying rule.
#' @export
ph_timevarying_formula <- function(all_terms, plan, surv_lhs = "Surv(time, event)") {
  tv <- plan$variable[!is.na(plan$sensitivity) & plan$sensitivity == "time_varying"]
  if (!length(tv)) return(NULL)
  stats::as.formula(paste(surv_lhs, "~", paste(all_terms, collapse = " + "), "+",
                          paste(sprintf("tt(%s)", tv), collapse = " + ")))
}

#' Post-remediation PH test, where one is defined.
#'
#' `cox.zph` is undefined for models containing `tt()` terms. Returning `NA`
#' without saying so is how a missing diagnostic becomes indistinguishable from
#' a passing one, so the reason is returned alongside.
#'
#' @param fit A `coxph` fit, or `NULL`.
#' @return `list(global_p, status)`.
#' @export
ph_retest <- function(fit) {
  if (is.null(fit)) return(list(global_p = NA_real_, status = "model_not_fitted"))
  has_tt <- any(grepl("tt\\(", attr(stats::terms(fit), "term.labels")))
  if (has_tt)
    return(list(global_p = NA_real_,
                status = "undefined_for_tt_models"))
  z <- tryCatch(survival::cox.zph(fit), error = function(e) NULL)
  if (is.null(z)) return(list(global_p = NA_real_, status = "cox_zph_failed"))
  list(global_p = unname(z$table["GLOBAL", "p"]), status = "ok")
}

#' Model comparison that does not disappear when there are many violators.
#' @param fits Named list of `coxph` fits (or `NULL` entries).
#' @return A data frame with `model`, `aic`, `n_terms`, `delta_aic_vs_original`.
#' @export
ph_model_comparison <- function(fits) {
  keep <- fits[!vapply(fits, is.null, logical(1))]
  if (!length(keep))
    return(data.frame(model = character(0), aic = numeric(0),
                      n_terms = integer(0), delta_aic_vs_original = numeric(0),
                      stringsAsFactors = FALSE))
  aic <- vapply(keep, function(f) tryCatch(as.numeric(stats::AIC(f)),
                                           error = function(e) NA_real_), numeric(1))
  nt <- vapply(keep, function(f) length(attr(stats::terms(f), "term.labels")),
               integer(1))
  base <- if ("original" %in% names(keep)) aic[["original"]] else NA_real_
  data.frame(model = names(keep), aic = round(unname(aic), 2),
             n_terms = unname(nt),
             delta_aic_vs_original = round(unname(aic) - base, 2),
             stringsAsFactors = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
