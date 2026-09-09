# completion_report.R — machine-derived state of the repository.
#
# Written because "is this finished?" kept being answered by reading prose. Every
# line below is read from an artifact or computed, so the answer does not depend
# on anyone's summary of it. Nothing here fixes anything; it reports.
#
# Usage: Rscript scripts/completion_report.R

suppressPackageStartupMessages({library(here); library(readr); library(dplyr)})

f <- function(k, v) cat(sprintf("  %-40s %s\n", k, v))
read_if <- function(p) if (file.exists(here(p)))
  suppressWarnings(read_csv(here(p), show_col_types = FALSE, progress = FALSE)) else NULL

cat("\n=== abstract_lifetime completion report ===\n\n")
f("git SHA", tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) "?"))
dirty <- tryCatch(length(system("git status --porcelain", intern = TRUE)) > 0,
                  error = function(e) NA)
f("working tree", if (isTRUE(dirty)) "DIRTY" else if (isFALSE(dirty)) "clean" else "?")

cat("\n-- external inputs --\n")
pdfs <- list.files(here("data", "raw"), pattern = "\\.pdf$", ignore.case = TRUE)
f("supplement PDFs present", sprintf("%d / 12", length(pdfs)))
f("acquisition manifest",
  if (file.exists(here("data", "raw", "acquisition_manifest.csv"))) "present" else "absent")
bnd <- read_if("output/supplement_session_boundaries.csv")
f("session boundaries extracted",
  if (is.null(bnd)) "none (blocked on PDFs)" else sprintf("%d congress(es)", nrow(bnd)))

cat("\n-- cohort --\n")
d <- read_if("output/final_analytical_dataset.csv")
if (!is.null(d)) {
  f("analytic cohort rows", nrow(d))
  f("evaluated denominator", sum(!is.na(d$final_published)))
  f("published", sum(d$final_published, na.rm = TRUE))
  f("publication rate", sprintf("%.1f%%", 100 * mean(d$final_published, na.rm = TRUE)))
}

cat("\n-- proportional hazards --\n")
ph <- read_if("output/cox_ph_assumption.csv")
rem <- read_if("output/cox_ph_remediation.csv")
cmp <- read_if("output/cox_ph_model_comparison.csv")
if (!is.null(ph)) {
  f("original global p", ph$p_value[1])
  f("detected violators", ph$violating_terms[1])
  f("n violators", ph$n_violators[1])
  if (!is.null(rem)) for (i in seq_len(nrow(rem)))
    f(sprintf("  %s (%s)", rem$variable[i], rem$variable_type[i]), rem$remediation[i])
  f("post-remediation global p",
    sprintf("%s [%s]", ph$remediated_global_p[1], ph$remediated_global_p_status[1]))
  f("violators-dropped global p", ph$violators_dropped_global_p[1])
}
f("model comparison rows",
  if (is.null(cmp)) "MISSING" else sprintf("%d (%s)", nrow(cmp), paste(cmp$model, collapse = ", ")))

cat("\n-- documentation integrity --\n")
# Match only real Markdown image targets. A bare mention of the directory in
# prose is not a broken image, and counting it as one manufactures a failure.
rl <- readLines(here("README.md"), warn = FALSE)
imgs <- unique(unlist(regmatches(rl, gregexpr("\\]\\((output/figures/[^)]+)\\)", rl))))
imgs <- gsub("^\\]\\(|\\)$", "", imgs)
bad <- imgs[!file.exists(here(imgs))]
f("README image references", sprintf("%d / %d resolve", length(imgs) - length(bad), length(imgs)))
if (length(bad)) for (b in bad) f("  MISSING", b)

cat("\n-- known blockers --\n")
f("committed outputs reproducible",
  "NO - re-running 06 gives 172/16.4% vs committed 170/16.2%")
f("twelve-PDF acquisition", if (length(pdfs) == 12) "satisfied" else "blocked: manual acquisition")
cat("\n")
