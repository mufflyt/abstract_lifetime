# Generate docs/DECISIONS_PENDING.md from tests/expected_failures.yaml.
#
# Sixteen tests in this repository fail on purpose. Each marks a question that
# belongs to the author rather than a defect in the code, and every one already
# carries a reason and the decision it waits on. The problem was distribution:
# that information existed only inside a YAML file read by CI, so the person who
# actually has to make the calls had no single page to work from.
#
# The document is GENERATED rather than written, because a hand-written copy
# would drift the moment an entry was added or resolved, and a stale list of
# open decisions is worse than none: it invites someone to answer a question
# that has already been settled.
# tests/testthat/test-decisions_pending_current.R fails if the committed file
# and the manifest disagree.

suppressPackageStartupMessages({
  library(yaml)
  library(here)
})

# Folded YAML scalars arrive with hard newlines from the source wrapping. Those
# are an artifact of how the file is typed, not of the sentence, so reflow to
# single-spaced prose and let Markdown wrap it.
unwrap <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  trimws(gsub("\\s+", " ", x))
}

build_decisions_markdown <- function(manifest_path = here::here("tests", "expected_failures.yaml")) {
  m <- yaml::yaml.load_file(manifest_path)
  entries <- m$expected_failures

  # Group by the file the test lives in. The cycles were thematic, so the file
  # is a better organising unit than the order of registration.
  by_file <- split(entries, vapply(entries, function(e) e$file, character(1)))

  out <- c(
    "<!-- GENERATED FILE. Do not edit by hand.",
    "     Source: tests/expected_failures.yaml",
    "     Regenerate: Rscript R/generate_decisions_pending.R -->",
    "",
    "# Decisions pending",
    "",
    sprintf(paste("This repository keeps %d tests failing on purpose. Each one below is a",
                  "question that code cannot answer: resolving it changes the estimand,",
                  "the cohort, or an adjudication that a human already recorded. None is",
                  "a defect awaiting a fix."), length(entries)),
    "",
    paste("CI is green while exactly these fail. If one of them starts passing, CI goes",
          "red until its entry is removed, so this list cannot quietly outlive its",
          "reasons."),
    "",
    "## How to use this",
    "",
    paste("Work an item by deciding the question in **Decision needed**, then either",
          "make the change and delete the entry from `tests/expected_failures.yaml`, or",
          "record why the current behaviour stands and leave it. Regenerate this file",
          "afterwards."),
    ""
  )

  n <- 0L
  for (f in sort(names(by_file))) {
    out <- c(out, sprintf("## %s", f), "")
    for (e in by_file[[f]]) {
      n <- n + 1L
      out <- c(out,
        sprintf("### %d. %s", n, unwrap(e$test)),
        "",
        sprintf("**What fails.** %s", unwrap(e$reason)),
        "",
        sprintf("**Decision needed.** %s", unwrap(e$decision_needed)),
        ""
      )
      if (!is.null(e$documented_in)) {
        out <- c(out, sprintf("**Documented in.** %s", unwrap(e$documented_in)), "")
      }
    }
  }
  paste0(paste(out, collapse = "\n"), "\n")
}

if (identical(environment(), globalenv()) && !interactive()) {
  p <- here::here("docs", "DECISIONS_PENDING.md")
  # cat(), not writeLines(): the string already ends in a newline, and
  # writeLines would append a second one, so the file would never match what
  # the currency test regenerates.
  cat(build_decisions_markdown(), file = p)
  cat("wrote", p, "\n")
}
