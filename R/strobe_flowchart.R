# ============================================================
# STROBE participant-flow diagram: how 1,154 parsed abstracts
# become the 1,051 that form the publication-rate denominator.
#
# Built with the 'flowchart' package. Every count is derived
# from the pipeline outputs, never typed in, so the diagram
# cannot drift from the data the way the prose numbers did.
# ============================================================

suppressPackageStartupMessages({
  library(flowchart)
  library(readr)
  library(here)
})

parsed_tbl   <- read_csv(here("data", "processed", "abstracts_parsed.csv"),
                         show_col_types = FALSE)
cleaned_tbl  <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                         show_col_types = FALSE)
analytic_tbl <- read_csv(here("output", "final_analytical_dataset.csv"),
                         show_col_types = FALSE)

n_parsed    <- nrow(parsed_tbl)
n_cohort    <- nrow(cleaned_tbl)
n_video     <- n_parsed - n_cohort
n_pending   <- sum(is.na(analytic_tbl$final_published))
n_evaluated <- n_cohort - n_pending
n_published <- sum(analytic_tbl$final_published, na.rm = TRUE)
n_not_pub   <- n_evaluated - n_published

# Abstracts whose credited publication predates their congress. PI decision,
# 2026-05-09: such a paper cannot be a conference-to-publication conversion, so
# they are counted UNPUBLISHED. They stay in the denominator, which is why they
# are drawn as a breakdown of "Not published" rather than as an exclusion arrow
# off the spine: an exclusion arrow would say they left the study, and they did
# not. See docs/OUTCOME_DEFINITION.md.
n_pre_congress <- sum(!is.na(analytic_tbl$months_to_pub) &
                        analytic_tbl$months_to_pub < 0, na.rm = TRUE)
n_no_pub_found <- n_not_pub - n_pre_congress

stopifnot(
  nrow(analytic_tbl) == n_cohort,
  n_published + n_not_pub == n_evaluated,
  n_evaluated + n_pending == n_cohort,
  # No abstract may be counted published on a paper that predates its congress.
  sum(analytic_tbl$final_published %in% TRUE &
        analytic_tbl$months_to_pub < 0, na.rm = TRUE) == 0,
  n_no_pub_found + n_pre_congress == n_not_pub
)

message("parsed ", n_parsed, " -> cohort ", n_cohort, " -> evaluated ", n_evaluated)
message("publication rate: ", round(n_published / n_evaluated * 100, 1), "% (",
        n_published, "/", n_evaluated, ")")
message("not published ", n_not_pub, " = ", n_no_pub_found,
        " with no qualifying publication + ", n_pre_congress,
        " whose publication predates the congress")

INK <- "#1B3A4B"

strobe_fc <- as_fc(
  N            = n_parsed,
  label        = "Abstracts parsed from AAGL Global Congress\nsupplements, 2012-2023",
  text_pattern = "{label}\nn = {N}",
  text_fs      = 9,
  bg_fill      = "#EAF1F5",
  border_color = INK
) |>
  fc_filter(
    N                = n_cohort,
    label            = "Oral presentation cohort",
    text_pattern     = "{label}\nn = {n}",
    show_exc         = TRUE,
    direction_exc    = "right",
    label_exc        = "Excluded: video presentations",
    text_pattern_exc = "{label}\nn = {n}",
    text_fs          = 9,
    text_fs_exc      = 8,
    bg_fill          = "#EAF1F5",
    border_color     = INK,
    border_color_exc = "#8A9BA5",
    offset_exc       = 0.18,
    width_exc        = 0.30
  ) |>
  fc_filter(
    N                = n_evaluated,
    label            = "Evaluated for publication status\n(denominator)",
    text_pattern     = "{label}\nn = {n}",
    show_exc         = TRUE,
    direction_exc    = "right",
    label_exc        = paste0("Excluded: adjudication unresolved\n",
                              "(algorithm probable or possible,\nreviewer skipped)"),
    text_pattern_exc = "{label}\nn = {n}",
    text_fs          = 9,
    text_fs_exc      = 8,
    text_fface       = 2,
    bg_fill          = "#D6E5EE",
    border_color     = INK,
    border_color_exc = "#8A9BA5",
    offset_exc       = 0.18,
    width_exc        = 0.30
  ) |>
  fc_split(
    N            = c(n_published, n_not_pub),
    label        = c("Published in a peer-reviewed journal",
                     "Not published"),
    text_pattern = "{label}\nn = {n} ({perc}%)",
    round_digits = 1,
    text_fs      = 9,
    bg_fill      = "white",
    border_color = INK
  )

# Break the unpublished arm down. This is a split, not an exclusion arrow,
# because these abstracts remain in the denominator: they are counted
# unpublished, not removed from the study. An arrow off the spine would say the
# opposite.
#
# fc_split() addresses branches by positional name ("group 1", "group 2")
# because as_fc(N = ) has no real grouping variable to name them after. Looking
# the group up by its label rather than hard-coding a position means a future
# reordering of the split fails loudly instead of silently annotating the
# published arm.
not_pub_group <- strobe_fc$fc$group[
  strobe_fc$fc$type == "split" & strobe_fc$fc$label == "Not published"]
stopifnot(length(not_pub_group) == 1)

strobe_fc <- strobe_fc |>
  fc_split(
    N            = c(n_no_pub_found, n_pre_congress),
    sel_group    = not_pub_group,
    label        = c("No qualifying\npublication identified",
                     "Publication predates\nthe congress"),
    text_pattern = "{label}\nn = {n}",
    text_fs      = 8,
    width        = 0.19,
    bg_fill      = "#F4F1EC",
    border_color = "#8A9BA5"
  )

# fc_export() requires the drawn object, not the spec
strobe_fc <- fc_draw(strobe_fc)

fc_export(strobe_fc, filename = "figure1_strobe_flowchart.png",
          path = here("output", "figures"),
          width = 2400, height = 1750, res = 260, units = "px")
fc_export(strobe_fc, filename = "figure1_strobe_flowchart.pdf",
          path = here("output", "figures"),
          width = 9, height = 6.7, units = "in")

message("wrote output/figures/figure1_strobe_flowchart.{png,pdf}")
