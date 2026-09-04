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

stopifnot(
  nrow(analytic_tbl) == n_cohort,
  n_published + n_not_pub == n_evaluated,
  n_evaluated + n_pending == n_cohort
)

message("parsed ", n_parsed, " -> cohort ", n_cohort, " -> evaluated ", n_evaluated)
message("publication rate: ", round(n_published / n_evaluated * 100, 1), "% (",
        n_published, "/", n_evaluated, ")")

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

# fc_export() requires the drawn object, not the spec
strobe_fc <- fc_draw(strobe_fc)

fc_export(strobe_fc, filename = "figure1_strobe_flowchart.png",
          path = here("output", "figures"),
          width = 2400, height = 1500, res = 260, units = "px")
fc_export(strobe_fc, filename = "figure1_strobe_flowchart.pdf",
          path = here("output", "figures"),
          width = 9, height = 5.8, units = "in")

message("wrote output/figures/figure1_strobe_flowchart.{png,pdf}")
