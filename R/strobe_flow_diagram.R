# strobe_flow_diagram.R — STROBE-style flow diagram for abstract inclusion/exclusion
#
# Uses DiagrammeR (Graphviz) for the diagram and DiagrammeRsvg + rsvg to export
# a publication-quality PNG and PDF.

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(cli)
})

cli_h2("STROBE Flow Diagram")

# ── Pull live counts ──────────────────────────────────────────────────────────
parsed   <- read_csv(here("data", "processed", "abstracts_parsed.csv"),
                     show_col_types = FALSE)
cleaned  <- read_csv(here("data", "processed", "abstracts_cleaned.csv"),
                     show_col_types = FALSE)
matches  <- read_csv(here("output", "abstracts_with_matches.csv"),
                     show_col_types = FALSE)
decisions_path <- here("output", "manual_review_decisions.csv")
decisions <- if (file.exists(decisions_path))
  read_csv(decisions_path, show_col_types = FALSE) else tibble()

n_parsed      <- nrow(parsed)
n_video       <- if ("session_type" %in% names(parsed)) {
  sum(parsed$session_type == "Video", na.rm = TRUE)
} else {
  0L
}
n_included    <- nrow(cleaned)        # after video exclusion
n_with_cands  <- sum(matches$n_candidates > 0, na.rm = TRUE)
n_no_cands    <- sum(matches$n_candidates == 0, na.rm = TRUE)

n_definite    <- sum(matches$classification == "definite")
n_probable    <- sum(matches$classification == "probable")
n_possible    <- sum(matches$classification == "possible")
n_no_match    <- sum(matches$classification == "no_match")
n_excluded    <- sum(matches$classification == "excluded")

human_confirmed <- if (nrow(decisions) > 0) {
  decisions |> filter(reviewer != "AUTO", manual_decision == "match") |>
    pull(abstract_id) |> unique() |> length()
} else {
  0L
}

n_confirmed_published   <- n_definite + human_confirmed
n_pending_review        <- n_probable + n_possible
n_confirmed_unpublished <- n_no_match + n_no_cands

cli_alert_info("Parsed: {n_parsed} | Included: {n_included} | Video excluded: {n_video}")
cli_alert_info("Published: {n_confirmed_published} | Pending: {n_pending_review} | Unpublished: {n_confirmed_unpublished}")

# ── Build Graphviz DOT ────────────────────────────────────────────────────────
dot <- sprintf('
digraph strobe {
  graph [
    rankdir = TB
    fontname = "Helvetica"
    fontsize = 11
    splines = ortho
    nodesep = 0.6
    ranksep = 0.7
    bgcolor = white
  ]
  node [
    shape = box
    style = "filled,rounded"
    fillcolor = "#EBF4FA"
    color = "#2C6E9B"
    penwidth = 1.5
    fontname = "Helvetica"
    fontsize = 10
    margin = "0.2,0.12"
    width = 3.2
  ]
  edge [
    color = "#555555"
    arrowsize = 0.8
    penwidth = 1.2
  ]

  /* ── Identification ── */
  A [label = "Abstracts identified from\\nScienceDirect supplement issues\\n(AAGL 2012–2023)\\nn = %d", fillcolor = "#D6EAF8"]

  /* ── Exclusion box (side) */
  excl_video [
    label   = "Excluded: Video presentations\\nn = %d"
    shape   = box
    style   = "filled"
    fillcolor = "#FADBD8"
    color   = "#B03A2E"
    width   = 2.6
  ]

  /* ── Screening ── */
  B [label = "Oral-presentation abstracts\\nincluded in analysis\\nn = %d"]

  /* ── Candidate search ── */
  C [label = "Abstracts with ≥1 candidate\\npublication identified\\nn = %d"]

  excl_nocand [
    label   = "No candidate publications\\nfound in any database\\nn = %d"
    shape   = box
    style   = "filled"
    fillcolor = "#FADBD8"
    color   = "#B03A2E"
    width   = 2.6
  ]

  /* ── Algorithm scoring ── */
  D [label = "Candidates scored using\\nCochrane-aligned algorithm\\n(title + author + date + journal)"]

  /* ── Classification outcomes ── */
  E_def [
    label     = "Definite match\\n(score ≥ threshold + text evidence)\\nn = %d"
    fillcolor = "#D5F5E3"
    color     = "#1E8449"
  ]
  E_prob [
    label     = "Probable match\\n(awaiting human review)\\nn = %d"
    fillcolor = "#FEF9E7"
    color     = "#B7950B"
  ]
  E_poss [
    label     = "Possible match\\n(awaiting human review)\\nn = %d"
    fillcolor = "#FEF9E7"
    color     = "#B7950B"
  ]
  E_excl [
    label     = "Excluded\\n(candidate published before conference)\\nn = %d"
    fillcolor = "#FADBD8"
    color     = "#B03A2E"
  ]
  E_none [
    label     = "No match identified\\nn = %d"
    fillcolor = "#FADBD8"
    color     = "#B03A2E"
  ]

  /* ── Final outcomes ── */
  F_pub [
    label     = "Confirmed published\\n(definite + reviewer-confirmed)\\nn = %d"
    fillcolor = "#D5F5E3"
    color     = "#1E8449"
    penwidth  = 2
  ]
  F_pend [
    label     = "Pending human adjudication\\n(probable + possible)\\nn = %d"
    fillcolor = "#FEF9E7"
    color     = "#B7950B"
    penwidth  = 2
  ]
  F_unpub [
    label     = "Not published\\n(no match + no candidates)\\nn = %d"
    fillcolor = "#FADBD8"
    color     = "#B03A2E"
    penwidth  = 2
  ]

  /* ── Edges ── */
  A -> B
  A -> excl_video [style = dashed]

  B -> C
  B -> excl_nocand [style = dashed]

  C -> D
  D -> E_def
  D -> E_prob
  D -> E_poss
  D -> E_excl [style = dashed]
  D -> E_none [style = dashed]

  E_def  -> F_pub
  E_prob -> F_pend
  E_poss -> F_pend
  E_excl -> F_unpub [style = dashed]
  E_none -> F_unpub [style = dashed]

  /* ── Layout ranks ── */
  { rank = same; A; excl_video }
  { rank = same; B; excl_nocand }
  { rank = same; E_def; E_prob; E_poss; E_excl; E_none }
  { rank = same; F_pub; F_pend; F_unpub }
}
',
  n_parsed, n_video, n_included,
  n_with_cands, n_no_cands,
  n_definite, n_probable, n_possible, n_excluded, n_no_match,
  n_confirmed_published, n_pending_review, n_confirmed_unpublished
)

# ── Render ────────────────────────────────────────────────────────────────────
dir.create(here("output", "figures"), showWarnings = FALSE, recursive = TRUE)

if (requireNamespace("DiagrammeR", quietly = TRUE) &&
    requireNamespace("DiagrammeRsvg", quietly = TRUE) &&
    requireNamespace("rsvg", quietly = TRUE)) {

  g   <- DiagrammeR::grViz(dot)
  svg <- DiagrammeRsvg::export_svg(g)
  writeLines(svg, here("output", "figures", "strobe_flow.svg"))

  rsvg::rsvg_png(chartr("\n", " ", here("output", "figures", "strobe_flow.svg")),
                 here("output", "figures", "strobe_flow.png"),
                 width = 2400, height = 3000)
  rsvg::rsvg_pdf(chartr("\n", " ", here("output", "figures", "strobe_flow.svg")),
                 here("output", "figures", "strobe_flow.pdf"))

  cli_alert_success("Saved: output/figures/strobe_flow.png (.svg, .pdf)")

} else {
  # Fallback: write DOT file for manual rendering
  writeLines(dot, here("output", "figures", "strobe_flow.dot"))
  cli_alert_warning("DiagrammeR/rsvg not found — wrote strobe_flow.dot (render at https://dreampuf.github.io/GraphvizOnline)")
}
