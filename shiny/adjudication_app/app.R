# Shiny Adjudication App — Manual review of abstract-to-publication matches
# Left panel: score breakdown chart; Right panel: candidate papers with scores
# Backend: Google Sheets (service account) with local CSV fallback

library(shiny)
library(bslib)
library(DT)
library(readr)
library(dplyr)
library(stringr)
library(here)
library(shinyjs)
library(googlesheets4)

# --- Path resolution (dev vs shinyapps.io deploy) ---
# In dev, data/output live at the repo root. In a deployed bundle, only the
# app directory is uploaded, so we copy a subset of files into ./bundle/ at
# deploy time. app_path() looks in the app directory first, then falls back
# to the repo-relative location that here::here() produces.
APP_DIR <- tryCatch(normalizePath(".", mustWork = FALSE), error = function(e) ".")

app_path <- function(...) {
  local <- file.path(APP_DIR, "bundle", ...)
  if (file.exists(local)) return(local)
  here(...)
}

# --- Google Sheets helpers ---

#' Get the Google Sheet ID from env or config
get_sheet_id <- function() {

  id <- Sys.getenv("GOOGLE_SHEETS_ID", "")
  if (nchar(id) == 0) {
    cfg_path <- app_path("config.yml")
    if (file.exists(cfg_path)) {
      cfg <- yaml::read_yaml(cfg_path)
      id <- cfg$default$google_sheet_id %||% ""
    }
  }
  if (nchar(id) == 0) NULL else id
}

#' Attempt Google Sheets authentication via service account
gs_auth_init <- function() {
  json_paths <- c(
    file.path(APP_DIR, "google_credentials.json"),
    "google_credentials.json",
    here("shiny", "adjudication_app", "google_credentials.json")
  )
  for (p in json_paths) {
    if (file.exists(p)) {
      tryCatch({
        gs4_auth(path = p)
        return(TRUE)
      }, error = function(e) NULL)
    }
  }
  gs4_deauth()
  FALSE
}

#' Check if Google Sheets backend is available
gs_available <- function() {
  id <- get_sheet_id()
  if (is.null(id)) return(FALSE)
  auth_ok <- gs_auth_init()
  if (!auth_ok) return(FALSE)
  tryCatch({
    sheet_names(id)
    TRUE
  }, error = function(e) FALSE)
}

# Canonical decision-row schema. Order preserved — new columns appended to the
# right so previously written sheets stay valid (existing rows get NA for new
# columns).
DECISION_COLS <- c(
  "abstract_id", "reviewer", "manual_decision", "manual_pmid",
  "reviewer_notes", "review_timestamp",
  "abstract_title", "abstract_first_author", "abstract_subtype", "session_type",
  "congress_year", "sciencedirect_url",
  "matched_pub_title", "matched_pub_journal", "matched_pub_year",
  "matched_score", "matched_title_similarity",
  "matched_pub_type", "matched_pub_types_raw",
  "n_authors", "n_unique_affiliations",
  "first_author_state", "first_author_acog_district",
  "first_author_gender"
)

empty_decisions <- function() {
  out <- as_tibble(setNames(
    replicate(length(DECISION_COLS), character(), simplify = FALSE),
    DECISION_COLS
  ))
  out
}

#' Read decisions from Google Sheets
gs_read_decisions <- function(sheet_id) {
  tryCatch({
    col_types <- strrep("c", length(DECISION_COLS))
    d <- read_sheet(sheet_id, sheet = "decisions", col_types = col_types)
    if (nrow(d) == 0) return(empty_decisions())
    for (col in DECISION_COLS) {
      if (!col %in% names(d)) d[[col]] <- NA_character_
    }
    d |> mutate(across(everything(), as.character),
                reviewer = if_else(is.na(reviewer) | reviewer == "NA", "AUTO", reviewer))
  }, error = function(e) {
    NULL
  })
}

#' Ensure the sheet's header row contains all DECISION_COLS. Existing columns
#' keep their position; missing columns are appended to the right.
gs_ensure_headers <- function(sheet_id) {
  tryCatch({
    current <- tryCatch(
      names(read_sheet(sheet_id, sheet = "decisions", n_max = 0)),
      error = function(e) character()
    )
    missing <- setdiff(DECISION_COLS, current)
    if (length(missing) == 0) return(TRUE)
    new_header <- c(current, missing)
    header_tbl <- as_tibble(setNames(
      replicate(length(new_header), character(), simplify = FALSE),
      new_header
    ))
    range_write(sheet_id, header_tbl, sheet = "decisions",
                range = "A1", col_names = TRUE, reformat = FALSE)
    TRUE
  }, error = function(e) {
    warning("Failed to extend headers: ", conditionMessage(e))
    FALSE
  })
}

#' Append a single decision row to Google Sheet. Reorders columns to match the
#' sheet's header so values land in the correct cells.
gs_append_decision <- function(sheet_id, new_row) {
  tryCatch({
    header <- names(read_sheet(sheet_id, sheet = "decisions", n_max = 0))
    # Add cols in header missing from new_row
    for (col in setdiff(header, names(new_row))) new_row[[col]] <- NA_character_
    # Drop cols in new_row not in header (avoids "undefined columns" error)
    new_row <- new_row[, intersect(names(new_row), header), drop = FALSE]
    # Reorder to match header
    new_row <- new_row[, header, drop = FALSE]
    sheet_append(sheet_id, new_row, sheet = "decisions")
    TRUE
  }, error = function(e) {
    warning("Google Sheets append failed: ", conditionMessage(e))
    FALSE
  })
}

#' Read all decisions from Google Sheet then download as tibble
gs_all_decisions <- function(sheet_id) {
  gs_read_decisions(sheet_id)
}

# --- Data loading ---
load_data <- function(use_gs = FALSE, sheet_id = NULL) {
  # Primary queue = full 98 abstracts with algorithm classification. This
  # lets reviewers spot-check auto-accepts and auto-rejects, not just the
  # algorithm's "review" cases.
  review_path     <- app_path("output", "abstracts_with_matches.csv")
  candidates_path <- app_path("data", "processed", "pubmed_candidates.csv")
  scores_path     <- app_path("data", "processed", "match_scores_detailed.rds")
  decisions_path  <- app_path("output", "manual_review_decisions.csv")
  abstracts_path  <- app_path("data", "processed", "abstracts_cleaned.csv")

  review_queue  <- if (file.exists(review_path)) read_csv(review_path, show_col_types = FALSE) else tibble()
  candidates    <- if (file.exists(candidates_path)) read_csv(candidates_path, show_col_types = FALSE) else tibble()
  scores_detail <- if (file.exists(scores_path)) readRDS(scores_path) else tibble()
  abstracts     <- if (file.exists(abstracts_path)) {
    read_csv(abstracts_path, show_col_types = FALSE) |>
      select(any_of(c("abstract_id", "abstract_text", "authors_raw",
                       "author_name_first", "author_name_last",
                       "title", "subtype", "session_type",
                       "congress_year", "article_url", "doi",
                       "first_author_normalized")))
  } else tibble(abstract_id = character(), abstract_text = character(),
                 authors_raw = character())

  # Load decisions: Google Sheets first, then CSV fallback
  decisions <- NULL
  if (use_gs && !is.null(sheet_id)) {
    decisions <- gs_read_decisions(sheet_id)
  }
  if (is.null(decisions)) {
    decisions <- if (file.exists(decisions_path)) {
      d <- read_csv(decisions_path, show_col_types = FALSE)
      for (col in DECISION_COLS) {
        if (!col %in% names(d)) d[[col]] <- NA_character_
      }
      d |> mutate(across(everything(), as.character),
                  reviewer = if_else(is.na(reviewer) | reviewer == "NA", "AUTO", reviewer))
    } else {
      empty_decisions()
    }
  }

  list(
    review_queue  = review_queue,
    candidates    = candidates,
    scores_detail = scores_detail,
    decisions     = decisions,
    abstracts     = abstracts
  )
}

#' Deduplicate decisions: keep latest per (abstract_id, reviewer) pair
dedup_decisions <- function(decisions) {
  if (nrow(decisions) == 0) return(decisions)
  decisions |>
    group_by(abstract_id, reviewer) |>
    arrange(desc(review_timestamp)) |>
    slice(1) |>
    ungroup()
}

# --- JavaScript for keyboard shortcuts ---
keyboard_js <- "
$(document).on('keydown', function(e) {
  // Skip if user is typing in an input field
  var tag = document.activeElement.tagName.toLowerCase();
  var type = (document.activeElement.type || '').toLowerCase();
  if (tag === 'input' || tag === 'textarea' || tag === 'select') return;

  var key = e.key;
  var handled = false;

  if (key === 'm' || key === 'M') {
    Shiny.setInputValue('kb_decision', 'match', {priority: 'event'});
    handled = true;
  } else if (key === 'n' || key === 'N') {
    Shiny.setInputValue('kb_decision', 'no_match', {priority: 'event'});
    handled = true;
  } else if (key === 's' || key === 'S') {
    Shiny.setInputValue('kb_decision', 'skip', {priority: 'event'});
    handled = true;
  } else if (key === 'Enter' || (e.ctrlKey && key === 's')) {
    Shiny.setInputValue('kb_save', Math.random(), {priority: 'event'});
    handled = true;
  } else if (key === 'ArrowRight' || key === ']') {
    Shiny.setInputValue('kb_next', Math.random(), {priority: 'event'});
    handled = true;
  } else if (key === 'ArrowLeft' || key === '[') {
    Shiny.setInputValue('kb_prev', Math.random(), {priority: 'event'});
    handled = true;
  }

  if (handled) e.preventDefault();
});

// Bootstrap 5 tooltip init — re-run after each Shiny output update
$(document).on('shiny:value', function() {
  setTimeout(function() {
    document.querySelectorAll('[data-bs-toggle=\"tooltip\"]').forEach(function(el) {
      if (!bootstrap.Tooltip.getInstance(el)) {
        new bootstrap.Tooltip(el, {trigger: 'hover'});
      }
    });
  }, 50);
});

// localStorage persistence for reviewer initials
// Use shiny:connected (not a fixed timeout) so initials fire only after
// the Shiny session is fully ready to accept setInputValue calls.
$(document).on('shiny:connected', function() {
  var saved = localStorage.getItem('adjudication_reviewer_initials');
  if (saved && saved.length >= 2) {
    Shiny.setInputValue('ls_reviewer_initials', saved, {priority: 'event'});
  }
});
$(document).on('change blur', '#reviewer_initials', function() {
  var val = $(this).val().trim().toUpperCase();
  if (val.length >= 2) localStorage.setItem('adjudication_reviewer_initials', val);
});
"

# --- UI helpers ---
# Small "?" icon that shows a tooltip on hover. Place next to a label or
# control when the meaning might not be obvious.
help_icon <- function(text) {
  bslib::tooltip(
    tags$span(
      icon("circle-question"),
      style = "color: #6c757d; cursor: help; margin-left: 4px; font-size: 0.85em;"
    ),
    text,
    placement = "top"
  )
}

# --- UI ---
ui <- page_sidebar(
  title = "Abstract-to-Publication Adjudication",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # shinyjs + keyboard handler
  useShinyjs(),
  tags$head(
    tags$script(HTML(keyboard_js)),
    tags$style(HTML("
      .decision-panel {
        margin: 1rem -1rem -1rem;
        padding: 0.75rem 1rem 1rem;
        background: var(--bs-body-bg);
        border-top: 1px solid var(--bs-border-color);
      }
      .candidate-card {
        min-height: 720px;
      }
      .candidate-card > .card-body {
        display: flex;
        flex-direction: column;
      }
      .candidate-workspace {
        flex: 0 0 440px;
        min-height: 440px;
        overflow-y: auto;
        padding-right: 0.25rem;
      }
      .candidate-table-wrap {
        min-height: 260px;
        overflow-x: auto;
        overflow-y: auto;
      }
      .candidate-table-wrap table.dataTable td {
        white-space: normal;
      }
      .candidate-table-wrap table.dataTable td.dt-top {
        vertical-align: top;
      }
      .score-chip {
        display: inline-block;
        padding: 2px 7px;
        border-radius: 4px;
        font-size: 0.75em;
        font-weight: 600;
        margin: 0 2px 2px 0;
        background: #e9ecef;
        color: #495057;
      }
      .score-chip.positive { background: #d1e7dd; color: #0a3622; }
      .score-chip.negative { background: #f8d7da; color: #58151c; }
      .cand-summary-bar {
        background: #f0f7ff;
        border: 1px solid #b8daff;
        border-radius: 6px;
        padding: 6px 10px;
        font-size: 0.82em;
        margin-bottom: 6px;
        line-height: 1.6;
      }
      .comparison-grid {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        gap: 0.75rem;
      }
      .comparison-pane {
        max-height: 240px;
        overflow-y: auto;
        padding: 0.75rem;
        border: 1px solid var(--bs-border-color);
        border-radius: 6px;
        font-size: 0.85em;
        line-height: 1.5;
      }
      .comparison-pane.abstract-source {
        background: #f8f9fa;
      }
      .comparison-pane.candidate-source {
        background: #f0f7ff;
      }
      @media (max-width: 900px) {
        .comparison-grid {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),

  sidebar = sidebar(
    width = 280,
    tooltip(
      selectInput("abstract_select", "Select Abstract:", choices = NULL, selected = NULL),
      "Pick any abstract. The left card shows the abstract; the right card shows PubMed candidates scored against it.",
      placement = "right"
    ),
    tooltip(
      checkboxInput("filter_unreviewed", "Show unreviewed only", value = FALSE),
      "Hide abstracts that already have a decision saved (by any reviewer). Useful for dividing work.",
      placement = "right"
    ),
    tooltip(
      radioButtons(
        "filter_year",
        tagList("Congress year:",
                help_icon("Narrow the list to a specific AAGL Global Congress year. 'All' shows every congress.")),
        choices = c("All" = "all",
                    "2012" = "2012", "2013" = "2013", "2014" = "2014",
                    "2015" = "2015", "2016" = "2016", "2017" = "2017", "2018" = "2018",
                    "2019" = "2019", "2020" = "2020", "2021" = "2021",
                    "2022" = "2022", "2023" = "2023"),
        selected = "all",
        inline = TRUE
      ),
      "Filter the selector by AAGL year.",
      placement = "right"
    ),
    tooltip(
      radioButtons(
        "filter_class",
        tagList("Algorithm classification:",
                help_icon(HTML(paste(
                  "Filters by the pipeline's Cochrane-aligned match classification:<br>",
                  "• <b>All</b> — every abstract.<br>",
                  "• <b>Definite</b> — high-confidence auto-accepted matches (score &ge;7 + text evidence).<br>",
                  "• <b>Probable</b> — likely matches needing human confirmation (score 3-7 + text evidence).<br>",
                  "• <b>Possible</b> — weak-evidence matches (ties, no text overlap).<br>",
                  "• <b>No match</b> — low-scoring, no viable candidates.<br>",
                  "• <b>Excluded</b> — candidate published before the conference (pre-conference penalty).<br>",
                  "• <b>No candidates</b> — no search results found."
                )))),
        choices = c("All" = "all", "Definite" = "definite",
                    "Probable" = "probable", "Possible" = "possible",
                    "No match" = "no_match", "Excluded" = "excluded",
                    "No candidates" = "no_candidates"),
        selected = "all",
        inline = FALSE
      ),
      "Narrow the abstract list to a specific pipeline decision category.",
      placement = "right"
    ),
    div(
      class = "d-flex gap-2 mb-3",
      actionButton("btn_prev", "Prev", icon = icon("arrow-left"), class = "btn-sm btn-outline-secondary flex-fill"),
      actionButton("btn_next", "Next", icon = icon("arrow-right"), class = "btn-sm btn-outline-secondary flex-fill")
    ),
    hr(),
    value_box(
      title = "Review Progress",
      value = textOutput("progress_count"),
      showcase = icon("clipboard-check"),
      theme = "primary",
      height = "120px"
    ),
    uiOutput("conflict_summary"),
    hr(),
    tooltip(
      downloadButton("download_decisions", "Export Decisions", class = "btn-success btn-sm w-100 mb-2"),
      "Download all decisions (yours + everyone else's) as a CSV snapshot.",
      placement = "right"
    ),
    tooltip(
      actionButton("btn_refresh", "Refresh Data", icon = icon("rotate"), class = "btn-info btn-sm w-100"),
      "Re-read decisions from Google Sheets to pull in other reviewers' recent saves.",
      placement = "right"
    ),
    hr(),
    uiOutput("backend_badge")
  ),

  layout_columns(
    col_widths = c(4, 8),

    # --- Left card: Abstract details + score breakdown ---
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Abstract Details"),
        uiOutput("status_badge")
      ),
      card_body(
        tags$div(class = "mb-2",
          tags$strong("Title: "), textOutput("abs_title", inline = TRUE)
        ),
        tags$div(class = "mb-2",
          tags$strong("Authors: "), uiOutput("abs_authors", inline = TRUE)
        ),
        tags$div(class = "mb-2",
          tags$strong("Session: "), uiOutput("abs_session", inline = TRUE),
          help_icon("Which AAGL session the abstract was presented in — Oral Presentations or Video Presentations. (The JMIG supplement has no poster section.)")
        ),
        tags$div(class = "mb-2",
          uiOutput("abs_sciencedirect_link"),
          help_icon("Open the original abstract page on ScienceDirect in a new tab.")
        ),
        tags$div(class = "mb-2",
          tags$strong("Pub type (top candidate): "),
          uiOutput("abs_pub_type_badge", inline = TRUE),
          help_icon("PubMed PublicationType for the highest-scored candidate: Journal Article / Review / RCT/Trial / Case Report / Editorial-Letter / Observational Study.")
        ),
        tags$div(class = "mb-2",
          tags$strong("Author context: "),
          uiOutput("abs_author_context", inline = TRUE),
          help_icon("n authors / n unique affiliations (from PubMed author list). First-author state + ACOG district (parsed from affiliation) and gender (inferred from given name via Social Security data) apply only when we have a PubMed match for the abstract.")
        ),
        tags$div(class = "mb-2",
          tags$strong("Total Score: "), uiOutput("score_badge", inline = TRUE),
          help_icon(HTML(paste(
            "Sum of matching-signal points for the top PubMed candidate.",
            "Components: title similarity, abstract-text overlap, first/last/",
            "coauthor matches, team bonus, journal, keywords, publication date,",
            "minus a no-text-evidence penalty.<br><br>",
            "Badge color: green &ge; 7 (auto-accept), yellow 4–6.9 (review),",
            "red &lt; 4 (reject).<br><br>",
            "<b>(TIE)</b> means two or more candidates share the top score — the",
            "algorithm can't pick between them, so the abstract always goes to",
            "manual review. <b>0.0 (TIE)</b> means every candidate scored zero",
            "(no overlap at all) — usually this is a 'No Match'."
          )))
        ),
        tags$div(class = "mb-2",
          uiOutput("pubmed_link")
        ),
        hr(),
        h6("Abstract Text"),
        div(style = "max-height: 200px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px; font-size: 0.85em; line-height: 1.5;",
          uiOutput("abs_full_text_html")
        ),
        hr(),
        uiOutput("conflict_panel")
      )
    ),

    # --- Right card: Candidates table + decision ---
    card(
      class = "candidate-card",
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Candidate Publications"),
        help_icon(HTML(paste(
          "PubMed papers that the search pipeline surfaced as possible",
          "publications of this abstract, sorted by Score (highest first).<br><br>",
          "<b>Columns:</b><br>",
          "• <b>Score</b>: total matching points (same scale as Total Score).<br>",
          "• <b>Title Sim</b>: cosine similarity of title bigrams (0–1).<br>",
          "• <b>Pub Title / Journal / Year</b>: from PubMed.<br>",
          "• <b>PMID</b>: click to open the record on PubMed.<br><br>",
          "Click a row to expand that candidate's abstract for side-by-side reading."
        )))
      ),
      card_body(
        div(class = "candidate-workspace",
          div(class = "candidate-table-wrap",
            DTOutput("candidates_table")
          )
        ),
        uiOutput("candidate_abstract_panel"),
        div(class = "decision-panel",
          h5("Decision"),
          div(class = "d-flex gap-3 align-items-end mb-2",
            div(style = "flex: 0 0 auto;",
              tooltip(
                textInput("reviewer_initials", tagList("Reviewer Initials:", help_icon("2–4 uppercase letters identifying you (e.g., TM, ABC). Required so multi-reviewer conflicts can be detected.")), width = "100px"),
                "Your initials are stamped on every saved decision.",
                placement = "top"
              )
            ),
            div(style = "flex: 1;",
              radioButtons("decision",
                           tagList("Match decision:",
                                   help_icon(HTML(paste(
                                     "<b>Confirmed match</b>: the PMID (best or overridden) is the published version of this abstract.<br>",
                                     "<b>No match found</b>: you've reviewed the candidates and none correspond.<br>",
                                     "<b>Skip / Unsure</b>: can't decide — come back later."
                                   )))),
                           choices = c("Confirmed match" = "match",
                                       "No match found" = "no_match",
                                       "Skip / Unsure" = "skip"),
                           selected = "skip", inline = TRUE)
            )
          ),
          div(class = "d-flex gap-2 align-items-end",
            div(style = "flex: 1;",
              tooltip(
                textInput("manual_pmid", tagList("Override PMID (if different from best):", help_icon("Enter a 7-8 digit PubMed ID if the correct match isn't the top candidate. Leave blank to accept the 'best_pmid' on a match decision."))),
                "Use when the right paper is lower in the candidate list or not listed at all.",
                placement = "top"
              )
            ),
            tooltip(
              actionButton("btn_use_selected_pmid", "Use selected PMID", icon = icon("arrow-down"), class = "btn-outline-primary mb-3"),
              "Copy the selected candidate row's PMID into the override field and mark this as a confirmed match.",
              placement = "top"
            )
          ),
          tooltip(
            textAreaInput("notes", tagList("Reviewer notes:", help_icon("Optional free-text rationale. Helpful for edge cases and conflict resolution between reviewers.")), rows = 2),
            "These notes ship to the Google Sheet alongside your decision.",
            placement = "top"
          ),
          tooltip(
            actionButton("btn_save", "Save Decision", class = "btn-primary", icon = icon("floppy-disk")),
            "Writes your decision to Google Sheets (or local CSV fallback). Also confirms conflicts if another reviewer already decided.",
            placement = "top"
          ),
          tags$div(class = "text-muted small mt-2",
            tags$strong("Keyboard shortcuts: "),
            tags$span("m = match, n = no match, s = skip, Enter = save, "), tags$br(),
            tags$span("Arrow keys or [ ] = prev/next (when not in a text field)")
          )
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  # --- Restore reviewer initials from browser localStorage ---
  observeEvent(input$ls_reviewer_initials, {
    if (nchar(trimws(isolate(input$reviewer_initials))) == 0) {
      updateTextInput(session, "reviewer_initials", value = input$ls_reviewer_initials)
    }
  }, once = TRUE)

  # --- Candidate table proxy (used for auto-selection) ---
  cands_proxy <- DT::dataTableProxy("candidates_table")

  # --- Google Sheets state ---
  use_gs <- reactiveVal(FALSE)
  sheet_id <- reactiveVal(NULL)

  # Initialize backend on startup
  observe({
    sid <- get_sheet_id()
    if (!is.null(sid)) {
      if (gs_auth_init()) {
        ok <- tryCatch({
          sheets <- sheet_names(sid)
          if (!"decisions" %in% sheets) {
            sheet_add(sid, sheet = "decisions")
            range_write(sid, empty_decisions(), sheet = "decisions",
                        col_names = TRUE)
          } else {
            gs_ensure_headers(sid)
          }
          TRUE
        }, error = function(e) FALSE)
        if (ok) {
          use_gs(TRUE)
          sheet_id(sid)
          showNotification("Connected to Google Sheets backend", type = "message")
        } else {
          showNotification("Google Sheets unavailable, using local CSV", type = "warning")
        }
      } else {
        showNotification("No Google credentials found, using local CSV", type = "warning")
      }
    }
  }) |> bindEvent(TRUE, once = TRUE)

  # --- Data reactive ---
  data <- reactiveVal(NULL)

  observe({
    data(load_data(use_gs(), sheet_id()))
    # Derive year choices from loaded data so new congress years are picked up
    # automatically without code changes.
    d <- data()
    if (!is.null(d) && "congress_year" %in% names(d$abstracts) &&
        nrow(d$abstracts) > 0) {
      yrs <- sort(unique(na.omit(as.character(d$abstracts$congress_year))))
      yr_choices <- c("All" = "all", setNames(yrs, yrs))
      updateRadioButtons(session, "filter_year", choices = yr_choices,
                         selected = "all", inline = TRUE)
    }
  }) |> bindEvent(TRUE, once = TRUE)

  # Also reload after gs state is set (runs once after auth)
  observe({
    req(!is.null(data()))
    if (use_gs()) {
      data(load_data(TRUE, sheet_id()))
    }
  }) |> bindEvent(use_gs(), ignoreInit = TRUE)

  # --- Deduped decisions (latest per abstract_id + reviewer) ---
  decisions_dedup <- reactive({
    d <- data()
    req(d)
    dedup_decisions(d$decisions)
  })

  # --- Backend badge ---
  output$backend_badge <- renderUI({
    if (use_gs()) {
      sid <- sheet_id()
      url <- paste0("https://docs.google.com/spreadsheets/d/", sid)
      tags$a(href = url, target = "_blank", class = "btn btn-sm btn-outline-success w-100 text-decoration-none",
             icon("cloud"), " Open Google Sheet ", icon("up-right-from-square"))
    } else {
      csv_path <- app_path("output", "manual_review_decisions.csv")
      tags$div(
        span(class = "badge bg-secondary", icon("file"), " Local CSV"),
        tags$div(class = "small mt-1", style = "word-break: break-all;",
          tags$a(href = paste0("file://", csv_path), target = "_blank",
                 csv_path)
        )
      )
    }
  })

  # --- Conflict detection ---
  conflict_status <- reactive({
    dd <- decisions_dedup()
    if (nrow(dd) == 0) return(tibble(abstract_id = character(), status = character()))

    dd |>
      group_by(abstract_id) |>
      summarise(
        n_reviewers = n_distinct(reviewer, na.rm = TRUE),
        n_decisions = n_distinct(manual_decision),
        decisions_list = paste(unique(paste0(reviewer, ": ", manual_decision)), collapse = "; "),
        status = case_when(
          n_reviewers >= 2 & n_decisions > 1 ~ "Conflict",
          n_reviewers >= 2 & n_decisions == 1 ~ "Agreement",
          n_reviewers == 1 ~ "Single reviewer",
          TRUE ~ "Pending"
        ),
        .groups = "drop"
      )
  })

  # --- Conflict summary in sidebar ---
  output$conflict_summary <- renderUI({
    cs <- conflict_status()
    if (nrow(cs) == 0) return(NULL)
    n_conflict <- sum(cs$status == "Conflict")
    n_agree    <- sum(cs$status == "Agreement")
    n_single   <- sum(cs$status == "Single reviewer")

    if (n_conflict == 0 && n_agree == 0) return(NULL)

    parts <- list()
    if (n_conflict > 0) {
      parts <- c(parts, list(
        span(class = "badge bg-danger", paste(n_conflict, "conflicts"))
      ))
    }
    if (n_agree > 0) {
      parts <- c(parts, list(
        span(class = "badge bg-success ms-1", paste(n_agree, "agreements"))
      ))
    }
    total_multi <- n_conflict + n_agree
    if (total_multi > 0) {
      pct <- round(n_agree / total_multi * 100)
      parts <- c(parts, list(
        tags$div(class = "small text-muted mt-1",
                 sprintf("Agreement: %d%% (%d/%d multi-reviewer)", pct, n_agree, total_multi))
      ))
    }
    tags$div(class = "mt-2", do.call(tagList, parts))
  })

  # --- Conflict panel in left card ---
  output$conflict_panel <- renderUI({
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    dd <- decisions_dedup()
    abs_decisions <- dd |> filter(abstract_id == abs_id)
    if (nrow(abs_decisions) <= 1) return(NULL)

    cs <- conflict_status() |> filter(abstract_id == abs_id)
    if (nrow(cs) == 0) return(NULL)

    badge_class <- if (cs$status[1] == "Conflict") "bg-danger" else "bg-success"

    rows <- lapply(seq_len(nrow(abs_decisions)), function(i) {
      r <- abs_decisions[i, ]
      dec_label <- switch(r$manual_decision,
        "match" = span(class = "badge bg-success", "Match"),
        "no_match" = span(class = "badge bg-danger", "No Match"),
        "skip" = span(class = "badge bg-warning text-dark", "Skip"),
        span(class = "badge bg-secondary", r$manual_decision)
      )
      tags$div(class = "d-flex justify-content-between align-items-center mb-1",
        tags$strong(r$reviewer %||% "Unknown"),
        dec_label
      )
    })

    tags$details(
      tags$summary(
        span(class = paste("badge", badge_class, "me-1"), cs$status[1]),
        sprintf("%d reviewers", nrow(abs_decisions))
      ),
      tags$div(class = "mt-2 p-2 border rounded", do.call(tagList, rows))
    )
  })

  # --- Visible IDs (respects filters) ---
  visible_ids <- reactive({
    d <- data()
    req(d)
    if (nrow(d$review_queue) == 0) return(character(0))
    rq <- d$review_queue
    yr <- input$filter_year %||% "all"
    if (yr != "all" && "congress_year" %in% names(rq)) {
      rq <- rq[as.character(rq$congress_year) == yr, , drop = FALSE]
    }
    cls <- input$filter_class %||% "all"
    if (cls != "all" && "classification" %in% names(rq)) {
      rq <- rq[rq$classification %in% cls, , drop = FALSE]
    }
    ids <- rq$abstract_id
    if (isTRUE(input$filter_unreviewed)) {
      reviewed <- unique(d$decisions$abstract_id)
      ids <- ids[!ids %in% reviewed]
    }
    ids
  })

  # --- Build selector labels with checkmarks + conflict dots ---
  make_choices <- function(ids, d) {
    reviewed <- unique(d$decisions$abstract_id)
    cs <- conflict_status()
    class_glyph <- c(definite = "[D]", probable = "[P]", possible = "[?]",
                     no_match = "[x]", excluded = "[E]", no_candidates = "[0]")
    labels <- vapply(ids, function(id) {
      row <- d$review_queue[d$review_queue$abstract_id == id, ]
      conflict_row <- cs |> filter(abstract_id == id)
      prefix <- if (nrow(conflict_row) > 0 && conflict_row$status[1] == "Conflict") {
        "\U0001F534 "
      } else if (id %in% reviewed) {
        "\u2713 "
      } else {
        "\u2022 "
      }
      cls_tag <- if ("classification" %in% names(row) && nrow(row) > 0 &&
                     !is.na(row$classification[1])) {
        paste0(class_glyph[row$classification[1]] %||% "", " ")
      } else ""
      yr_tag <- if ("congress_year" %in% names(row) && nrow(row) > 0 &&
                    !is.na(row$congress_year[1])) {
        paste0("'", substr(as.character(row$congress_year[1]), 3, 4), " ")
      } else ""
      paste0(prefix, cls_tag, yr_tag, id, ": ", str_trunc(row$title[1], 38))
    }, character(1))
    setNames(ids, labels)
  }

  # Populate abstract selector
  observe({
    d <- data()
    req(d)
    ids <- visible_ids()
    if (length(ids) > 0) {
      choices <- make_choices(ids, d)
      current <- isolate(input$abstract_select)
      sel <- if (!is.null(current) && current %in% ids) current else ids[1]
      updateSelectInput(session, "abstract_select", choices = choices, selected = sel)
    } else {
      updateSelectInput(session, "abstract_select", choices = c("No abstracts" = ""), selected = "")
    }
  })

  # --- Navigation ---
  navigate_next <- function() {
    ids <- visible_ids()
    current_idx <- match(input$abstract_select, ids)
    if (!is.na(current_idx) && current_idx < length(ids)) {
      updateSelectInput(session, "abstract_select", selected = ids[current_idx + 1])
    }
  }

  navigate_prev <- function() {
    ids <- visible_ids()
    current_idx <- match(input$abstract_select, ids)
    if (!is.na(current_idx) && current_idx > 1) {
      updateSelectInput(session, "abstract_select", selected = ids[current_idx - 1])
    }
  }

  observeEvent(input$btn_next, navigate_next())
  observeEvent(input$btn_prev, navigate_prev())

  # --- Keyboard shortcut handlers ---
  observeEvent(input$kb_decision, {
    updateRadioButtons(session, "decision", selected = input$kb_decision)
  })

  observeEvent(input$kb_save, {
    click("btn_save")
  })

  observeEvent(input$kb_next, navigate_next())
  observeEvent(input$kb_prev, navigate_prev())

  # --- Current abstract reactive ---
  current_abstract <- reactive({
    req(input$abstract_select, input$abstract_select != "")
    d <- data()
    req(d)
    d$review_queue |> filter(abstract_id == input$abstract_select)
  })

  # --- Best candidate scores (from nested tibble) ---
  best_candidate_scores <- reactive({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    row <- d$scores_detail |> filter(abstract_id == abs_id)
    if (nrow(row) == 0 || is.null(row$score_details[[1]])) return(NULL)
    details <- row$score_details[[1]]
    if (nrow(details) == 0) return(NULL)
    details |> arrange(desc(total_score)) |> slice(1)
  })

  # --- Restore decisions when abstract changes ---
  observeEvent(input$abstract_select, {
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    if (is.null(abs_id) || abs_id == "") return()

    # Show the current reviewer's previous decision if it exists
    reviewer <- trimws(toupper(input$reviewer_initials))
    dd <- decisions_dedup()
    prev <- dd |> filter(abstract_id == abs_id)

    # Prefer current reviewer's decision, else show first
    if (nchar(reviewer) > 0) {
      own <- prev |> filter(reviewer == !!reviewer)
      if (nrow(own) > 0) prev <- own
    }

    if (nrow(prev) > 0) {
      updateRadioButtons(session, "decision", selected = prev$manual_decision[1])
      pmid_val <- if (!is.na(prev$manual_pmid[1])) as.character(prev$manual_pmid[1]) else ""
      updateTextInput(session, "manual_pmid", value = pmid_val)
      notes_val <- if (!is.na(prev$reviewer_notes[1])) prev$reviewer_notes[1] else ""
      updateTextAreaInput(session, "notes", value = notes_val)
    } else {
      # No prior human decision — pre-fill from algorithm classification so
      # reviewers can confirm with one click rather than changing from "skip".
      rq_row <- d$review_queue[d$review_queue$abstract_id == abs_id, , drop = FALSE]
      algo_cls <- if (nrow(rq_row) > 0 && "classification" %in% names(rq_row))
        rq_row$classification[1] else NA_character_
      default_decision <- dplyr::case_when(
        algo_cls %in% c("definite")                          ~ "match",
        algo_cls %in% c("no_match", "no_candidates", "excluded") ~ "no_match",
        TRUE                                                  ~ "skip"
      )
      updateRadioButtons(session, "decision", selected = default_decision)
      updateTextInput(session, "manual_pmid", value = "")
      updateTextAreaInput(session, "notes", value = "")
    }
  }, ignoreInit = TRUE)

  # Auto-select top-scored candidate row whenever the abstract changes
  observeEvent(input$abstract_select, {
    DT::selectRows(cands_proxy, NULL)
    shinyjs::delay(200, DT::selectRows(cands_proxy, 1))
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # --- Outputs: abstract details ---
  output$abs_title <- renderText({
    a <- current_abstract()
    if (nrow(a) > 0) a$title[1] else ""
  })

  output$abs_authors <- renderUI({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    row <- d$abstracts |> filter(abstract_id == abs_id)
    if (nrow(row) > 0 && "authors_raw" %in% names(row) && !is.na(row$authors_raw[1])) {
      span(style = "font-size: 0.9em;", row$authors_raw[1])
    } else {
      a <- current_abstract()
      if (nrow(a) > 0) span(a$first_author_normalized[1]) else span("")
    }
  })

  output$abs_author_context <- renderUI({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    row <- d$review_queue[d$review_queue$abstract_id == abs_id, , drop = FALSE]
    if (nrow(row) == 0) return(tags$em("—"))
    get <- function(c) if (c %in% names(row) && !is.na(row[[c]][1])) row[[c]][1] else NA

    na <- get("n_authors"); nua <- get("n_unique_affiliations")
    st <- get("first_author_state"); dist <- get("first_author_acog_district")
    g  <- get("first_author_gender")

    bits <- list()
    if (!is.na(na))  bits[[length(bits)+1]] <- sprintf("%s authors", na)
    if (!is.na(nua)) bits[[length(bits)+1]] <- sprintf("%s affiliation%s", nua, if (nua == 1) "" else "s")
    if (!is.na(st) || !is.na(dist)) {
      lbl <- paste0(na_if(st, ""), if (!is.na(dist)) sprintf(" (ACOG %s)", dist) else "")
      bits[[length(bits)+1]] <- lbl
    }
    if (!is.na(g)) bits[[length(bits)+1]] <- sprintf("1st au: %s", g)
    if (length(bits) == 0) return(tags$em("n/a"))
    span(style = "font-size: 0.9em;", paste(unlist(bits), collapse = " • "))
  })

  output$abs_pub_type_badge <- renderUI({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    row <- d$review_queue[d$review_queue$abstract_id == abs_id, , drop = FALSE]
    pt <- if (nrow(row) > 0 && "pub_type_canonical" %in% names(row) &&
              !is.na(row$pub_type_canonical[1])) row$pub_type_canonical[1] else NA_character_
    if (is.na(pt)) return(span(class = "badge bg-secondary", "Unknown"))
    cls <- switch(pt,
      "Journal Article"     = "bg-primary",
      "Review"              = "bg-info",
      "RCT/Trial"           = "bg-success",
      "Case Report"         = "bg-warning text-dark",
      "Editorial/Letter"    = "bg-secondary",
      "Observational Study" = "bg-primary",
      "bg-secondary")
    span(class = paste("badge", cls), pt)
  })

  output$abs_sciencedirect_link <- renderUI({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    row <- d$abstracts |> filter(abstract_id == abs_id)
    url <- if (nrow(row) > 0 && "article_url" %in% names(row) &&
               !is.na(row$article_url[1]) && nchar(row$article_url[1]) > 0) {
      row$article_url[1]
    } else NA_character_
    if (is.na(url)) return(tags$em("No ScienceDirect link available."))
    tags$a(href = url, target = "_blank", rel = "noopener",
           icon("up-right-from-square"), " View on ScienceDirect")
  })

  output$abs_session <- renderUI({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    row <- d$abstracts |> filter(abstract_id == abs_id)
    st <- if (nrow(row) > 0 && "session_type" %in% names(row) &&
              !is.na(row$session_type[1])) row$session_type[1] else NA_character_
    if (is.na(st)) return(span(class = "badge bg-secondary", "Unknown"))
    cls <- switch(st,
                  "Oral"  = "bg-primary",
                  "Video" = "bg-info",
                  "Poster" = "bg-warning text-dark",
                  "bg-secondary")
    span(class = paste("badge", cls), st)
  })

  # Format abstract text with section headers as paragraphs
  format_abstract_html <- function(txt) {
    if (is.na(txt) || nchar(txt) == 0) return(tags$em("No abstract text available."))

    # Use gregexpr to find header positions, then manually split.
    # strsplit with zero-width lookaheads fragments text incorrectly in R.
    headers_re <- "\\bStudy Objective\\b|\\bObjective\\b|\\bDesign\\b|\\bSetting\\b|\\bPatients or Participants\\b|\\bPatients\\b|\\bParticipants\\b|\\bIntervention\\b|\\bMeasurements\\b|\\bResults\\b|\\bConclusion\\b|\\bMethods\\b|\\bBackground\\b|\\bPurpose\\b"
    m <- gregexpr(headers_re, txt, perl = TRUE)[[1]]

    if (m[1] == -1) {
      return(tags$p(style = "margin-bottom: 6px;", txt))
    }

    positions <- as.integer(m)
    lengths   <- attr(m, "match.length")

    # Remove overlapping matches (e.g. "Objective" inside "Study Objective")
    keep <- rep(TRUE, length(positions))
    for (i in seq_along(positions)) {
      if (!keep[i]) next
      for (j in seq_along(positions)) {
        if (i != j && keep[j] &&
            positions[j] > positions[i] &&
            positions[j] < positions[i] + lengths[i]) {
          keep[j] <- FALSE
        }
      }
    }
    positions <- positions[keep]
    lengths   <- lengths[keep]

    # Build section list
    tags_list <- list()

    # Text before first header
    if (positions[1] > 1) {
      pre <- trimws(substr(txt, 1, positions[1] - 1))
      if (nchar(pre) > 0) {
        tags_list[[length(tags_list) + 1]] <- tags$p(style = "margin-bottom: 6px;", pre)
      }
    }

    for (i in seq_along(positions)) {
      header <- substr(txt, positions[i], positions[i] + lengths[i] - 1)
      body_start <- positions[i] + lengths[i]
      body_end   <- if (i < length(positions)) positions[i + 1] - 1 else nchar(txt)
      body <- trimws(substr(txt, body_start, body_end))
      tags_list[[length(tags_list) + 1]] <- tags$p(
        style = "margin-bottom: 6px;",
        tags$strong(header), " ", body
      )
    }

    do.call(tagList, tags_list)
  }

  output$abs_full_text_html <- renderUI({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    row <- d$abstracts |> filter(abstract_id == abs_id)
    if (nrow(row) > 0 && "abstract_text" %in% names(row) && !is.na(row$abstract_text[1])) {
      format_abstract_html(row$abstract_text[1])
    } else {
      a <- current_abstract()
      if (nrow(a) == 0) return(tags$em("No abstract text available."))
      parts <- c(a$abstract_objective[1], a$abstract_conclusion[1])
      parts <- parts[!is.na(parts) & parts != ""]
      if (length(parts) > 0) format_abstract_html(paste(parts, collapse = " ")) else tags$em("No abstract text available.")
    }
  })

  # --- Status badge ---
  output$status_badge <- renderUI({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    if (is.null(abs_id) || abs_id == "") return(span(class = "badge bg-secondary", "Pending"))
    dd <- decisions_dedup()
    prev <- dd |> filter(abstract_id == abs_id)
    if (nrow(prev) == 0) return(span(class = "badge bg-secondary", "Pending"))

    # Show conflict status if multiple reviewers
    cs <- conflict_status() |> filter(abstract_id == abs_id)
    if (nrow(cs) > 0 && cs$status[1] == "Conflict") {
      return(span(class = "badge bg-danger", "Conflict"))
    }

    dec <- prev$manual_decision[1]
    switch(dec,
      "match"    = span(class = "badge bg-success", "Matched"),
      "no_match" = span(class = "badge bg-danger", "No Match"),
      "skip"     = span(class = "badge bg-warning text-dark", "Skipped"),
      span(class = "badge bg-secondary", "Pending")
    )
  })

  # --- Score badge ---
  output$score_badge <- renderUI({
    a <- current_abstract()
    if (nrow(a) == 0) return(NULL)
    score <- a$best_score[1]
    tie_text <- if (isTRUE(a$has_tie[1])) " (TIE)" else ""
    bg <- if (score >= 7) "bg-success" else if (score >= 4) "bg-warning text-dark" else "bg-danger"
    span(class = paste("badge", bg), sprintf("%.1f%s", score, tie_text))
  })

  # --- PubMed search link ---
  output$pubmed_link <- renderUI({
    a <- current_abstract()
    if (nrow(a) == 0) return(NULL)
    query <- utils::URLencode(a$title[1])
    url <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", query)
    tags$a(href = url, target = "_blank", class = "btn btn-outline-secondary btn-sm",
           icon("magnifying-glass"), " Search PubMed")
  })

  # --- Candidate rows with score details, ordered as shown in the table ---
  candidate_rows <- reactive({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")

    cands <- d$candidates |>
      filter(abstract_id == abs_id)

    if (nrow(cands) == 0) return(cands)

    score_row <- d$scores_detail |> filter(abstract_id == abs_id)
    if (nrow(score_row) > 0 && !is.null(score_row$score_details[[1]])) {
      sd <- score_row$score_details[[1]] |>
        select(pmid, total_score, title_pts, abstract_pts,
               first_au_pts, last_au_pts, journal_pts, date_pts, no_text_penalty)
      cands <- cands |>
        left_join(sd, by = "pmid") |>
        arrange(desc(total_score))
    }

    cands
  })

  selected_candidate <- reactive({
    sel <- input$candidates_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    cands <- candidate_rows()
    if (nrow(cands) == 0 || sel[1] > nrow(cands)) return(NULL)
    cands[sel[1], , drop = FALSE]
  })

  current_abstract_text <- reactive({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    row <- d$abstracts |> filter(abstract_id == abs_id)
    if (nrow(row) > 0 && "abstract_text" %in% names(row) && !is.na(row$abstract_text[1])) {
      return(row$abstract_text[1])
    }
    a <- current_abstract()
    if (nrow(a) == 0) return(NA_character_)
    parts <- c(a$abstract_objective[1], a$abstract_conclusion[1])
    parts <- parts[!is.na(parts) & parts != ""]
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = " ")
  })

  # Score-chip builder: renders one badge per scoring component
  # Tooltip descriptions for each scoring component
  CHIP_TIPS <- list(
    title_pts      = "Title similarity: cosine similarity of title bigrams × weight. Max ~3 pts. Zero means the titles share no word-pairs.",
    abstract_pts   = "Abstract text overlap: TF-IDF cosine similarity between conference abstract and candidate abstract. Max ~2 pts. Zero when either abstract is missing.",
    first_au_pts   = "First-author match: fuzzy name comparison between abstract's first author and candidate's first author. Max ~1.5 pts.",
    last_au_pts    = "Last-author match: fuzzy name comparison between abstract's last author and candidate's last author. Max ~2 pts.",
    journal_pts    = "Journal match: bonus when the candidate is published in JMIG (the target journal). Max ~1 pt.",
    date_pts       = "Publication date: bonus when candidate was published 6–36 months after the congress date — the expected window for a conference-to-journal pipeline. Max ~1 pt.",
    no_text_penalty = "No-text-evidence penalty: negative adjustment applied when neither title similarity nor abstract overlap provides any signal. Penalises coincidental author/journal matches."
  )

  make_score_chips <- function(cand_row) {
    chip <- function(label, col) {
      val <- if (col %in% names(cand_row)) cand_row[[col]][1] else NA
      if (is.na(val)) return(NULL)
      v <- suppressWarnings(as.numeric(val))
      if (is.na(v)) return(NULL)
      cls  <- paste("score-chip", if (v > 0) "positive" else if (v < 0) "negative" else "neutral")
      tip  <- CHIP_TIPS[[col]] %||% label
      span(class = cls,
           `data-bs-toggle` = "tooltip",
           `data-bs-placement` = "top",
           title = tip,
           sprintf("%s %+.1f", label, v))
    }
    tagList(
      chip("Title",    "title_pts"),
      chip("Abstract", "abstract_pts"),
      chip("1st Au",   "first_au_pts"),
      chip("Last Au",  "last_au_pts"),
      chip("Journal",  "journal_pts"),
      chip("Date",     "date_pts"),
      chip("Penalty",  "no_text_penalty")
    )
  }

  # --- Candidate abstract comparison (shown on row click) ---
  output$candidate_abstract_panel <- renderUI({
    cand_row <- selected_candidate()
    if (is.null(cand_row)) {
      return(div(class = "text-muted small mt-2", icon("hand-pointer"),
                 " Click a candidate row above to compare abstracts"))
    }

    pub_abs <- if ("pub_abstract" %in% names(cand_row) && !is.na(cand_row$pub_abstract[1])) {
      cand_row$pub_abstract[1]
    } else "No abstract available for this candidate."

    pub_authors <- if ("pub_all_authors" %in% names(cand_row) && !is.na(cand_row$pub_all_authors[1])) {
      cand_row$pub_all_authors[1]
    } else ""

    # Compact summary bar
    score_val  <- if ("total_score" %in% names(cand_row)) cand_row$total_score[1] else NA
    pub_year   <- if ("pub_year"    %in% names(cand_row)) cand_row$pub_year[1]    else NA
    pub_jrnl   <- if ("pub_journal" %in% names(cand_row)) cand_row$pub_journal[1] else NA
    first_au   <- if ("pub_first_author" %in% names(cand_row)) cand_row$pub_first_author[1] else NA
    last_au    <- if ("pub_last_author"  %in% names(cand_row)) cand_row$pub_last_author[1]  else NA
    pmid_link  <- tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/", cand_row$pmid[1], "/"),
                         target = "_blank", paste0("PMID ", cand_row$pmid[1]))

    summary_meta <- paste(
      Filter(function(x) !is.na(x) && nchar(as.character(x)) > 0,
             list(if (!is.na(score_val)) sprintf("Score %.2f", score_val),
                  as.character(pub_year),
                  pub_jrnl,
                  if (!is.na(first_au) || !is.na(last_au))
                    paste0(first_au %||% "?", " \u2192 ", last_au %||% "?")))
    , collapse = " | ")

    summary_bar <- div(class = "cand-summary-bar mt-2",
      tags$div(pmid_link, span(class = "text-muted mx-1", "|"), summary_meta),
      tags$div(class = "mt-1", make_score_chips(cand_row))
    )

    div(
      summary_bar,
      tags$details(open = "open",
        tags$summary(tags$strong("Compare abstracts")),
        div(class = "small text-muted mb-2",
          if ("pub_title" %in% names(cand_row) && !is.na(cand_row$pub_title[1])) {
            tags$span(cand_row$pub_title[1])
          },
          if (nchar(pub_authors) > 0) tags$div(pub_authors)
        ),
        div(class = "comparison-grid",
          div(
            tags$div(class = "fw-bold small mb-1",
              "Original abstract",
              help_icon(HTML(paste(
                "The AAGL conference abstract text scraped from ScienceDirect.",
                "Abstract text is available for <b>2019-2023</b> only.",
                "Congresses <b>2012-2018</b> do not have parseable abstract",
                "text on ScienceDirect (older page format), so this panel",
                "will show 'No abstract text available' for those years.",
                "The matching algorithm still works via title + author",
                "matching even without abstract text."
              )))
            ),
            div(class = "comparison-pane abstract-source",
              format_abstract_html(current_abstract_text())
            )
          ),
          div(
            tags$div(class = "fw-bold small mb-1", "Candidate publication abstract"),
            div(class = "comparison-pane candidate-source",
              format_abstract_html(pub_abs)
            )
          )
        )
      )
    )
  })

  observeEvent(input$btn_use_selected_pmid, {
    cand_row <- selected_candidate()
    if (is.null(cand_row)) {
      showNotification("Select a candidate row first.", type = "warning")
      return()
    }
    updateTextInput(session, "manual_pmid", value = as.character(cand_row$pmid[1]))
    updateRadioButtons(session, "decision", selected = "match")
    showNotification(paste("Using selected PMID", cand_row$pmid[1]), type = "message")
  })

  # --- Candidates table with per-candidate scores ---
  output$candidates_table <- renderDT({
    cands <- candidate_rows() |>
      select(any_of(c("pmid", "pub_title", "pub_first_author", "pub_last_author",
                       "pub_journal", "pub_year", "pub_doi", "total_score")))

    if (nrow(cands) == 0) return(datatable(tibble()))

    cands <- cands |>
      mutate(
        Link = if_else(!is.na(pub_doi),
                       paste0('<a href="https://doi.org/', pub_doi, '" target="_blank">DOI</a>'),
                       paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/', pmid, '/" target="_blank">PM</a>'))
      ) |>
      select(-any_of("pub_doi"))

    display <- cands |>
      rename(any_of(c(
        Score = "total_score", PMID = "pmid", Title = "pub_title",
        `1st Author` = "pub_first_author", `Last Author` = "pub_last_author",
        Journal = "pub_journal", Year = "pub_year"
      ))) |>
      select(any_of(c("Score", "PMID", "Title", "1st Author",
                      "Last Author", "Journal", "Year", "Link")))

    dt_opts <- list(
      pageLength = 5,
      scrollX = FALSE,
      scrollY = "230px",
      scrollCollapse = FALSE,
      autoWidth = FALSE
    )
    score_idx <- match("Score", names(display))
    if (!is.na(score_idx)) {
      dt_opts$order <- list(list(score_idx - 1L, "desc"))
    }
    title_idx <- match("Title", names(display))
    if (!is.na(title_idx)) {
      dt_opts$columnDefs <- list(
        list(targets = title_idx - 1L, width = "38%"),
        list(targets = "_all", className = "dt-top")
      )
    }

    dt <- datatable(display, escape = FALSE, selection = "single", rownames = FALSE, options = dt_opts)

    if ("Score" %in% names(display)) {
      dt <- dt |>
        formatStyle("Score",
                    backgroundColor = styleInterval(c(2, 4),
                                                    c("#ffcdd2", "#fff9c4", "#c8e6c9")))
    }
    dt
  })

  # --- Progress (scoped to the current classification filter) ---
  output$progress_count <- renderText({
    d <- data()
    req(d)
    ids <- visible_ids()
    total <- length(ids)
    reviewed <- sum(ids %in% unique(d$decisions$abstract_id))
    sprintf("%d / %d (%.0f%%)", reviewed, total,
            if (total > 0) reviewed / total * 100 else 0)
  })

  # --- Export / Download ---
  output$download_decisions <- downloadHandler(
    filename = function() {
      paste0("adjudication_decisions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (use_gs() && !is.null(sheet_id())) {
        decisions <- gs_read_decisions(sheet_id())
        if (is.null(decisions)) decisions <- data()$decisions
      } else {
        decisions <- data()$decisions
      }
      write_csv(decisions, file)
    }
  )

  # --- Save decision ---
  observeEvent(input$btn_save, {
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")

    # Validate reviewer initials: required, 2-4 uppercase letters
    initials <- trimws(toupper(input$reviewer_initials))
    if (nchar(initials) == 0) {
      showNotification("Please enter your reviewer initials before saving.", type = "error")
      return()
    }
    if (!grepl("^[A-Z]{2,4}$", initials)) {
      showNotification("Reviewer initials must be 2-4 uppercase letters (e.g., TM, ABC).", type = "error")
      return()
    }

    # Validate PMID format if provided
    pmid_input <- trimws(input$manual_pmid)
    if (nchar(pmid_input) > 0 && !grepl("^\\d{7,8}$", pmid_input)) {
      showNotification("PMID must be 7-8 digits (e.g., 12345678).", type = "error")
      return()
    }

    # Warn if match confirmed but no PMID available
    d <- data()
    if (input$decision == "match" && nchar(pmid_input) == 0) {
      best_pmid <- d$review_queue$best_pmid[d$review_queue$abstract_id == abs_id]
      if (length(best_pmid) == 0 || is.na(best_pmid[1]) || nchar(as.character(best_pmid[1])) == 0) {
        showNotification("Warning: Confirmed as match but no PMID is available. Consider adding an override PMID.", type = "warning")
      }
    }

    # Check for existing decision by a DIFFERENT reviewer — show confirmation
    dd <- decisions_dedup()
    existing <- dd |> filter(abstract_id == abs_id, reviewer != initials)
    if (nrow(existing) > 0) {
      other_decisions <- paste(existing$reviewer, "->", existing$manual_decision, collapse = ", ")
      showModal(modalDialog(
        title = "Other reviewer(s) have already decided",
        p(sprintf("Existing decisions: %s", other_decisions)),
        p("Your decision will be recorded alongside theirs. Continue?"),
        footer = tagList(
          actionButton("confirm_save", "Save Anyway", class = "btn-primary"),
          modalButton("Cancel")
        )
      ))
      return()
    }

    # No conflict — save directly
    do_save(abs_id, initials, pmid_input)
  })

  # Confirmed save after conflict modal
  observeEvent(input$confirm_save, {
    removeModal()
    abs_id <- input$abstract_select
    initials <- trimws(toupper(input$reviewer_initials))
    pmid_input <- trimws(input$manual_pmid)
    do_save(abs_id, initials, pmid_input)
  })

  # --- Actual save logic ---
  do_save <- function(abs_id, initials, pmid_input) {
    d <- data()

    # Only attach a PMID when the reviewer actually confirmed a match. For
    # "no_match" / "skip" we must NOT backfill from best_pmid, or the saved
    # row would look like the reviewer endorsed that candidate.
    final_pmid <- if (!identical(input$decision, "match")) {
      NA_character_
    } else if (nchar(pmid_input) > 0) {
      pmid_input
    } else {
      bp <- d$review_queue$best_pmid[d$review_queue$abstract_id == abs_id]
      if (length(bp) > 0) as.character(bp[1]) else NA_character_
    }

    # Abstract-level identifiers
    abs_row <- if (nrow(d$abstracts) > 0) {
      d$abstracts[d$abstracts$abstract_id == abs_id, , drop = FALSE]
    } else tibble()
    rq_row <- if (nrow(d$review_queue) > 0) {
      d$review_queue[d$review_queue$abstract_id == abs_id, , drop = FALSE]
    } else tibble()

    pick <- function(df, col) {
      if (nrow(df) == 0 || !col %in% names(df)) return(NA_character_)
      v <- df[[col]][1]
      if (is.null(v) || is.na(v)) NA_character_ else as.character(v)
    }

    abstract_title        <- pick(abs_row, "title")
    abstract_first_author <- pick(abs_row, "first_author_normalized")
    if (is.na(abstract_first_author)) {
      abstract_first_author <- pick(abs_row, "author_name_first")
    }
    abstract_subtype      <- pick(abs_row, "subtype")
    session_type          <- pick(abs_row, "session_type")
    congress_year         <- pick(abs_row, "congress_year")
    sciencedirect_url     <- pick(abs_row, "article_url")

    # Matched publication details (looked up by the chosen PMID)
    cand_row <- if (!is.na(final_pmid) && nchar(final_pmid) > 0 &&
                    nrow(d$candidates) > 0 && "pmid" %in% names(d$candidates)) {
      d$candidates[as.character(d$candidates$pmid) == final_pmid, , drop = FALSE]
    } else tibble()

    # Only populate matched-publication columns when the reviewer confirmed a
    # match. For no_match/skip, leave them blank so the row can't be mistaken
    # for an endorsement of the top candidate.
    if (identical(input$decision, "match")) {
      matched_pub_title   <- pick(cand_row, "pub_title")
      matched_pub_journal <- pick(cand_row, "pub_journal")
      matched_pub_year    <- pick(cand_row, "pub_year")
      if (is.na(matched_pub_title))   matched_pub_title   <- pick(rq_row, "pub_title")
      if (is.na(matched_pub_journal)) matched_pub_journal <- pick(rq_row, "pub_journal")
      matched_score            <- pick(rq_row, "best_score")
      matched_title_similarity <- pick(rq_row, "title_sim")
      matched_pub_type         <- pick(rq_row, "pub_type_canonical")
      matched_pub_types_raw    <- pick(rq_row, "pub_types")
    } else {
      matched_pub_title        <- NA_character_
      matched_pub_journal      <- NA_character_
      matched_pub_year         <- NA_character_
      matched_score            <- NA_character_
      matched_title_similarity <- NA_character_
      matched_pub_type         <- NA_character_
      matched_pub_types_raw    <- NA_character_
    }

    new_decision <- tibble(
      abstract_id              = abs_id,
      reviewer                 = initials,
      manual_decision          = input$decision,
      manual_pmid              = final_pmid,
      reviewer_notes           = input$notes,
      review_timestamp         = as.character(Sys.time()),
      abstract_title           = abstract_title,
      abstract_first_author    = abstract_first_author,
      abstract_subtype         = abstract_subtype,
      session_type             = session_type,
      congress_year            = congress_year,
      sciencedirect_url        = sciencedirect_url,
      matched_pub_title        = matched_pub_title,
      matched_pub_journal      = matched_pub_journal,
      matched_pub_year         = matched_pub_year,
      matched_score            = matched_score,
      matched_title_similarity = matched_title_similarity,
      matched_pub_type         = matched_pub_type,
      matched_pub_types_raw    = matched_pub_types_raw,
      n_authors                = pick(rq_row, "n_authors"),
      n_unique_affiliations    = pick(rq_row, "n_unique_affiliations"),
      first_author_state       = pick(rq_row, "first_author_state"),
      first_author_acog_district = pick(rq_row, "first_author_acog_district"),
      first_author_gender      = pick(rq_row, "first_author_gender")
    )

    saved_to_gs <- FALSE
    if (use_gs() && !is.null(sheet_id())) {
      saved_to_gs <- gs_append_decision(sheet_id(), new_decision)
      if (!saved_to_gs) {
        showNotification("Google Sheets write failed, saving to local CSV", type = "warning")
      }
    }

    # Always update local data; also write local CSV as backup/fallback
    updated_decisions <- bind_rows(d$decisions, new_decision)

    if (!saved_to_gs) {
      # Write to local CSV (replace old row for same abstract_id + reviewer)
      local_dedup <- dedup_decisions(updated_decisions)
      write_csv(local_dedup, app_path("output", "manual_review_decisions.csv"))
    }

    d$decisions <- updated_decisions
    data(d)

    showNotification(
      paste("Saved:", abs_id, "->", input$decision,
            if (saved_to_gs) "(Google Sheets)" else "(local CSV)"),
      type = "message"
    )

    # Auto-advance within visible list
    ids <- visible_ids()
    current_idx <- match(abs_id, ids)
    if (!is.na(current_idx) && current_idx < length(ids)) {
      updateSelectInput(session, "abstract_select", selected = ids[current_idx + 1])
    }
  }

  # --- Refresh ---
  observeEvent(input$btn_refresh, {
    data(load_data(use_gs(), sheet_id()))
    showNotification("Data refreshed", type = "message")
  })
}

shinyApp(ui = ui, server = server)
