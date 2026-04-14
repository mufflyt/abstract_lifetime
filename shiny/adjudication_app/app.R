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

# --- Google Sheets helpers ---

#' Get the Google Sheet ID from env or config
get_sheet_id <- function() {

  id <- Sys.getenv("GOOGLE_SHEETS_ID", "")
  if (nchar(id) == 0) {
    cfg_path <- here("config.yml")
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

#' Read decisions from Google Sheets
gs_read_decisions <- function(sheet_id) {
  tryCatch({
    d <- read_sheet(sheet_id, sheet = "decisions", col_types = "cccccc")
    if (nrow(d) == 0) {
      return(tibble(abstract_id = character(), reviewer = character(),
                    manual_decision = character(), manual_pmid = character(),
                    reviewer_notes = character(), review_timestamp = character()))
    }
    # Ensure all expected columns exist
    expected <- c("abstract_id", "reviewer", "manual_decision",
                  "manual_pmid", "reviewer_notes", "review_timestamp")
    for (col in expected) {
      if (!col %in% names(d)) d[[col]] <- NA_character_
    }
    d |> mutate(across(everything(), as.character),
                reviewer = if_else(is.na(reviewer) | reviewer == "NA", "AUTO", reviewer))
  }, error = function(e) {
    NULL
  })
}

#' Append a single decision row to Google Sheet
gs_append_decision <- function(sheet_id, new_row) {
  tryCatch({
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
  review_path     <- here("output", "manual_review_queue.csv")
  candidates_path <- here("data", "processed", "pubmed_candidates.csv")
  scores_path     <- here("data", "processed", "match_scores_detailed.rds")
  decisions_path  <- here("output", "manual_review_decisions.csv")
  abstracts_path  <- here("data", "processed", "abstracts_cleaned.csv")

  review_queue  <- if (file.exists(review_path)) read_csv(review_path, show_col_types = FALSE) else tibble()
  candidates    <- if (file.exists(candidates_path)) read_csv(candidates_path, show_col_types = FALSE) else tibble()
  scores_detail <- if (file.exists(scores_path)) readRDS(scores_path) else tibble()
  abstracts     <- if (file.exists(abstracts_path)) {
    read_csv(abstracts_path, show_col_types = FALSE) |>
      select(any_of(c("abstract_id", "abstract_text", "authors_raw",
                       "author_name_first", "author_name_last")))
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
      if (!"reviewer" %in% names(d))          d$reviewer          <- NA_character_
      if (!"reviewer_notes" %in% names(d))   d$reviewer_notes   <- NA_character_
      if (!"review_timestamp" %in% names(d)) d$review_timestamp <- NA_character_
      d |> mutate(across(everything(), as.character),
                  reviewer = if_else(is.na(reviewer) | reviewer == "NA", "AUTO", reviewer))
    } else {
      tibble(abstract_id = character(), reviewer = character(),
             manual_decision = character(), manual_pmid = character(),
             reviewer_notes = character(), review_timestamp = character())
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
"

# --- UI ---
ui <- page_sidebar(
  title = "AAGL 2023 Abstract-to-Publication Adjudication",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # shinyjs + keyboard handler
  useShinyjs(),
  tags$head(tags$script(HTML(keyboard_js))),

  sidebar = sidebar(
    width = 280,
    selectInput("abstract_select", "Select Abstract:", choices = NULL, selected = NULL),
    checkboxInput("filter_unreviewed", "Show unreviewed only", value = FALSE),
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
    downloadButton("download_decisions", "Export Decisions", class = "btn-success btn-sm w-100 mb-2"),
    actionButton("btn_refresh", "Refresh Data", icon = icon("rotate"), class = "btn-info btn-sm w-100"),
    hr(),
    uiOutput("backend_badge")
  ),

  layout_columns(
    col_widths = c(5, 7),

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
          tags$strong("Total Score: "), uiOutput("score_badge", inline = TRUE)
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
      card_header("Candidate Publications"),
      card_body(
        div(style = "max-height: 300px; overflow-y: auto;",
          DTOutput("candidates_table")
        ),
        uiOutput("candidate_abstract_panel"),
        hr(),
        h5("Decision"),
        div(class = "d-flex gap-3 align-items-end mb-2",
          div(style = "flex: 0 0 auto;",
            textInput("reviewer_initials", "Reviewer Initials:", width = "100px")
          ),
          div(style = "flex: 1;",
            radioButtons("decision", "Match decision:",
                         choices = c("Confirmed match" = "match",
                                     "No match found" = "no_match",
                                     "Skip / Unsure" = "skip"),
                         selected = "skip", inline = TRUE)
          )
        ),
        textInput("manual_pmid", "Override PMID (if different from best):"),
        textAreaInput("notes", "Reviewer notes:", rows = 2),
        actionButton("btn_save", "Save Decision", class = "btn-primary", icon = icon("floppy-disk")),
        tags$div(class = "text-muted small mt-2",
          tags$strong("Keyboard shortcuts: "),
          tags$span("m = match, n = no match, s = skip, Enter = save, "), tags$br(),
          tags$span("Arrow keys or [ ] = prev/next (when not in a text field)")
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
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
            # Create 'decisions' sheet with header
            header <- tibble(
              abstract_id = character(), reviewer = character(),
              manual_decision = character(), manual_pmid = character(),
              reviewer_notes = character(), review_timestamp = character()
            )
            sheet_add(sid, sheet = "decisions")
            range_write(sid, header, sheet = "decisions", col_names = TRUE)
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
      csv_path <- here("output", "manual_review_decisions.csv")
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

  # --- Visible IDs (respects filter) ---
  visible_ids <- reactive({
    d <- data()
    req(d)
    if (nrow(d$review_queue) == 0) return(character(0))
    ids <- d$review_queue$abstract_id
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
    labels <- vapply(ids, function(id) {
      row <- d$review_queue[d$review_queue$abstract_id == id, ]
      # Status prefix
      conflict_row <- cs |> filter(abstract_id == id)
      prefix <- if (nrow(conflict_row) > 0 && conflict_row$status[1] == "Conflict") {
        "\U0001F534 "
      } else if (id %in% reviewed) {
        "\u2713 "
      } else {
        "\u2022 "
      }
      paste0(prefix, id, ": ", str_trunc(row$title[1], 45))
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
      updateRadioButtons(session, "decision", selected = "skip")
      updateTextInput(session, "manual_pmid", value = "")
      updateTextAreaInput(session, "notes", value = "")
    }
  }, ignoreInit = TRUE)

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

  # Format abstract text with section headers as paragraphs
  format_abstract_html <- function(txt) {
    if (is.na(txt) || nchar(txt) == 0) return(tags$em("No abstract text available."))

    # Use gregexpr to find header positions, then manually split.
    # strsplit with zero-width lookaheads fragments text incorrectly in R.
    headers_re <- "Study Objective|Objective|Design|Setting|Patients or Participants|Patients|Participants|Intervention|Measurements|Results|Conclusion|Methods|Background|Purpose"
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


  # --- Candidate abstract comparison (shown on row click) ---
  output$candidate_abstract_panel <- renderUI({
    sel <- input$candidates_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(div(class = "text-muted small mt-2", icon("hand-pointer"),
                 " Click a candidate row above to compare abstracts"))
    }
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")

    cands <- d$candidates |> filter(abstract_id == abs_id)
    score_row <- d$scores_detail |> filter(abstract_id == abs_id)
    if (nrow(score_row) > 0 && !is.null(score_row$score_details[[1]])) {
      sd <- score_row$score_details[[1]] |> select(pmid, total_score)
      cands <- cands |> left_join(sd, by = "pmid") |> arrange(desc(total_score))
    }
    if (sel > nrow(cands)) return(NULL)
    selected_pmid <- cands$pmid[sel]

    cand_row <- d$candidates |> filter(pmid == selected_pmid, abstract_id == abs_id)
    if (nrow(cand_row) == 0) return(NULL)

    pub_abs <- if ("pub_abstract" %in% names(cand_row) && !is.na(cand_row$pub_abstract[1])) {
      cand_row$pub_abstract[1]
    } else "No abstract available for this candidate."

    pub_authors <- if ("pub_all_authors" %in% names(cand_row) && !is.na(cand_row$pub_all_authors[1])) {
      cand_row$pub_all_authors[1]
    } else ""

    div(class = "mt-2",
      tags$details(open = "open",
        tags$summary(tags$strong(sprintf("Candidate PMID %s — Abstract", selected_pmid))),
        if (nchar(pub_authors) > 0) div(class = "small text-muted mb-1", pub_authors),
        div(style = "max-height: 180px; overflow-y: auto; background: #f0f7ff; padding: 8px; border-radius: 4px; font-size: 0.85em;",
          pub_abs
        )
      )
    )
  })

  # --- Candidates table with per-candidate scores ---
  output$candidates_table <- renderDT({
    d <- data()
    req(d)
    abs_id <- input$abstract_select
    if (is.null(abs_id) || abs_id == "") return(datatable(tibble()))

    cands <- d$candidates |>
      filter(abstract_id == abs_id) |>
      select(any_of(c("pmid", "pub_title", "pub_first_author", "pub_last_author",
                       "pub_journal", "pub_year", "pub_doi")))

    if (nrow(cands) == 0) return(datatable(tibble()))

    score_row <- d$scores_detail |> filter(abstract_id == abs_id)
    if (nrow(score_row) > 0 && !is.null(score_row$score_details[[1]])) {
      sd <- score_row$score_details[[1]] |>
        select(pmid, total_score, title_pts, abstract_pts,
               first_au_pts, last_au_pts, journal_pts, date_pts, no_text_penalty)
      cands <- cands |>
        left_join(sd, by = "pmid") |>
        arrange(desc(total_score))
    }

    cands <- cands |>
      mutate(
        Link = if_else(!is.na(pub_doi),
                       paste0('<a href="https://doi.org/', pub_doi, '" target="_blank">DOI</a>'),
                       paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/', pmid, '/" target="_blank">PM</a>'))
      ) |>
      select(-any_of("pub_doi"))

    display <- cands |>
      rename(any_of(c(
        PMID = "pmid", Title = "pub_title",
        `1st Author` = "pub_first_author", `Last Author` = "pub_last_author",
        Journal = "pub_journal", Year = "pub_year", Score = "total_score",
        `Title Pts` = "title_pts", `Abstr` = "abstract_pts",
        `1st Au Pts` = "first_au_pts", `Last Au Pts` = "last_au_pts",
        `Jrnl Pts` = "journal_pts", `Date` = "date_pts",
        Penalty = "no_text_penalty"
      )))

    dt_opts <- list(pageLength = 5, scrollX = TRUE)
    score_idx <- match("Score", names(display))
    if (!is.na(score_idx)) {
      dt_opts$order <- list(list(score_idx - 1L, "desc"))
    }
    title_idx <- match("Title", names(display))
    if (!is.na(title_idx)) {
      dt_opts$columnDefs <- list(list(targets = title_idx - 1L, width = "200px"))
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

  # --- Progress ---
  output$progress_count <- renderText({
    d <- data()
    req(d)
    total <- nrow(d$review_queue)
    reviewed <- sum(d$review_queue$abstract_id %in% unique(d$decisions$abstract_id))
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

    final_pmid <- if (nchar(pmid_input) > 0) {
      pmid_input
    } else {
      bp <- d$review_queue$best_pmid[d$review_queue$abstract_id == abs_id]
      if (length(bp) > 0) as.character(bp[1]) else NA_character_
    }

    new_decision <- tibble(
      abstract_id      = abs_id,
      reviewer         = initials,
      manual_decision  = input$decision,
      manual_pmid      = final_pmid,
      reviewer_notes   = input$notes,
      review_timestamp = as.character(Sys.time())
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
      write_csv(local_dedup, here("output", "manual_review_decisions.csv"))
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
