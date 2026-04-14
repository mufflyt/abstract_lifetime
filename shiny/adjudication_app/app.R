# Shiny Adjudication App — Manual review of abstract-to-publication matches
# Left panel: score breakdown chart; Right panel: candidate papers with scores

library(shiny)
library(bslib)
library(DT)
library(readr)
library(dplyr)
library(stringr)
library(here)

# --- Data loading ---
load_data <- function() {
  review_path    <- here("output", "manual_review_queue.csv")
  candidates_path <- here("data", "processed", "pubmed_candidates.csv")
  scores_path    <- here("data", "processed", "match_scores_detailed.rds")
  decisions_path <- here("output", "manual_review_decisions.csv")

  review_queue <- if (file.exists(review_path)) read_csv(review_path, show_col_types = FALSE) else tibble()
  candidates   <- if (file.exists(candidates_path)) read_csv(candidates_path, show_col_types = FALSE) else tibble()
  scores_detail <- if (file.exists(scores_path)) readRDS(scores_path) else tibble()

  # Load existing decisions — coalesce missing columns from older CSVs

  decisions <- if (file.exists(decisions_path)) {
    d <- read_csv(decisions_path, show_col_types = FALSE)
    if (!"reviewer_notes" %in% names(d))   d$reviewer_notes   <- NA_character_
    if (!"review_timestamp" %in% names(d)) d$review_timestamp <- NA_character_
    d
  } else {
    tibble(abstract_id = character(), manual_decision = character(),
           manual_pmid = character(), reviewer_notes = character(),
           review_timestamp = character())
  }

  list(
    review_queue  = review_queue,
    candidates    = candidates,
    scores_detail = scores_detail,
    decisions     = decisions
  )
}

# --- UI ---
ui <- page_sidebar(
  title = "AAGL 2023 Abstract-to-Publication Adjudication",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

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
    hr(),
    actionButton("btn_refresh", "Refresh Data", icon = icon("rotate"), class = "btn-info btn-sm w-100")
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
          tags$strong("First Author: "), textOutput("abs_first_author", inline = TRUE)
        ),
        tags$div(class = "mb-2",
          tags$strong("Total Score: "), uiOutput("score_badge", inline = TRUE)
        ),
        tags$div(class = "mb-2",
          uiOutput("pubmed_link")
        ),
        hr(),
        h6("Objective"),
        textOutput("abs_objective"),
        h6("Conclusion"),
        textOutput("abs_conclusion"),
        hr(),
        h6("Score Breakdown (Best Candidate)"),
        plotOutput("score_chart", height = "260px")
      )
    ),

    # --- Right card: Candidates table + decision ---
    card(
      card_header("Candidate Publications"),
      card_body(
        DTOutput("candidates_table"),
        hr(),
        h5("Decision"),
        radioButtons("decision", "Match decision:",
                     choices = c("Confirmed match" = "match",
                                 "No match found" = "no_match",
                                 "Skip / Unsure" = "skip"),
                     selected = "skip", inline = TRUE),
        textInput("manual_pmid", "Override PMID (if different from best):"),
        textAreaInput("notes", "Reviewer notes:", rows = 2),
        actionButton("btn_save", "Save Decision", class = "btn-primary", icon = icon("floppy-disk"))
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  data <- reactiveVal(load_data())

  # --- Visible IDs (respects filter) ---
  visible_ids <- reactive({
    d <- data()
    if (nrow(d$review_queue) == 0) return(character(0))
    ids <- d$review_queue$abstract_id
    if (isTRUE(input$filter_unreviewed)) {
      reviewed <- d$decisions$abstract_id
      ids <- ids[!ids %in% reviewed]
    }
    ids
  })

  # --- Build selector labels with checkmarks ---
  make_choices <- function(ids, d) {
    reviewed <- d$decisions$abstract_id
    labels <- vapply(ids, function(id) {
      row <- d$review_queue[d$review_queue$abstract_id == id, ]
      prefix <- if (id %in% reviewed) "\u2713 " else "\u2022 "
      paste0(prefix, id, ": ", str_trunc(row$title[1], 45))
    }, character(1))
    setNames(ids, labels)
  }

  # Populate abstract selector
  observe({
    d <- data()
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
  observeEvent(input$btn_next, {
    ids <- visible_ids()
    current_idx <- match(input$abstract_select, ids)
    if (!is.na(current_idx) && current_idx < length(ids)) {
      updateSelectInput(session, "abstract_select", selected = ids[current_idx + 1])
    }
  })

  observeEvent(input$btn_prev, {
    ids <- visible_ids()
    current_idx <- match(input$abstract_select, ids)
    if (!is.na(current_idx) && current_idx > 1) {
      updateSelectInput(session, "abstract_select", selected = ids[current_idx - 1])
    }
  })

  # --- Current abstract reactive ---
  current_abstract <- reactive({
    req(input$abstract_select, input$abstract_select != "")
    d <- data()
    d$review_queue |> filter(abstract_id == input$abstract_select)
  })

  # --- Best candidate scores (from nested tibble) ---
  best_candidate_scores <- reactive({
    d <- data()
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")
    row <- d$scores_detail |> filter(abstract_id == abs_id)
    if (nrow(row) == 0 || is.null(row$score_details[[1]])) return(NULL)
    details <- row$score_details[[1]]
    if (nrow(details) == 0) return(NULL)
    # Pick best candidate by total_score
    details |> arrange(desc(total_score)) |> slice(1)
  })

  # --- Restore decisions when abstract changes ---
  observeEvent(input$abstract_select, {
    d <- data()
    abs_id <- input$abstract_select
    if (is.null(abs_id) || abs_id == "") return()

    prev <- d$decisions |> filter(abstract_id == abs_id)
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

  output$abs_first_author <- renderText({
    a <- current_abstract()
    if (nrow(a) > 0) a$first_author_normalized[1] else ""
  })

  output$abs_objective <- renderText({
    a <- current_abstract()
    if (nrow(a) > 0 && "abstract_objective" %in% names(a)) a$abstract_objective[1] else ""
  })

  output$abs_conclusion <- renderText({
    a <- current_abstract()
    if (nrow(a) > 0 && "abstract_conclusion" %in% names(a)) a$abstract_conclusion[1] else ""
  })

  # --- Status badge ---
  output$status_badge <- renderUI({
    d <- data()
    abs_id <- input$abstract_select
    if (is.null(abs_id) || abs_id == "") return(span(class = "badge bg-secondary", "Pending"))
    prev <- d$decisions |> filter(abstract_id == abs_id)
    if (nrow(prev) == 0) return(span(class = "badge bg-secondary", "Pending"))
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

  # --- Score breakdown chart ---
  output$score_chart <- renderPlot({
    best <- best_candidate_scores()
    if (is.null(best)) {
      plot.new()
      text(0.5, 0.5, "No score details available", cex = 1.2, col = "grey50")
      return()
    }

    components <- c(
      "Title"      = best$title_pts[1],
      "Abstract"   = best$abstract_pts[1],
      "1st Author" = best$first_au_pts[1],
      "Last Author" = best$last_au_pts[1],
      "Coauthors"  = best$coauthor_pts[1],
      "Team Bonus" = best$team_bonus[1],
      "Journal"    = best$journal_pts[1],
      "Keywords"   = best$keyword_pts[1],
      "Date"       = best$date_pts[1],
      "No-text Penalty" = -abs(best$no_text_penalty[1])
    )

    cols <- ifelse(components >= 0, "#2196F3", "#F44336")
    par(mar = c(3, 8, 2, 1), las = 1)
    bp <- barplot(rev(components), horiz = TRUE, col = rev(cols),
                  border = NA, xlab = "Points",
                  names.arg = rev(names(components)), cex.names = 0.85,
                  xlim = c(min(0, min(components) - 0.3), max(components) + 0.5))
    text(rev(components) + ifelse(rev(components) >= 0, 0.15, -0.15),
         bp, labels = rev(sprintf("%.1f", components)),
         cex = 0.8, col = "grey30")
  }, bg = "transparent")

  # --- Candidates table with per-candidate scores ---
  output$candidates_table <- renderDT({
    d <- data()
    abs_id <- input$abstract_select
    if (is.null(abs_id) || abs_id == "") return(datatable(tibble()))

    cands <- d$candidates |>
      filter(abstract_id == abs_id) |>
      select(any_of(c("pmid", "pub_title", "pub_journal", "pub_year", "pub_doi")))

    if (nrow(cands) == 0) return(datatable(tibble()))

    # Join score details
    score_row <- d$scores_detail |> filter(abstract_id == abs_id)
    if (nrow(score_row) > 0 && !is.null(score_row$score_details[[1]])) {
      sd <- score_row$score_details[[1]] |>
        select(pmid, total_score, title_pts, abstract_pts,
               first_au_pts, last_au_pts, journal_pts, date_pts, no_text_penalty)
      cands <- cands |>
        left_join(sd, by = "pmid") |>
        arrange(desc(total_score))
    }

    # Build PubMed/DOI link
    cands <- cands |>
      mutate(
        Link = if_else(!is.na(pub_doi),
                       paste0('<a href="https://doi.org/', pub_doi, '" target="_blank">DOI</a>'),
                       paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/', pmid, '/" target="_blank">PM</a>'))
      ) |>
      select(-any_of("pub_doi"))

    # Rename for display
    display <- cands |>
      rename(any_of(c(
        PMID = "pmid", Title = "pub_title", Journal = "pub_journal",
        Year = "pub_year", Score = "total_score",
        `Title Pts` = "title_pts", `Abstr` = "abstract_pts",
        `1st Au` = "first_au_pts", `Last Au` = "last_au_pts",
        `Jrnl Pts` = "journal_pts", `Date` = "date_pts",
        Penalty = "no_text_penalty"
      )))

    dt <- datatable(display, escape = FALSE, selection = "single",
              options = list(pageLength = 10, scrollX = TRUE,
                             order = list(list(which(names(display) == "Score") - 1, "desc")),
                             columnDefs = list(
                               list(targets = which(names(display) == "Title") - 1, width = "200px")
                             )))

    # Color-code Score column
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
    total <- nrow(d$review_queue)
    reviewed <- sum(d$review_queue$abstract_id %in% d$decisions$abstract_id)
    sprintf("%d / %d (%.0f%%)", reviewed, total,
            if (total > 0) reviewed / total * 100 else 0)
  })

  # --- Save decision ---
  observeEvent(input$btn_save, {
    d <- data()
    abs_id <- input$abstract_select
    req(abs_id, abs_id != "")

    new_decision <- tibble(
      abstract_id      = abs_id,
      manual_decision  = input$decision,
      manual_pmid      = if (nchar(input$manual_pmid) > 0) input$manual_pmid else
        d$review_queue$best_pmid[d$review_queue$abstract_id == abs_id],
      reviewer_notes   = input$notes,
      review_timestamp = as.character(Sys.time())
    )

    updated_decisions <- d$decisions |>
      filter(abstract_id != abs_id) |>
      bind_rows(new_decision)

    write_csv(updated_decisions, here("output", "manual_review_decisions.csv"))

    d$decisions <- updated_decisions
    data(d)

    showNotification(paste("Saved:", abs_id, "->", input$decision), type = "message")

    # Auto-advance within visible list
    ids <- visible_ids()
    current_idx <- match(abs_id, ids)
    if (!is.na(current_idx) && current_idx < length(ids)) {
      updateSelectInput(session, "abstract_select", selected = ids[current_idx + 1])
    }
  })

  # --- Refresh ---
  observeEvent(input$btn_refresh, {
    data(load_data())
    showNotification("Data refreshed", type = "message")
  })
}

shinyApp(ui = ui, server = server)
