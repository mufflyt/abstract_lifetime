# Shiny Adjudication App — Manual review of abstract-to-publication matches
# Left panel: abstract details; Right panel: candidate papers with scores

library(shiny)
library(DT)
library(readr)
library(dplyr)
library(stringr)
library(here)

# --- Data loading ---
load_data <- function() {
  review_path <- here("output", "manual_review_queue.csv")
  candidates_path <- here("data", "processed", "pubmed_candidates.csv")
  scores_path <- here("data", "processed", "match_scores_detailed.rds")
  decisions_path <- here("output", "manual_review_decisions.csv")

  review_queue <- if (file.exists(review_path)) read_csv(review_path, show_col_types = FALSE) else tibble()
  candidates <- if (file.exists(candidates_path)) read_csv(candidates_path, show_col_types = FALSE) else tibble()
  scores_detail <- if (file.exists(scores_path)) readRDS(scores_path) else tibble()

  # Load existing decisions
  decisions <- if (file.exists(decisions_path)) {
    read_csv(decisions_path, show_col_types = FALSE)
  } else {
    tibble(abstract_id = character(), manual_decision = character(),
           manual_pmid = character(), reviewer_notes = character(),
           review_timestamp = character())
  }

  list(
    review_queue = review_queue,
    candidates = candidates,
    scores_detail = scores_detail,
    decisions = decisions
  )
}

# --- UI ---
ui <- fluidPage(
  titlePanel("AAGL 2023 Abstract-to-Publication Adjudication"),

  fluidRow(
    column(3,
      wellPanel(
        h4("Review Queue"),
        selectInput("abstract_select", "Select Abstract:",
                    choices = NULL, selected = NULL),
        actionButton("btn_prev", "Previous", icon = icon("arrow-left")),
        actionButton("btn_next", "Next", icon = icon("arrow-right")),
        hr(),
        h5("Progress"),
        textOutput("progress_text"),
        hr(),
        actionButton("btn_refresh", "Refresh Data", icon = icon("refresh"),
                      class = "btn-info btn-sm")
      )
    ),

    column(4,
      wellPanel(
        h4("Abstract Details"),
        tags$div(
          tags$strong("Title:"), textOutput("abs_title", inline = TRUE)
        ),
        tags$div(
          tags$strong("First Author:"), textOutput("abs_first_author", inline = TRUE)
        ),
        tags$div(
          tags$strong("Score:"), textOutput("abs_score", inline = TRUE)
        ),
        hr(),
        h5("Objective"),
        textOutput("abs_objective"),
        h5("Conclusion"),
        textOutput("abs_conclusion")
      )
    ),

    column(5,
      wellPanel(
        h4("Candidate Publications"),
        DTOutput("candidates_table"),
        hr(),
        h4("Decision"),
        radioButtons("decision", "Match decision:",
                     choices = c("Confirmed match" = "match",
                                 "No match found" = "no_match",
                                 "Skip / Unsure" = "skip"),
                     selected = "skip"),
        textInput("manual_pmid", "Override PMID (if different from best):"),
        textAreaInput("notes", "Reviewer notes:", rows = 2),
        actionButton("btn_save", "Save Decision",
                     class = "btn-primary", icon = icon("save"))
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  data <- reactiveVal(load_data())

  # Populate abstract selector
  observe({
    d <- data()
    if (nrow(d$review_queue) > 0) {
      choices <- setNames(d$review_queue$abstract_id,
                          paste0(d$review_queue$abstract_id, ": ",
                                 str_trunc(d$review_queue$title, 50)))
      updateSelectInput(session, "abstract_select", choices = choices)
    }
  })

  # Navigation buttons
  observeEvent(input$btn_next, {
    d <- data()
    ids <- d$review_queue$abstract_id
    current_idx <- match(input$abstract_select, ids)
    if (!is.na(current_idx) && current_idx < length(ids)) {
      updateSelectInput(session, "abstract_select", selected = ids[current_idx + 1])
    }
  })

  observeEvent(input$btn_prev, {
    d <- data()
    ids <- d$review_queue$abstract_id
    current_idx <- match(input$abstract_select, ids)
    if (!is.na(current_idx) && current_idx > 1) {
      updateSelectInput(session, "abstract_select", selected = ids[current_idx - 1])
    }
  })

  # Current abstract
  current_abstract <- reactive({
    d <- data()
    d$review_queue |> filter(abstract_id == input$abstract_select)
  })

  # Outputs: abstract details
  output$abs_title <- renderText({
    a <- current_abstract()
    if (nrow(a) > 0) a$title[1] else ""
  })

  output$abs_first_author <- renderText({
    a <- current_abstract()
    if (nrow(a) > 0) a$first_author_normalized[1] else ""
  })

  output$abs_score <- renderText({
    a <- current_abstract()
    if (nrow(a) > 0) sprintf("%.1f (%s)", a$best_score[1],
                              if (a$has_tie[1]) "TIE" else "no tie") else ""
  })

  output$abs_objective <- renderText({
    a <- current_abstract()
    if (nrow(a) > 0 && "abstract_objective" %in% names(a)) a$abstract_objective[1] else ""
  })

  output$abs_conclusion <- renderText({
    a <- current_abstract()
    if (nrow(a) > 0 && "abstract_conclusion" %in% names(a)) a$abstract_conclusion[1] else ""
  })

  # Candidates table
  output$candidates_table <- renderDT({
    d <- data()
    abs_id <- input$abstract_select
    if (is.null(abs_id) || abs_id == "") return(tibble())

    cands <- d$candidates |>
      filter(abstract_id == abs_id) |>
      select(any_of(c("pmid", "pub_title", "pub_journal", "pub_year",
                       "pub_first_author", "pub_doi", "strategies"))) |>
      mutate(
        pub_link = if_else(!is.na(pub_doi),
                           paste0('<a href="https://doi.org/', pub_doi,
                                  '" target="_blank">DOI</a>'),
                           paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/', pmid,
                                  '/" target="_blank">PubMed</a>'))
      )

    datatable(cands, escape = FALSE, selection = "single",
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # Progress
  output$progress_text <- renderText({
    d <- data()
    total <- nrow(d$review_queue)
    reviewed <- sum(d$review_queue$abstract_id %in% d$decisions$abstract_id)
    sprintf("%d / %d reviewed (%.0f%%)", reviewed, total,
            if (total > 0) reviewed / total * 100 else 0)
  })

  # Save decision
  observeEvent(input$btn_save, {
    d <- data()

    new_decision <- tibble(
      abstract_id = input$abstract_select,
      manual_decision = input$decision,
      manual_pmid = if (nchar(input$manual_pmid) > 0) input$manual_pmid else
        d$review_queue$best_pmid[d$review_queue$abstract_id == input$abstract_select],
      reviewer_notes = input$notes,
      review_timestamp = as.character(Sys.time())
    )

    # Update decisions
    updated_decisions <- d$decisions |>
      filter(abstract_id != input$abstract_select) |>
      bind_rows(new_decision)

    write_csv(updated_decisions, here("output", "manual_review_decisions.csv"))

    # Refresh
    d$decisions <- updated_decisions
    data(d)

    showNotification(paste("Saved decision for", input$abstract_select),
                     type = "message")

    # Auto-advance
    ids <- d$review_queue$abstract_id
    current_idx <- match(input$abstract_select, ids)
    if (!is.na(current_idx) && current_idx < length(ids)) {
      updateSelectInput(session, "abstract_select", selected = ids[current_idx + 1])
    }
  })

  # Refresh
  observeEvent(input$btn_refresh, {
    data(load_data())
    showNotification("Data refreshed", type = "message")
  })
}

shinyApp(ui = ui, server = server)
