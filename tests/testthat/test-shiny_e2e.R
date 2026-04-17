# tests/testthat/test-shiny_e2e.R
# End-to-end browser tests using shinytest2.
# These tests drive a real headless Chromium session against the full app UI.

library(testthat)
library(shinytest2)
library(here)

APP_PATH <- here("shiny", "adjudication_app")

# Helper: create an AppDriver and wait for data to load
make_driver <- function() {
  app <- AppDriver$new(
    app_dir      = APP_PATH,
    name         = "adjudication_app",
    height       = 900,
    width        = 1280,
    load_timeout = 40000,
    timeout      = 20000
  )
  app$wait_for_idle(timeout = 15000)
  app
}

# Helper: navigate to the first available abstract
select_first_abstract <- function(app) {
  first_id <- app$get_value(input = "abstract_select")
  if (is.null(first_id) || nchar(first_id) == 0) {
    # Fall back: choose first option from JS
    first_id <- app$get_js(
      "Object.keys(Shiny.shinyapp.$inputValues).includes('abstract_select')
         ? Shiny.shinyapp.$inputValues.abstract_select : ''"
    )
  }
  if (!is.null(first_id) && nchar(first_id) > 0) {
    app$set_inputs(abstract_select = first_id, wait_ = TRUE, timeout_ = 10000)
  }
  app$wait_for_idle(timeout = 10000)
  first_id
}

# ── 1. App loads ──────────────────────────────────────────────────────────────

test_that("app loads and page title is correct", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  title <- app$get_text("h1, .navbar-brand, title")
  expect_match(title, "Abstract|Adjudication", ignore.case = TRUE)
})

# ── 2. Abstract selector is populated ────────────────────────────────────────

test_that("abstract selector has a value after load", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  current_val <- app$get_value(input = "abstract_select")
  expect_false(is.null(current_val),
               info = "abstract_select should have a selected value")
  expect_gt(nchar(current_val), 0L,
            label = "abstract_select value should be non-empty")
})

# ── 3. Abstract details render ────────────────────────────────────────────────

test_that("selected abstract shows title text in left panel", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  abs_title <- app$get_text("#abs_title")
  expect_gt(nchar(trimws(abs_title)), 5L,
            label = "abstract title should be non-trivial text")
})

# ── 4. Candidate table renders ────────────────────────────────────────────────

test_that("candidate table has data rows", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  app$wait_for_js(
    "$('#candidates_table table').length > 0",
    timeout = 10000
  )
  n_rows <- app$get_js(
    "$('#candidates_table table tbody tr').not('.dataTables_empty').length"
  )
  expect_gt(n_rows, 0L, label = "candidate table must have at least 1 data row")
})

# ── 5. Auto-select top candidate ──────────────────────────────────────────────

test_that("top candidate row is auto-selected after abstract changes", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  Sys.sleep(0.6)   # allow 200ms auto-select delay + render
  app$wait_for_idle(timeout = 8000)

  n_selected <- app$get_js(
    "$('#candidates_table table tbody tr.selected').length"
  )
  expect_equal(n_selected, 1L,
               label = "exactly one candidate row should be auto-selected")
})

# ── 6. Score chip summary bar ─────────────────────────────────────────────────

test_that("score chip summary bar renders with PMID and Score", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  Sys.sleep(0.6)
  app$wait_for_idle(timeout = 8000)

  app$wait_for_js("$('.cand-summary-bar').length > 0", timeout = 8000)
  bar_text <- app$get_text(".cand-summary-bar")
  expect_match(bar_text, "PMID",  ignore.case = FALSE)
  expect_match(bar_text, "Score", ignore.case = FALSE)
})

# ── 7. Score chips have tooltip attributes ────────────────────────────────────

test_that("score chips carry data-bs-toggle tooltip attributes", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  Sys.sleep(0.6)
  app$wait_for_js("$('.score-chip').length > 0", timeout = 8000)

  n_with_tooltip <- app$get_js(
    "$('.score-chip[data-bs-toggle=\"tooltip\"]').length"
  )
  expect_gt(n_with_tooltip, 0L,
            label = "score chips should have Bootstrap tooltip attribute")
})

# ── 8. Original abstract text in comparison pane ──────────────────────────────

test_that("comparison pane shows real abstract text (not placeholder)", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  # Explicitly choose a 2022 abstract (guaranteed to have text)
  app$set_inputs(filter_year = "2022", wait_ = TRUE, timeout_ = 8000)
  app$wait_for_idle(timeout = 8000)

  first_2022 <- app$get_value(input = "abstract_select")
  skip_if(is.null(first_2022) || nchar(first_2022) == 0,
          "No 2022 abstract available")
  Sys.sleep(0.6)
  app$wait_for_idle(timeout = 8000)

  app$wait_for_js("$('.comparison-pane.abstract-source').length > 0",
                  timeout = 10000)
  src_text <- app$get_text(".comparison-pane.abstract-source")

  expect_false(
    grepl("No abstract text available", src_text, ignore.case = TRUE),
    info = paste("Expected real abstract text, got:", substr(src_text, 1, 120))
  )
  expect_gt(nchar(trimws(src_text)), 30L,
            label = "abstract-source pane should have substantial text")
})

# ── 9. Year filter reduces selector options ───────────────────────────────────

test_that("year filter 2022 restricts selector to 2022 abstracts", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  all_ids_js  <- "Object.keys(window.Shiny ? Shiny.shinyapp.$inputValues : {}).length"
  id_all  <- app$get_value(input = "abstract_select")

  app$set_inputs(filter_year = "2022", wait_ = TRUE, timeout_ = 8000)
  app$wait_for_idle(timeout = 8000)
  id_2022 <- app$get_value(input = "abstract_select")

  # The selected ID should contain '2022' in its label, or we can check
  # that the progress counter changed
  progress_txt <- app$get_text("#progress_count")
  total_match  <- regmatches(progress_txt, regexpr("/ \\d+", progress_txt))
  total_2022   <- as.integer(gsub("[^0-9]", "", total_match))

  # Reset and get all-years total
  app$set_inputs(filter_year = "all", wait_ = TRUE, timeout_ = 8000)
  app$wait_for_idle(timeout = 5000)
  progress_all <- app$get_text("#progress_count")
  total_match_all <- regmatches(progress_all, regexpr("/ \\d+", progress_all))
  total_all <- as.integer(gsub("[^0-9]", "", total_match_all))

  expect_lt(total_2022, total_all,
            label = "2022 filter should show fewer abstracts than 'All'")
  expect_gt(total_2022, 0L,
            label = "2022 filter should still show some abstracts")
})

# ── 10. Decision validation: missing initials ─────────────────────────────────

test_that("saving without reviewer initials triggers error notification", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  app$set_inputs(reviewer_initials = "", decision = "skip", wait_ = TRUE)
  app$click("btn_save")
  app$wait_for_js("$('.shiny-notification-error').length > 0", timeout = 8000)

  notif <- app$get_text(".shiny-notification-error")
  expect_match(notif, "initials", ignore.case = TRUE)
})

# ── 11. Keyboard shortcuts ────────────────────────────────────────────────────

test_that("keyboard shortcut 'm' (via kb_decision) selects Confirmed match", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  # Simulate keyboard handler: it calls Shiny.setInputValue('kb_decision', 'm')
  app$run_js("Shiny.setInputValue('kb_decision', 'match', {priority: 'event'})")
  app$wait_for_idle(timeout = 5000)

  val <- app$get_value(input = "decision")
  expect_equal(val, "match", info = "'m' shortcut should set decision = match")
})

test_that("keyboard shortcut 'n' (via kb_decision) selects No match found", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  app$run_js("Shiny.setInputValue('kb_decision', 'no_match', {priority: 'event'})")
  app$wait_for_idle(timeout = 5000)

  val <- app$get_value(input = "decision")
  expect_equal(val, "no_match", info = "'n' shortcut should set decision = no_match")
})

test_that("keyboard shortcut 's' (via kb_decision) selects Skip / Unsure", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  app$set_inputs(decision = "match", wait_ = TRUE)
  app$run_js("Shiny.setInputValue('kb_decision', 'skip', {priority: 'event'})")
  app$wait_for_idle(timeout = 5000)

  val <- app$get_value(input = "decision")
  expect_equal(val, "skip", info = "'s' shortcut should set decision = skip")
})

# ── 12. Navigation buttons ────────────────────────────────────────────────────

test_that("Next button advances to a different abstract", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  current_id <- select_first_abstract(app)
  app$click("btn_next")
  app$wait_for_idle(timeout = 8000)

  new_id <- app$get_value(input = "abstract_select")
  expect_false(identical(new_id, current_id),
               info = "Next button should change the selected abstract")
})

test_that("Prev button at first abstract stays on same abstract", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  current_id <- select_first_abstract(app)
  app$click("btn_prev")
  app$wait_for_idle(timeout = 5000)

  still_id <- app$get_value(input = "abstract_select")
  expect_equal(still_id, current_id,
               info = "Prev at first abstract should not move")
})

# ── 13. Progress counter ──────────────────────────────────────────────────────

test_that("progress counter shows N / M (P%) format", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js("$('#progress_count').text().length > 0", timeout = 10000)
  txt <- app$get_text("#progress_count")
  expect_match(txt, "\\d+ / \\d+",
               info = "progress_count should be 'N / M (P%)'")
})

# ── 14. Use selected PMID button ──────────────────────────────────────────────

test_that("Use selected PMID button copies PMID and sets match decision", {
  app <- make_driver()
  on.exit(app$stop(), add = TRUE)

  select_first_abstract(app)
  Sys.sleep(0.6)
  app$wait_for_idle(timeout = 8000)

  # Must have a candidate selected (auto-select does this)
  n_sel <- app$get_js(
    "$('#candidates_table table tbody tr.selected').length"
  )
  skip_if(n_sel == 0, "No candidate row selected — cannot test PMID copy")

  app$click("btn_use_selected_pmid")
  app$wait_for_idle(timeout = 5000)

  pmid_val <- app$get_value(input = "manual_pmid")
  decision  <- app$get_value(input = "decision")

  expect_match(pmid_val, "^\\d{7,8}$",
               info = "manual_pmid should be a 7-8 digit PMID after clicking Use Selected PMID")
  expect_equal(decision, "match",
               info = "decision should switch to 'match' after Use Selected PMID")
})
