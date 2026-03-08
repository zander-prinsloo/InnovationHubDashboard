testServer(
  mod_interactive_dashboard_server,
  # Add here your module params
  args = list()
  , {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
})
 
test_that("module ui works", {
  ui <- mod_interactive_dashboard_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_interactive_dashboard_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

# ── MTG method integration tests ─────────────────────────────────────────────

test_that("mod_interactive_dashboard_server accepts data_yk and yk_metadata args", {
  fmls <- formals(mod_interactive_dashboard_server)
  expect_true("data_yk"     %in% names(fmls))
  expect_true("yk_metadata" %in% names(fmls))
})

test_that("UI contains 'NA-Survey gap adjustment' in method choices", {
  ui <- mod_interactive_dashboard_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("gap adjustment", ui_str, fixed = FALSE))
})

test_that("UI contains mtg_controls uiOutput placeholder", {
  ui <- mod_interactive_dashboard_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("mtg_controls", ui_str))
})

test_that("mod_interactive_dashboard_server has mtg_gini_latest and mtg_gini_na_type in formals or body", {
  # Confirm the server function references both new reactive inputs in its body
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("mtg_gini_latest",  body_str, fixed = TRUE))
  expect_true(grepl("mtg_gini_na_type", body_str, fixed = TRUE))
})
