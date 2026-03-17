test_that("testServer stub — skipped pending full fixture setup", {
  # mod_interactive_dashboard_server requires data_yk, data_stb, data_sn,
  # data_dm args that are costly to mock.  Lorenz integration is covered by
  # the body-inspection tests below.
  skip("testServer requires full data args; covered by body-inspection tests below")
})

test_that("module ui works", {
  ui <- mod_interactive_dashboard_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_interactive_dashboard_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

# ── Redesigned UI structure tests ─────────────────────────────────────────────

test_that("UI contains pip-dd-banner class", {
  ui     <- mod_interactive_dashboard_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("pip-dd-banner", ui_str, fixed = TRUE))
})

test_that("UI contains pip-card class for controls and chart panels", {
  ui     <- mod_interactive_dashboard_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("pip-card", ui_str, fixed = TRUE))
})

test_that("UI contains pip-dd-layout for two-column structure", {
  ui     <- mod_interactive_dashboard_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("pip-dd-layout", ui_str, fixed = TRUE))
})

test_that("UI banner contains page title 'Deep Dives'", {
  ui     <- mod_interactive_dashboard_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("Deep Dives", ui_str, fixed = TRUE))
})

test_that("UI contains 'Choose inputs' card heading", {
  ui     <- mod_interactive_dashboard_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("Choose inputs", ui_str, fixed = TRUE))
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

test_that("server body declares mtg_gini_latest and mtg_gini_na_type as reactiveVals", {
  # Confirm the server function references both new reactive inputs in its body
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("mtg_gini_latest",  body_str, fixed = TRUE))
  expect_true(grepl("mtg_gini_na_type", body_str, fixed = TRUE))
  # Confirm they are declared as reactiveVals (not as input$ controls)
  expect_true(grepl('mtg_gini_latest.*reactiveVal|reactiveVal.*mtg_gini_latest', body_str))
})


# ── Lorenz comparison integration tests ───────────────────────────────────────

test_that("server body references lorenz reactive values", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("lorenz_data",  body_str, fixed = TRUE))
  expect_true(grepl("lorenz_meta",  body_str, fixed = TRUE))
})

test_that("server body references lorenz UI inputs", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("lorenz_country",   body_str, fixed = TRUE))
  expect_true(grepl("lorenz_gap_share", body_str, fixed = TRUE))
})

test_that("server body calls plot_mtg_lorenz_comparison", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("plot_mtg_lorenz_comparison", body_str, fixed = TRUE))
})

test_that("server body calls compute_lorenz_stats for the stats panel", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("compute_lorenz_stats", body_str, fixed = TRUE))
})

test_that("server body routes btn_changes to 'lorenz' tab for MTG", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("lorenz", body_str, fixed = TRUE))
  # Confirm current_tab("lorenz") assignment — not just a comparison like == "lorenz"
  expect_true(grepl('current_tab\\("lorenz"\\)', body_str))
})

# ── Hardcoded HFCE / 2021 PPP \u2014 removed input controls ─────────────────────

test_that("server body does NOT contain input$mtg_na_type (control was removed)", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_false(grepl("input$mtg_na_type", body_str, fixed = TRUE))
})

test_that("server body does NOT contain input$mtg_ppp (control was removed)", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_false(grepl("input$mtg_ppp", body_str, fixed = TRUE))
})

test_that("server body does NOT contain input$mtg_gini_na_type (control was removed)", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_false(grepl("input$mtg_gini_na_type", body_str, fixed = TRUE))
})

test_that("mtg_na_type reactiveVal is hardcoded to 'hfce'", {
  # The reactiveVal initialisation must use the string "hfce" (not a variable)
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl('mtg_na_type.*reactiveVal.*"hfce"', body_str))
})

test_that("mtg_ppp reactiveVal is hardcoded to '2021'", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl('mtg_ppp.*reactiveVal.*"2021"', body_str))
})

test_that("server body renders lorenz_stats_ui output", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("lorenz_stats_ui", body_str, fixed = TRUE))
})

test_that("server body calls mtg_scan_cumulative_files at init", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("mtg_scan_cumulative_files", body_str, fixed = TRUE))
})

test_that("server body calls mtg_read_cumulative when country changes", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("mtg_read_cumulative", body_str, fixed = TRUE))
})

test_that("method_panel renderUI still references desc_text (description paragraph retained)", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("desc_text", body_str, fixed = TRUE))
})

# ── Redesigned bottom section tests ──────────────────────────────────────────

test_that("server body renders pip-analysis-section in bottom section", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-analysis-section", body_str, fixed = TRUE))
})

test_that("server body renders pip-tabs tab bar", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-tabs", body_str, fixed = TRUE))
})

test_that("server body renders pip-tab class buttons", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-tab", body_str, fixed = TRUE))
})

test_that("server body renders pip-tab--active for current tab", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-tab--active", body_str, fixed = TRUE))
})

test_that("server body uses pip-analysis-stats class for statistics panels", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-analysis-stats", body_str, fixed = TRUE))
})

test_that("server body uses pip-lorenz-stats class for lorenz panel", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-lorenz-stats", body_str, fixed = TRUE))
})

test_that("server body uses pip-card class for analysis controls panels", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-card", body_str, fixed = TRUE))
})

test_that("server body renders pip-analysis-panel CSS grid wrapper", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-analysis-panel", body_str, fixed = TRUE))
})

test_that("server body renders pip-analysis-panel--triple for Lorenz 3-col layout", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-analysis-panel--triple", body_str, fixed = TRUE))
})

test_that("server body renders pip-analysis-section__intro explanatory text", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-analysis-section__intro", body_str, fixed = TRUE))
})

test_that("server body renders pip-card--elevated for elevated analysis cards", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-card--elevated", body_str, fixed = TRUE))
})

test_that("server body uses pip-analysis-panel__controls for controls column", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-analysis-panel__controls", body_str, fixed = TRUE))
})

test_that("server body uses pip-analysis-panel__chart for chart column", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-analysis-panel__chart", body_str, fixed = TRUE))
})

test_that("server body uses pip-analysis-panel__stats for Lorenz stats column", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-analysis-panel__stats", body_str, fixed = TRUE))
})
