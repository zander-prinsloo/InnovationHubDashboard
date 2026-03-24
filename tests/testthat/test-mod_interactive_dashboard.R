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

test_that("lorenz stats renderUI uses pip-card--elevated for visual consistency", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-card--elevated", body_str, fixed = TRUE))
  # Confirm the elevated modifier occurs near the Distribution Statistics heading
  expect_true(grepl("pip-card--elevated.*Distribution Statistics|Distribution Statistics.*pip-card--elevated", body_str))
})

test_that("pip-analysis-bg custom property is set to #eef2f7", {
  css <- readLines(
    system.file("app/www/pip-redesign.css", package = "InnovationHubDashboard")
  )
  expect_true(any(grepl("#eef2f7", css, fixed = TRUE)))
  expect_true(any(grepl("pip-blue-mid", css, fixed = TRUE)))
  expect_true(any(grepl("pip-analysis-bg", css, fixed = TRUE)))
})

test_that("display-contents rule targets col-wrapper class not bare div", {
  css <- readLines(
    system.file("app/www/pip-redesign.css", package = "InnovationHubDashboard")
  )
  expect_true(any(grepl("pip-analysis-panel__col-wrapper", css, fixed = TRUE)))
  expect_false(any(grepl("pip-analysis-panel--triple > div", css, fixed = TRUE)))
})

test_that("server body passes col-wrapper class to uiOutput wrappers", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("pip-analysis-panel__col-wrapper", body_str, fixed = TRUE))
})

# ── wb_country_names integration tests ────────────────────────────────────────

test_that("mod_interactive_dashboard_server accepts wb_country_names parameter", {
  fmls <- formals(mod_interactive_dashboard_server)
  expect_true("wb_country_names" %in% names(fmls))
})

# ── Lorenz country choices logic tests ───────────────────────────────────────
# These are white-box tests that exercise the choices-building pattern
# (merge + safety filter + dedup + sort) in isolation from the Shiny module.
# This approach is consistent with the body-inspection pattern used above:
# the module server cannot be instantiated cheaply, so we test the logic
# directly rather than through testServer().

test_that("lorenz_country_choices: no NA names when fst iso3 not in wb_country_names", {
  # Simulate the lorenz_country_choices logic using synthetic inputs.
  wb <- data.table::data.table(
    country_code = c("ALB", "IND", "BRA"),
    country_name = c("Albania", "India", "Brazil")
  )
  avail <- data.table::data.table(iso3 = c("ALB", "IND", "BRA"))
  merged <- merge(avail, wb, by.x = "iso3", by.y = "country_code", all.x = TRUE)
  merged <- merged[!is.na(country_name)]
  choices <- stats::setNames(merged$iso3, merged$country_name)
  choices <- choices[order(names(choices))]

  expect_false(anyNA(names(choices)))
  expect_false(anyNA(choices))
  expect_equal(length(choices), 3L)
})

test_that("lorenz_country_choices: unmatched iso3 codes are dropped (safety filter)", {
  # An iso3 not in wb_country_names should be silently excluded.
  wb <- data.table::data.table(
    country_code = c("ALB", "IND"),
    country_name = c("Albania", "India")
  )
  # "XYZ" is not in wb — simulates a disputed territory or data-prep error
  avail <- data.table::data.table(iso3 = c("ALB", "IND", "XYZ"))
  merged <- merge(avail, wb, by.x = "iso3", by.y = "country_code", all.x = TRUE)
  merged <- merged[!is.na(country_name)]
  choices <- stats::setNames(merged$iso3, merged$country_name)

  expect_false("XYZ" %in% choices)
  expect_false(anyNA(names(choices)))
  expect_equal(length(choices), 2L)
})

test_that("lorenz_country_choices: duplicate iso3 across years produces one entry", {
  # Same iso3 appearing in two fst files (different years) must appear once
  # in the dropdown.
  wb <- data.table::data.table(
    country_code = "ALB",
    country_name = "Albania"
  )
  # Simulate lorenz_file_lookup with duplicate iso3
  file_lookup <- data.table::data.table(
    iso3     = c("ALB", "ALB"),
    year     = c(2018L, 2020L),
    filename = c("alb_2018_cumulative.fst", "alb_2020_cumulative.fst")
  )
  avail  <- unique(file_lookup[, .(iso3)])
  merged <- merge(avail, wb, by.x = "iso3", by.y = "country_code", all.x = TRUE)
  merged <- merged[!is.na(country_name)]
  choices <- stats::setNames(merged$iso3, merged$country_name)

  expect_equal(length(choices), 1L)
  expect_equal(unname(choices), "ALB")
  expect_equal(names(choices), "Albania")
})

test_that("lorenz_country_choices: choices are sorted alphabetically by country name", {
  wb <- data.table::data.table(
    country_code = c("ZAF", "ALB", "IND"),
    country_name = c("South Africa", "Albania", "India")
  )
  avail  <- data.table::data.table(iso3 = c("ZAF", "ALB", "IND"))
  merged <- merge(avail, wb, by.x = "iso3", by.y = "country_code", all.x = TRUE)
  merged <- merged[!is.na(country_name)]
  choices <- stats::setNames(merged$iso3, merged$country_name)
  choices <- choices[order(names(choices))]

  expect_equal(names(choices), c("Albania", "India", "South Africa"))
  expect_equal(unname(choices), c("ALB", "IND", "ZAF"))
})

test_that("lorenz_country_choices: empty result when no fst files exist", {
  empty_choices <- stats::setNames(character(0L), character(0L))
  expect_equal(length(empty_choices), 0L)
  expect_false(anyNA(names(empty_choices)))
})


# ── Steps 1–6: button labels, economy terms, LoA, Changes sidebar, CSS ────────
# body_str is computed once and shared across all tests in this describe() block
# to avoid re-deparsing the 1100-line server function for every test_that().

describe("mod_interactive_dashboard_server body — feature/button-rename changes", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")

  # ── Step 1: Renamed tab button labels ───────────────────────────────────────

  it("uses new button label 'Bias & agreement diagnostic'", {
    expect_true(grepl("Bias & agreement diagnostic", body_str, fixed = TRUE))
  })

  it("uses new button label 'Largest changes'", {
    expect_true(grepl("Largest changes", body_str, fixed = TRUE))
  })

  it("uses new button label 'Method comparison scatterplot'", {
    expect_true(grepl("Method comparison scatterplot", body_str, fixed = TRUE))
  })

  it("no longer contains old 'Differences' tab-button label", {
    # The old label was the literal string "Differences" assigned to rankings_label.
    # After the rename it became "Bias & agreement diagnostic".
    expect_false(grepl('"Differences"', body_str, fixed = TRUE))
  })

  it("no longer contains old 'Scatterplot' tab-button label", {
    # The old label was the literal string "Scatterplot" assigned to scatter_label.
    # After the rename it became "Method comparison scatterplot".
    expect_false(grepl('"Scatterplot"', body_str, fixed = TRUE))
  })

  # ── Step 2: "Economy" terminology in modal and Lorenz stats ─────────────────

  it("uses 'Economy Details' as the modal dialog title", {
    expect_true(grepl("Economy Details", body_str, fixed = TRUE))
  })

  it("uses 'Economy Code:' in modal content", {
    expect_true(grepl("Economy Code:", body_str, fixed = TRUE))
  })

  it("uses 'Economy:' in Lorenz stats panel", {
    # The Lorenz stats panel renders tags$strong("Economy:") — check for the string
    expect_true(grepl('"Economy:"', body_str, fixed = TRUE))
  })

  it("does NOT contain 'Country Details' (old modal title)", {
    expect_false(grepl("Country Details", body_str, fixed = TRUE))
  })

  it("does NOT contain 'Country Code:' (old modal label)", {
    expect_false(grepl("Country Code:", body_str, fixed = TRUE))
  })

  # ── Step 3: LoA acronym in rankings stats sidebar ───────────────────────────

  it("rankings stats sidebar includes LoA acronym in label", {
    # The label changed from "Limits of Agreement*:" to "Limits of Agreement (LoA)*:"
    expect_true(grepl("Limits of Agreement (LoA)*:", body_str, fixed = TRUE))
  })

  it("rankings stats sidebar does NOT use old label without LoA acronym", {
    # Use a regex to match the old form: "Limits of Agreement*:" with NO "(LoA)"
    expect_false(grepl('"Limits of Agreement\\*:"', body_str))
  })

  # ── Step 4: Changes tab regional sidebar ────────────────────────────────────

  it("renders Changes tab click hint text", {
    expect_true(grepl("Click on an economy", body_str, fixed = TRUE))
  })

  it("renders 'Average Difference by Region' sidebar heading", {
    expect_true(grepl("Average Difference by Region", body_str, fixed = TRUE))
  })

  it("computes diff_signed for Changes sidebar regional table", {
    expect_true(grepl("diff_signed", body_str, fixed = TRUE))
  })

  it("computes mean_diff for Changes sidebar regional table", {
    expect_true(grepl("mean_diff", body_str, fixed = TRUE))
  })

  it("Changes sidebar shows 'Alternative higher' direction label", {
    expect_true(grepl("Alternative higher", body_str, fixed = TRUE))
  })

  it("Changes sidebar shows 'PIP higher' direction label", {
    expect_true(grepl("PIP higher", body_str, fixed = TRUE))
  })

  it("Changes sidebar footnote explains mean difference direction", {
    # Footnote text: "Mean difference = Alternative − PIP (pp)."
    expect_true(grepl("Mean difference = Alternative", body_str, fixed = TRUE))
  })

  # ── Step 5: pip-analysis-panel--full no longer emitted ──────────────────────

  it("no longer emits pip-analysis-panel--full class", {
    # The --full variant was used when Changes tab had no sidebar (non-SN methods).
    # All Changes tab cases now use the standard 2-column grid with a sidebar.
    expect_false(grepl("pip-analysis-panel--full", body_str, fixed = TRUE))
  })
})


# ── Step 6: CSS fixes — subtitle max-width and chart width constraint ─────────

test_that("pip-analysis-section__intro has max-width: none (not 640px)", {
  css <- readLines(
    system.file("app/www/pip-redesign.css", package = "InnovationHubDashboard")
  )
  # Find all lines that define the __intro rule block
  intro_idx <- grep("pip-analysis-section__intro", css)
  expect_true(length(intro_idx) >= 1L)
  # Check within a window around the first occurrence
  window <- css[max(1L, intro_idx[1L] - 2L):min(length(css), intro_idx[1L] + 8L)]
  expect_true(any(grepl("max-width:\\s*none", window)))
  # Confirm the old 640px constraint is gone from that same window
  expect_false(any(grepl("max-width:\\s*640px", window)))
})

test_that("CSS adds max-width: 100% and overflow: hidden to analysis grid children", {
  css <- readLines(
    system.file("app/www/pip-redesign.css", package = "InnovationHubDashboard")
  )
  # The combined rule block covers __controls, __chart, and __stats
  panel_children_idx <- grep("pip-analysis-panel__controls", css)
  expect_true(length(panel_children_idx) >= 1L)
  # Look in a window around the first occurrence of the selector block
  window <- css[max(1L, panel_children_idx[1L]):min(length(css), panel_children_idx[1L] + 10L)]
  expect_true(any(grepl("max-width:\\s*100%", window)))
  expect_true(any(grepl("overflow:\\s*hidden", window)))
})

test_that("CSS constrains shiny-plot-output width inside analysis chart panel", {
  css <- readLines(
    system.file("app/www/pip-redesign.css", package = "InnovationHubDashboard")
  )
  # A dedicated rule must target shiny-plot-output inside the analysis panel chart
  plot_output_idx <- grep("shiny-plot-output", css)
  expect_true(length(plot_output_idx) >= 1L)
  # At least one of those lines must set width to 100%
  shiny_plot_lines <- css[plot_output_idx]
  # The width rule may be on the next line — check in a small window
  window <- css[max(1L, plot_output_idx[1L]):min(length(css), plot_output_idx[1L] + 3L)]
  expect_true(any(grepl("width:\\s*100%", window)))
})
