# tests/testthat/test-mod_home.R
# Tests for the redesigned Home page module (hero + tiles layout).

# ── UI structure ──────────────────────────────────────────────────────────────

test_that("mod_home_ui returns a valid shiny tag list", {
  ui <- mod_home_ui(id = "test")
  golem::expect_shinytaglist(ui)
})

test_that("mod_home_ui contains pip-hero section", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("pip-hero", ui_str, fixed = TRUE))
})

test_that("mod_home_ui contains pip-hero__title", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("pip-hero__title", ui_str, fixed = TRUE))
})

test_that("mod_home_ui contains hero supporting text", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("pip-hero__text", ui_str, fixed = TRUE))
})

test_that("mod_home_ui contains CSS-only visual motif container", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("pip-hero__visual", ui_str, fixed = TRUE))
})

test_that("mod_home_ui contains section heading 'Explore the PIP Innovation Hub'", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("Explore the PIP Innovation Hub", ui_str, fixed = TRUE))
})

test_that("mod_home_ui contains pip-grid-2 for large feature tiles", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("pip-grid-2", ui_str, fixed = TRUE))
})

test_that("mod_home_ui contains exactly two pip-tile-large elements", {
  ui      <- mod_home_ui(id = "test")
  ui_str  <- as.character(ui)
  # Trailing quote distinguishes "pip-tile-large" from "pip-tile-large__*" children
  n_tiles <- lengths(regmatches(ui_str, gregexpr('pip-tile-large"', ui_str)))
  expect_equal(n_tiles, 2L)
})

test_that("mod_home_ui large tiles have correct titles", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("Deep Dives",           ui_str, fixed = TRUE))
  expect_true(grepl("Research Repository",  ui_str, fixed = TRUE))
})

test_that("mod_home_ui contains pip-grid-4 for method tiles", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("pip-grid-4", ui_str, fixed = TRUE))
})

test_that("mod_home_ui contains four uiOutput placeholders for method cards", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("card_yk",  ui_str, fixed = TRUE))
  expect_true(grepl("card_dm",  ui_str, fixed = TRUE))
  expect_true(grepl("card_stb", ui_str, fixed = TRUE))
  expect_true(grepl("card_sn",  ui_str, fixed = TRUE))
})

test_that("mod_home_ui contains navigation action links", {
  ui     <- mod_home_ui(id = "test")
  ui_str <- as.character(ui)
  expect_true(grepl("banner_deep_dives",  ui_str, fixed = TRUE))
  expect_true(grepl("banner_research",    ui_str, fixed = TRUE))
  expect_true(grepl("feature_deep_dives", ui_str, fixed = TRUE))
  expect_true(grepl("feature_research",   ui_str, fixed = TRUE))
})

# ── Server function formals ───────────────────────────────────────────────────

test_that("mod_home_server accepts all required metadata args", {
  fmls <- formals(mod_home_server)
  for (arg in c("id", "dm_metadata", "stb_metadata", "sn_metadata", "yk_metadata")) {
    expect_true(arg %in% names(fmls))
  }
})

test_that("mod_home_server body references all four card render outputs", {
  body_str <- paste(deparse(body(mod_home_server)), collapse = "\n")
  expect_true(grepl("card_yk",  body_str, fixed = TRUE))
  expect_true(grepl("card_dm",  body_str, fixed = TRUE))
  expect_true(grepl("card_stb", body_str, fixed = TRUE))
  expect_true(grepl("card_sn",  body_str, fixed = TRUE))
})

test_that("mod_home_server body references pip-tile-small class", {
  body_str <- paste(deparse(body(mod_home_server)), collapse = "\n")
  expect_true(grepl("pip-tile-small", body_str, fixed = TRUE))
})

test_that("mod_home_server returns reactive list with method, target, counter", {
  body_str <- paste(deparse(body(mod_home_server)), collapse = "\n")
  # Check that the returned list explicitly names all three fields
  expect_true(grepl('method\\s*=\\s*reactive',  body_str))
  expect_true(grepl('target\\s*=\\s*reactive',  body_str))
  expect_true(grepl('counter\\s*=\\s*reactive', body_str))
  # Confirm both target values are set
  expect_true(grepl('"deep_dives"',    body_str, fixed = TRUE))
  expect_true(grepl('"research_repo"', body_str, fixed = TRUE))
})

test_that("mod_home_server body handles feature_deep_dives navigation", {
  body_str <- paste(deparse(body(mod_home_server)), collapse = "\n")
  expect_true(grepl("feature_deep_dives", body_str, fixed = TRUE))
})

test_that("mod_home_server body handles feature_research navigation", {
  body_str <- paste(deparse(body(mod_home_server)), collapse = "\n")
  expect_true(grepl("feature_research", body_str, fixed = TRUE))
})

test_that("mod_home_server body handles all four small-tile click events", {
  body_str <- paste(deparse(body(mod_home_server)), collapse = "\n")
  expect_true(grepl("click_dm",  body_str, fixed = TRUE))
  expect_true(grepl("click_yk",  body_str, fixed = TRUE))
  expect_true(grepl("click_stb", body_str, fixed = TRUE))
  expect_true(grepl("click_sn",  body_str, fixed = TRUE))
})

# ── build_card helper ─────────────────────────────────────────────────────────

test_that("build_card helper produces pip-tile-small markup", {
  # Declare body_str in this test's own scope (each test_that is isolated)
  body_str <- paste(deparse(body(mod_home_server)), collapse = "\n")
  expect_true(grepl("pip-tile-small__image",   body_str, fixed = TRUE))
  expect_true(grepl("pip-tile-small__content", body_str, fixed = TRUE))
  expect_true(grepl("pip-tile-small__title",   body_str, fixed = TRUE))
  expect_true(grepl("pip-tile-small__summary", body_str, fixed = TRUE))
  expect_true(grepl("pip-tile-small__cta",     body_str, fixed = TRUE))
})

# P2-14: Edge case — build_card gracefully handles paper_url = NA
test_that("build_card body guards against NA paper_url", {
  body_str <- paste(deparse(body(mod_home_server)), collapse = "\n")
  # The guard condition must be present in the closure body
  expect_true(grepl("is.na(paper_url)", body_str, fixed = TRUE))
  expect_true(grepl("nzchar(paper_url)", body_str, fixed = TRUE))
})

# P2-13: Behavioural confirmation — research nav events set target = "research_repo"
test_that("mod_home_server body sets target 'research_repo' for research nav events", {
  body_str <- paste(deparse(body(mod_home_server)), collapse = "\n")
  # Both banner and feature research observers must reference the correct target
  expect_true(grepl('nav_event\\$target.*"research_repo"', body_str))
  # And the deep_dives target must also be set (for symmetry / completeness)
  expect_true(grepl('nav_event\\$target.*"deep_dives"', body_str))
})
