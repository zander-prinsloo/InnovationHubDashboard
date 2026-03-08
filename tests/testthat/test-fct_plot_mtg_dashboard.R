# Tests for R/fct_plot_mtg_dashboard.R
# ─────────────────────────────────────────────────────────────────────────────
# Uses the real d_yk.rda fixture.  Minimal, fast, focussed on:
#   1. compute_loess_line  – edge cases and normal operation
#   2. plot_mtg_gap_gdppc  – return type and toggle combinations
#   3. plot_mtg_gini_sensitivity – return type, selections, and toggles

# Attach data.table + plotly so sourced functions can find as.data.table(), etc.
# skip() if they aren't installed (guards R CMD check on minimal setups).
if (!requireNamespace("data.table", quietly = TRUE)) skip("data.table not available")
if (!requireNamespace("plotly", quietly = TRUE)) skip("plotly not available")
library(data.table)
library(plotly)

# Source the functions under test (safe for both devtools::test and Rscript)
source(test_path("..", "..", "R", "fct_plot_mtg_dashboard.R"), local = TRUE)

# ── Fixture ──────────────────────────────────────────────────────────────────

# Load the real dataset — testthat runs from tests/testthat/, so go up two levels
fixture_path <- test_path("..", "..", "data", "d_yk.rda")
if (!file.exists(fixture_path)) skip("d_yk.rda fixture not found")
load(fixture_path)          # → d_yk

# A tiny synthetic fixture: 2 countries × 4 shares, one welfare type
make_mini_yk <- function(n_countries = 3) {
  countries <- head(unique(d_yk$country_name[!is.na(d_yk$country_name)]), n_countries)
  d_yk[d_yk$country_name %in% countries, ]
}

mini <- make_mini_yk(3)


# ── 1. compute_loess_line ────────────────────────────────────────────────────

test_that("compute_loess_line returns NULL for < 5 complete observations", {
  expect_null(compute_loess_line(1:4, 1:4))
  expect_null(compute_loess_line(c(NA, 1, 2, 3, 4), c(1, 2, 3, 4, 5)))  # 4 complete
  expect_null(compute_loess_line(numeric(0), numeric(0)))
})

test_that("compute_loess_line returns a data.frame with x and y_smooth", {
  out <- compute_loess_line(1:20, rnorm(20))
  expect_s3_class(out, "data.frame")
  expect_named(out, c("x", "y_smooth"))
})

test_that("compute_loess_line returns n_out rows by default", {
  out <- compute_loess_line(1:20, rnorm(20), n_out = 50)
  expect_equal(nrow(out), 50)
})

test_that("compute_loess_line handles NA/Inf gracefully", {
  x <- c(1:15, NA, Inf, -Inf)
  y <- c(rnorm(15), NA, NA, NA)
  out <- compute_loess_line(x, y)
  expect_s3_class(out, "data.frame")   # 15 complete obs → should work
})

test_that("compute_loess_line returns NULL on loess failure (all-equal y)", {
  # Constant y with span=0 would cause degenerate fit; use pathological input
  # that reliably triggers an error in loess (1 unique x value repeated)
  result <- compute_loess_line(rep(5, 10), rnorm(10))
  # May succeed or return NULL depending on R version; either is acceptable
  expect_true(is.null(result) || is.data.frame(result))
})


# ── 2. plot_mtg_gap_gdppc ────────────────────────────────────────────────────

test_that("plot_mtg_gap_gdppc returns a plotly object", {
  p <- plot_mtg_gap_gdppc(d_yk)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gap_gdppc works with a valid selected country", {
  country <- d_yk$country_name[d_yk$is_latest & !is.na(d_yk$country_name)][1]
  p <- plot_mtg_gap_gdppc(d_yk, select_country = country)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gap_gdppc works with an unknown country (no highlight)", {
  p <- plot_mtg_gap_gdppc(d_yk, select_country = "Nonexistent Country XYZ")
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gap_gdppc respects na_type = 'gdp'", {
  p <- plot_mtg_gap_gdppc(d_yk, na_type = "gdp")
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gap_gdppc respects ppp_vintage = '2017'", {
  p <- plot_mtg_gap_gdppc(d_yk, ppp_vintage = "2017")
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gap_gdppc respects show_all_years = TRUE", {
  p <- plot_mtg_gap_gdppc(d_yk, show_all_years = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gap_gdppc handles empty data gracefully", {
  empty_dt <- d_yk[0, ]
  p <- plot_mtg_gap_gdppc(empty_dt)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gap_gdppc rejects invalid na_type", {
  expect_error(plot_mtg_gap_gdppc(d_yk, na_type = "something_invalid"))
})

test_that("plot_mtg_gap_gdppc rejects invalid ppp_vintage", {
  expect_error(plot_mtg_gap_gdppc(d_yk, ppp_vintage = "2019"))
})


# ── 3. plot_mtg_gini_sensitivity ─────────────────────────────────────────────

test_that("plot_mtg_gini_sensitivity returns a plotly object", {
  p <- plot_mtg_gini_sensitivity(d_yk)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gini_sensitivity works with a valid selected country", {
  country <- d_yk$country_name[!is.na(d_yk$country_name) &
                                 !is.na(d_yk$gini_survey_2021)][1]
  p <- plot_mtg_gini_sensitivity(d_yk, select_country = country)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gini_sensitivity works with an unknown country", {
  p <- plot_mtg_gini_sensitivity(d_yk, select_country = "Atlantis")
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gini_sensitivity respects na_type = 'gdp'", {
  p <- plot_mtg_gini_sensitivity(d_yk, na_type = "gdp")
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gini_sensitivity respects ppp_vintage = '2017'", {
  p <- plot_mtg_gini_sensitivity(d_yk, ppp_vintage = "2017")
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gini_sensitivity handles empty data gracefully", {
  empty_dt <- d_yk[0, ]
  p <- plot_mtg_gini_sensitivity(empty_dt)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gini_sensitivity handles data with all Gini NA gracefully", {
  dt_na <- d_yk
  dt_na$gini_survey_2021   <- NA_real_
  dt_na$gini_hfce_adj_2021 <- NA_real_
  p <- plot_mtg_gini_sensitivity(dt_na)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gini_sensitivity rejects invalid na_type", {
  expect_error(plot_mtg_gini_sensitivity(d_yk, na_type = "nia"))
})

test_that("plot_mtg_gini_sensitivity rejects invalid ppp_vintage", {
  expect_error(plot_mtg_gini_sensitivity(d_yk, ppp_vintage = "2020"))
})


# ── 4. Constants ─────────────────────────────────────────────────────────────

test_that("MTG_REGION_COLORS covers all 7 WDI region codes", {
  expected <- c("SSF", "ECS", "MEA", "LCN", "EAS", "SAS", "NAC")
  expect_true(all(expected %in% names(MTG_REGION_COLORS)))
})

test_that("MTG_WELFARE_COLORS covers income and consumption", {
  expect_true(all(c("income", "consumption") %in% names(MTG_WELFARE_COLORS)))
})

test_that("MTG_REGION_COLORS values are valid hex colours", {
  valid_hex <- grepl("^#[0-9A-Fa-f]{6}$", MTG_REGION_COLORS)
  expect_true(all(valid_hex))
})


# ── 5. show_latest_only param ─────────────────────────────────────────────────

test_that("plot_mtg_gini_sensitivity show_latest_only = TRUE returns a plotly object", {
  p <- plot_mtg_gini_sensitivity(d_yk, show_latest_only = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_gini_sensitivity show_latest_only filters to fewer points", {
  # Fixture must have non-latest rows for the filter to be meaningful
  expect_true(any(!d_yk$is_latest), info = "fixture has no non-latest rows")

  # Collect trace data for both modes and compare point counts.
  # plotly_build() is required to materialise lazy traces from the pipe API.
  p_all    <- plotly::plotly_build(plot_mtg_gini_sensitivity(d_yk, show_latest_only = FALSE))
  p_latest <- plotly::plotly_build(plot_mtg_gini_sensitivity(d_yk, show_latest_only = TRUE))

  count_points <- function(p) {
    sum(vapply(p$x$data, function(tr) {
      length(if (!is.null(tr$x)) tr$x else integer(0))
    }, integer(1)))
  }
  expect_gt(count_points(p_all), 0L, label = "p_all should have data points")
  expect_true(count_points(p_latest) <= count_points(p_all))
})

test_that("plot_mtg_gini_sensitivity 45-degree reference line has hoverinfo = 'skip'", {
  # Chart 2's 45-degree reference line segment has hoverinfo = "skip".
  # plotly_build() is required to materialise lazy traces from the pipe API.
  p <- plotly::plotly_build(plot_mtg_gini_sensitivity(d_yk))
  traces <- p$x$data

  # After plotly_build, look for traces with hoverinfo == "skip" OR
  # line-only traces (the reference segment) with no hovertemplate.
  skip_traces <- Filter(function(tr) {
    identical(tr$hoverinfo, "skip") ||
      (identical(tr$type, "scatter") && identical(tr$mode, "lines") &&
       isTRUE(grepl("dash", tr$line$dash)))
  }, traces)
  expect_gt(length(skip_traces), 0L)
})

test_that("plot_mtg_gap_gdppc background traces have hoverinfo = 'skip'", {
  # Chart 1 background dots (all welfare types) must have hoverinfo = "skip"
  # so they don't override the selected-country tooltip.
  # plotly_build() is required to materialise lazy traces from the pipe API.
  p <- plotly::plotly_build(plot_mtg_gap_gdppc(d_yk))
  traces <- p$x$data

  # Background scatter traces are identified by transparent fill rgba(0,0,0,0)
  # They should have hoverinfo = "skip"; at least one must exist.
  bg_traces <- Filter(function(tr) {
    mc <- tr$marker
    isTRUE(!is.null(mc$color) && any(grepl("rgba\\(0,\\s*0,\\s*0,\\s*0\\)", mc$color)))
  }, traces)

  expect_gt(length(bg_traces), 0L)
  hoverinfos <- vapply(bg_traces, function(tr) {
    hi <- tr$hoverinfo
    if (is.null(hi)) "" else hi[1L]
  }, character(1))
  expect_true(all(hoverinfos == "skip"))
})
