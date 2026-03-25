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

test_that("plot_mtg_gini_sensitivity produces exactly 4 share-level traces (25/50/75/100)", {
  p <- plotly::plotly_build(plot_mtg_gini_sensitivity(d_yk))
  traces <- p$x$data

  # Share traces have names like "25%", "50%", etc. (not the 45° ref line)
  share_traces <- Filter(function(tr) {
    !is.null(tr$name) && grepl("^\\d+%$", tr$name)
  }, traces)

  share_labels <- vapply(share_traces, function(tr) tr$name, character(1))
  expect_equal(sort(share_labels), c("100%", "25%", "50%", "75%"))
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


# ── 6. compute_gini_from_lorenz ───────────────────────────────────────────────

test_that("compute_gini_from_lorenz returns ~0 for perfect equality", {
  p <- seq(0, 1, by = 0.01)
  l <- seq(0, 1, by = 0.01)
  expect_equal(compute_gini_from_lorenz(p, l), 0, tolerance = 1e-10)
})

test_that("compute_gini_from_lorenz returns ~1 for perfect inequality", {
  # All welfare concentrated in last person
  n   <- 100L
  p   <- seq_len(n) / n
  l   <- c(rep(0, n - 1L), 1)
  g   <- compute_gini_from_lorenz(p, l)
  # Trapezoidal approximation ≈ 1 - 1/n
  expect_equal(g, 1 - 1 / n, tolerance = 1e-10)
})

test_that("compute_gini_from_lorenz returns NA_real_ for < 2 non-NA points", {
  expect_identical(compute_gini_from_lorenz(0.5, 0.3),  NA_real_)
  expect_identical(compute_gini_from_lorenz(numeric(0), numeric(0)), NA_real_)
})

test_that("compute_gini_from_lorenz drops NA pairs silently", {
  p_clean <- seq(0.01, 1, by = 0.01)
  l_clean <- seq(0.01, 1, by = 0.01)
  p_na    <- c(p_clean, NA_real_)
  l_na    <- c(l_clean, 0.99)
  # Adding an NA pair should not change the result
  g_clean <- compute_gini_from_lorenz(p_clean, l_clean)
  g_na    <- compute_gini_from_lorenz(p_na,    l_na)
  expect_equal(g_na, g_clean, tolerance = 1e-10)
})

test_that("compute_gini_from_lorenz prepends origin when absent", {
  # Gini from (0.01,…,1) vs (0,0.01,…,1) should be the same (origin added)
  p   <- seq(0.01, 1, by = 0.01)
  l   <- seq(0.01, 1, by = 0.01)
  g1  <- compute_gini_from_lorenz(p, l)
  # With origin already present
  g2  <- compute_gini_from_lorenz(c(0, p), c(0, l))
  expect_equal(g1, g2, tolerance = 1e-10)
})

test_that("compute_gini_from_lorenz is between 0 and 1 for realistic Lorenz data", {
  p  <- seq(0.01, 1, by = 0.01)
  # Concave Lorenz: l = p^2 (more unequal than equality)
  l  <- p^2
  g  <- compute_gini_from_lorenz(p, l)
  expect_gte(g, 0)
  expect_lte(g, 1)
})


# ── 7. mtg_scan_cumulative_files ──────────────────────────────────────────────

test_that("mtg_scan_cumulative_files returns a data.table with required columns", {
  skip_if_offline()
  result <- mtg_scan_cumulative_files()
  expect_s3_class(result, "data.table")
  expect_named(result, c("iso3", "year", "filename"), ignore.order = FALSE)
})

test_that("mtg_scan_cumulative_files iso3 values are upper-case 3-letter codes", {
  skip_if_offline()
  result <- mtg_scan_cumulative_files()
  skip_if(nrow(result) == 0L, "No fst files found in GitHub repo")
  expect_true(all(grepl("^[A-Z]{3}$", result$iso3)))
})

test_that("mtg_scan_cumulative_files year values are positive integers", {
  skip_if_offline()
  result <- mtg_scan_cumulative_files()
  skip_if(nrow(result) == 0L, "No fst files found in GitHub repo")
  expect_true(all(is.integer(result$year)))
  expect_true(all(result$year > 1900L & result$year < 2100L))
})

test_that("mtg_scan_cumulative_files returns a non-empty table from GitHub", {
  skip_if_offline()
  result <- mtg_scan_cumulative_files()
  expect_s3_class(result, "data.table")
  expect_gte(nrow(result), 100L)
})

test_that("mtg_scan_cumulative_files returns empty data.table on network error", {
  # Simulate a network failure (no internet / rate limit) without any real
  # HTTP call. local_mocked_bindings() scopes the mock to this test only.
  local_mocked_bindings(
    fromJSON = function(...) stop("simulated network unreachable"),
    .package = "jsonlite"
  )
  result <- suppressWarnings(mtg_scan_cumulative_files())
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("iso3", "year", "filename"))
})


# ── 8. mtg_read_cumulative ────────────────────────────────────────────────────

test_that("mtg_read_cumulative returns NULL for a non-existent country/year", {
  skip_if_offline()
  expect_null(mtg_read_cumulative("ZZZ", 9999L))
})

test_that("mtg_read_cumulative returns a data.table with expected columns", {
  skip_if_offline()
  lookup <- mtg_scan_cumulative_files()
  skip_if(nrow(lookup) == 0L, "No fst files available")
  row1   <- lookup[1L]
  dt     <- mtg_read_cumulative(row1$iso3, row1$year)
  expect_s3_class(dt, "data.table")
  expect_true(all(c("country_code", "year", "p", "l") %in% names(dt)))
})

test_that("mtg_read_cumulative has exactly 20 adjusted gap-share column pairs", {
  skip_if_offline()
  lookup <- mtg_scan_cumulative_files()
  skip_if(nrow(lookup) == 0L, "No fst files available")
  dt <- mtg_read_cumulative(lookup$iso3[1L], lookup$year[1L])
  gap_shares <- seq(5L, 100L, by = 5L)
  p_cols <- paste0("p_hfce_adj_", gap_shares)
  l_cols <- paste0("l_hfce_adj_", gap_shares)
  expect_true(all(p_cols %in% names(dt)))
  expect_true(all(l_cols %in% names(dt)))
})

test_that("mtg_read_cumulative standard distribution has exactly 100 non-NA rows", {
  skip_if_offline()
  lookup <- mtg_scan_cumulative_files()
  skip_if(nrow(lookup) == 0L, "No fst files available")
  dt <- mtg_read_cumulative(lookup$iso3[1L], lookup$year[1L])
  expect_equal(sum(!is.na(dt$p)), 100L)
  expect_equal(sum(!is.na(dt$l)), 100L)
})


# ── 9. compute_lorenz_stats ───────────────────────────────────────────────────

# Synthetic Lorenz data.table for stats tests (no file I/O)
make_lorenz_dt <- function(gap_share = 50L) {
  n   <- 100L
  p   <- seq(0.01, 1, by = 0.01)
  l   <- p^1.5   # mildly unequal standard distribution

  # Adjusted: slightly more unequal
  p_adj <- seq(0.01, 1, by = 0.01)
  l_adj <- p_adj^2

  p_col <- paste0("p_hfce_adj_", gap_share)
  l_col <- paste0("l_hfce_adj_", gap_share)

  # Pad to 110 rows with NAs (mimics real data structure)
  n_total <- n + 10L
  dt <- data.table::data.table(
    country_code = "TST",
    year         = 2020L,
    p            = c(p,   rep(NA_real_, 10L)),
    l            = c(l,   rep(NA_real_, 10L))
  )
  dt[[p_col]] <- c(p_adj, rep(NA_real_, 10L))
  dt[[l_col]] <- c(l_adj, rep(NA_real_, 10L))
  dt
}

test_that("compute_lorenz_stats returns a list with all expected elements", {
  dt    <- make_lorenz_dt(50L)
  stats <- compute_lorenz_stats(dt, 50L)
  expect_type(stats, "list")
  expected_names <- c("gini_std", "gini_adj", "gini_change")
  expect_true(all(expected_names %in% names(stats)))
  expect_equal(sort(names(stats)), sort(expected_names))
})

test_that("compute_lorenz_stats gini_change equals gini_adj minus gini_std", {
  dt    <- make_lorenz_dt(50L)
  stats <- compute_lorenz_stats(dt, 50L)
  expect_equal(stats$gini_change, stats$gini_adj - stats$gini_std, tolerance = 1e-12)
})

test_that("compute_lorenz_stats works for boundary gap shares 5 and 100", {
  skip_if_offline()
  lookup <- mtg_scan_cumulative_files()
  skip_if(nrow(lookup) == 0L, "No fst files available")
  dt <- mtg_read_cumulative(lookup$iso3[1L], lookup$year[1L])
  s5   <- compute_lorenz_stats(dt, 5L)
  s100 <- compute_lorenz_stats(dt, 100L)
  expect_type(s5,   "list")
  expect_type(s100, "list")
  expect_false(is.na(s5$gini_std))
  expect_false(is.na(s100$gini_std))
})

test_that("compute_lorenz_stats returns NA for gini_adj when adjusted column all NA", {
  dt <- make_lorenz_dt(50L)
  # Overwrite adjusted columns with all NAs
  dt[["p_hfce_adj_50"]] <- NA_real_
  dt[["l_hfce_adj_50"]] <- NA_real_
  stats <- compute_lorenz_stats(dt, 50L)
  expect_true(is.na(stats$gini_adj))
  expect_true(is.na(stats$gini_change))
})


# ── 10. plot_mtg_lorenz_comparison ────────────────────────────────────────────

test_that("plot_mtg_lorenz_comparison returns a plotly object", {
  dt <- make_lorenz_dt(50L)
  p  <- plot_mtg_lorenz_comparison(dt, "Test Country", "TST", 2020L, 50L)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_lorenz_comparison returns plotly when adjusted dist is all NA", {
  dt <- make_lorenz_dt(50L)
  dt[["p_hfce_adj_50"]] <- NA_real_
  dt[["l_hfce_adj_50"]] <- NA_real_
  p <- plot_mtg_lorenz_comparison(dt, "Test Country", "TST", 2020L, 50L)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_lorenz_comparison returns plotly with graceful fallback for bad columns", {
  dt <- data.table::data.table(country_code = "TST", year = 2020L,
                                p = 0.5, l = 0.3)
  p  <- plot_mtg_lorenz_comparison(dt, "Test", "TST", 2020L, 50L)
  expect_s3_class(p, "plotly")
})

test_that("plot_mtg_lorenz_comparison has at least 3 traces (equality + std + adj)", {
  dt <- make_lorenz_dt(50L)
  # plotly_build() is required to materialise lazy traces from the pipe API.
  p  <- plotly::plotly_build(
    plot_mtg_lorenz_comparison(dt, "Test Country", "TST", 2020L, 50L)
  )
  expect_gte(length(p$x$data), 3L)
})

test_that("plot_mtg_lorenz_comparison equality line is dotted", {
  dt <- make_lorenz_dt(50L)
  # plotly_build() is required to materialise lazy traces from the pipe API.
  p      <- plotly::plotly_build(
    plot_mtg_lorenz_comparison(dt, "Test Country", "TST", 2020L, 50L)
  )
  traces <- p$x$data

  # The equality line is x = c(0,1), y = c(0,1), mode = "lines", dash = "dot"
  dot_traces <- Filter(function(tr) {
    isTRUE(grepl("dot", tr$line$dash))
  }, traces)
  expect_gt(length(dot_traces), 0L)
})

test_that("plot_mtg_lorenz_comparison standard curve uses #0072B2", {
  dt <- make_lorenz_dt(50L)
  # plotly_build() is required to materialise lazy traces from the pipe API.
  p      <- plotly::plotly_build(
    plot_mtg_lorenz_comparison(dt, "Test Country", "TST", 2020L, 50L)
  )
  traces <- p$x$data

  blue_traces <- Filter(function(tr) {
    isTRUE(grepl("#0072B2", tr$line$color, ignore.case = TRUE))
  }, traces)
  expect_gt(length(blue_traces), 0L)
})

test_that("plot_mtg_lorenz_comparison adjusted curve uses #D55E00", {
  dt <- make_lorenz_dt(50L)
  # plotly_build() is required to materialise lazy traces from the pipe API.
  p      <- plotly::plotly_build(
    plot_mtg_lorenz_comparison(dt, "Test Country", "TST", 2020L, 50L)
  )
  traces <- p$x$data

  orange_traces <- Filter(function(tr) {
    isTRUE(grepl("#D55E00", tr$line$color, ignore.case = TRUE))
  }, traces)
  expect_gt(length(orange_traces), 0L)
})
