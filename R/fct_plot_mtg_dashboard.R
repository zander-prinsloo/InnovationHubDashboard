#' Plot functions for the MTG (NA–Survey Gap Adjustment) method
#'
#' @description Chart functions for Method 4 of the Innovation Hub Deep Dives.
#'   Shows (1) the NA–survey welfare gap as a function of GDP per capita, and
#'   (2) the sensitivity of the Gini coefficient to different assumptions about
#'   how much of the gap is allocated to the top tail.
#'
#' @section Region colour palette:
#'   MTG charts use WDI region codes as keys (SSF, ECS, MEA, LCN, EAS, SAS, NAC).
#'   This differs from the PIP-style codes used in other dashboard methods.
#'
#' @name fct_plot_mtg_dashboard
NULL


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

#' WDI region colour palette for MTG charts
#'
#' @format A named character vector mapping WDI region iso3c codes to hex
#'   colour strings.
#' @noRd
MTG_REGION_COLORS <- c(
  "SSF" = "#FF9800",   # Sub-Saharan Africa
  "ECS" = "#AA0000",   # Europe & Central Asia
  "MEA" = "#664AB6",   # Middle East, N. Africa, Afghanistan & Pakistan
  "LCN" = "#0C7C68",   # Latin America & Caribbean
  "EAS" = "#F3578E",   # East Asia & Pacific
  "SAS" = "#4EC2C0",   # South Asia
  "NAC" = "#34A7F2"    # North America
)

#' Welfare-type colours for highlighted points in Chart 1
#' @noRd
MTG_WELFARE_COLORS <- c(
  "consumption" = "#B22D2D",   # dark red
  "income"      = "#1F4E79"    # dark blue
)

#' Preferred GDP pc tick values for Chart 1 x-axis (log scale)
#' Covers ~95% of the country range in 2021 PPP $/day.
#' @noRd
MTG_GDPPC_TICKS <- c(1, 2, 5, 10, 20, 50, 100, 200, 400)


# ---------------------------------------------------------------------------
# Helper: LOESS smoother
# ---------------------------------------------------------------------------

#' Compute a LOESS trend line for overlay on scatter plots
#'
#' @param x Numeric vector of x values (e.g. log GDP per capita).
#' @param y Numeric vector of y values (e.g. gap).
#' @param span Numeric smoothing span passed to [stats::loess()]. Default 0.75.
#' @param n_out Integer number of evenly-spaced output points. Default 100.
#'
#' @return A data frame with columns `x` and `y_smooth`, or `NULL` if fewer
#'   than 5 complete observations are available.
#'
#' @examples
#' set.seed(42)
#' df <- compute_loess_line(1:20, rnorm(20))
#' head(df)
#'
#' @noRd
compute_loess_line <- function(x, y, span = 0.75, n_out = 100) {
  # Keep only complete pairs
  ok    <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
  x_use <- x[ok]
  y_use <- y[ok]

  if (length(x_use) < 5L) {
    return(NULL)
  }

  tryCatch(
    {
      fit    <- suppressWarnings(stats::loess(y_use ~ x_use, span = span))
      x_seq  <- seq(min(x_use), max(x_use), length.out = n_out)
      y_pred <- stats::predict(fit, newdata = data.frame(x_use = x_seq))
      data.frame(x = x_seq, y_smooth = y_pred)
    },
    error = function(e) NULL
  )
}


# ---------------------------------------------------------------------------
# Chart 1: NA–Survey gap by GDP per capita
# ---------------------------------------------------------------------------

#' Plot NA–survey gap against GDP per capita (Chart 1 for MTG method)
#'
#' @description Scatter plot showing the magnitude of the NA–survey welfare gap
#'   as a function of GDP per capita (PPP, log scale). Background countries are
#'   shown as hollow circles coloured by welfare type at low opacity; the
#'   selected country is highlighted with solid filled circles. Separate LOESS
#'   trend lines are drawn for income and consumption observations.
#'
#' @param data A data.table produced by `data-raw/d_yk.R`. Must contain columns
#'   `country_code`, `country_name`, `year`, `welfare_type`, `region_code`,
#'   `is_latest`, and the gap/GDP pc columns implied by `na_type` and
#'   `ppp_vintage`.
#' @param select_country Character. Country name (as in `country_name`) to
#'   highlight. If `NULL` or not found, no country is highlighted.
#' @param na_type Character. National accounts aggregate to use for the gap.
#'   One of `"hfce"` (HFCE; default) or `"gdp"`.
#' @param ppp_vintage Character. PPP vintage. One of `"2021"` (default) or
#'   `"2017"`.
#' @param show_all_years Logical. If `FALSE` (default), only each country's
#'   most recent survey year is shown. If `TRUE`, all years are shown.
#'
#' @return A [plotly::plot_ly()] object.
#'
#' @examples
#' \dontrun{
#' load("data/d_yk.rda")
#' plot_mtg_gap_gdppc(d_yk, select_country = "India")
#' }
#'
#' @importFrom plotly plot_ly add_trace add_segments add_annotations layout
#' @importFrom data.table as.data.table fsubset
#' @noRd
plot_mtg_gap_gdppc <- function(data,
                               select_country = NULL,
                               na_type        = c("hfce", "gdp"),
                               ppp_vintage    = c("2021", "2017"),
                               show_all_years = FALSE) {

  na_type     <- match.arg(na_type)
  ppp_vintage <- match.arg(ppp_vintage)

  # -- Column names derived from toggles ------------------------------------
  gap_col    <- paste0("gap_",    na_type, "_", ppp_vintage)
  gdppc_col  <- paste0("gdp_pc_ppp_", ppp_vintage)
  na_label   <- if (na_type == "hfce") "HFCE" else "GDP"
  ppp_label  <- paste0(ppp_vintage, " PPP")   # "2021 PPP" / "2017 PPP"

  # -- Filter rows ----------------------------------------------------------
  dt <- as.data.table(data)
  if (!show_all_years) {
    dt <- dt[is_latest == TRUE]
  }
  # Deduplicate: one row per country × year × welfare_type (share irrelevant
  # for Chart 1 which shows gap, not Gini; gap is the same across share rows)
  dt <- unique(dt, by = c("country_code", "year", "welfare_type"))

  # Drop rows where either variable is NA or non-finite
  dt <- dt[!is.na(get(gap_col)) & !is.na(get(gdppc_col)) &
             is.finite(get(gap_col)) & is.finite(get(gdppc_col))]

  if (nrow(dt) == 0L) {
    return(
      plotly::plot_ly() |>
        plotly::add_annotations(
          text      = "No data available for the selected options.",
          x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(size = 16, color = "grey50")
        ) |>
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }

  # -- Identify selected country rows ---------------------------------------
  selected_codes <- character(0)
  if (!is.null(select_country) && nchar(select_country) > 0) {
    selected_codes <- dt[country_name == select_country, unique(country_code)]
  }

  dt_bg   <- dt[!country_code %in% selected_codes]  # background (grey)
  dt_fg   <- dt[country_code  %in% selected_codes]  # foreground (highlighted)

  # -- Axis values ----------------------------------------------------------
  x_bg <- dt_bg[[gdppc_col]]
  y_bg <- dt_bg[[gap_col]]

  # -- Build plot -----------------------------------------------------------
  pp <- plotly::plot_ly()

  # Zero-gap reference line
  x_range_raw <- range(dt[[gdppc_col]], na.rm = TRUE)
  pp <- pp |>
    plotly::add_segments(
      x        = x_range_raw[1],
      xend     = x_range_raw[2],
      y        = 0,
      yend     = 0,
      line     = list(color = "black", width = 1.5, dash = "dash"),
      showlegend = FALSE,
      hoverinfo  = "skip"
    )

  # Background points: one trace per welfare type, hollow circles coloured by
  # welfare type at low opacity. No tooltips on background points.
  bg_welfare_types <- sort(unique(dt_bg$welfare_type))
  for (wt in bg_welfare_types) {
    dt_bg_wt  <- dt_bg[welfare_type == wt]
    color_wt  <- MTG_WELFARE_COLORS[wt]
    if (is.na(color_wt)) color_wt <- "#888888"

    # Convert hex to rgba for the circle border at low opacity
    # Hollow circle: transparent fill, coloured border at 0.35 opacity
    rgb_vals  <- grDevices::col2rgb(color_wt)
    border_rgba <- sprintf("rgba(%d,%d,%d,0.35)",
                           rgb_vals[1], rgb_vals[2], rgb_vals[3])

    pp <- pp |>
      plotly::add_trace(
        x          = dt_bg_wt[[gdppc_col]],
        y          = dt_bg_wt[[gap_col]],
        type       = "scatter",
        mode       = "markers",
        marker     = list(
          size   = 7,
          color  = "rgba(0,0,0,0)",          # transparent fill
          line   = list(color = border_rgba, width = 1)
        ),
        hoverinfo  = "skip",                 # no tooltip on background points
        legendgroup = wt,
        showlegend  = TRUE,
        name        = tools::toTitleCase(wt)
      )
  }

  # LOESS trend lines per welfare type (computed on full dataset)
  welfare_types <- unique(dt$welfare_type)
  for (wt in welfare_types) {
    dt_wt    <- dt[welfare_type == wt]
    loess_df <- compute_loess_line(
      x    = log(dt_wt[[gdppc_col]]),
      y    = dt_wt[[gap_col]],
      span = 0.75
    )
    if (!is.null(loess_df)) {
      color_wt <- MTG_WELFARE_COLORS[wt]
      if (is.na(color_wt)) color_wt <- "grey40"

      # Convert log x back to original scale for plotly log axis
      pp <- pp |>
        plotly::add_trace(
          x          = exp(loess_df$x),
          y          = loess_df$y_smooth,
          type       = "scatter",
          mode       = "lines",
          line       = list(color = color_wt, width = 1.8, dash = "dot"),
          legendgroup = wt,
          showlegend = FALSE,
          name       = paste0("Trend: ", wt),
          hoverinfo  = "skip"
        )
    }
  }

  # Foreground: highlighted country points — added LAST so they render on top
  # and their tooltips are never obscured by background trace hover areas.
  if (nrow(dt_fg) > 0) {
    for (wt in unique(dt_fg$welfare_type)) {
      dt_wt    <- dt_fg[welfare_type == wt]
      color_wt <- MTG_WELFARE_COLORS[wt]
      if (is.na(color_wt)) color_wt <- "steelblue"

      fg_tooltip <- paste0(
        "<b>", dt_wt$country_name, "</b> (", dt_wt$country_code, ")<br>",
        "Year: ", dt_wt$year, "<br>",
        "Welfare: ", dt_wt$welfare_type, "<br>",
        "GDP pc (", ppp_label, "): ", round(dt_wt[[gdppc_col]], 1), " $/day<br>",
        na_label, "\u2013survey gap: ", round(dt_wt[[gap_col]], 3)
      )
      pp <- pp |>
        plotly::add_trace(
          x         = dt_wt[[gdppc_col]],
          y         = dt_wt[[gap_col]],
          type      = "scatter",
          mode      = "markers",
          marker    = list(
            size  = 14,
            color = color_wt,
            line  = list(color = "white", width = 1.5)
          ),
          text          = fg_tooltip,
          hovertemplate = "%{text}<extra></extra>",
          legendgroup   = wt,
          showlegend    = TRUE,
          name          = paste0(tools::toTitleCase(wt), " (selected)")
        )
    }
  }

  # -- Layout ---------------------------------------------------------------
  country_label <- if (!is.null(select_country) && length(selected_codes) > 0) {
    paste0("<br><span style='font-size:11px; font-weight:normal;'>",
           "Highlighted: ", select_country, "</span>")
  } else {
    ""
  }

  subtitle <- paste0(
    "Positive values: survey mean < ", na_label,
    " (survey may understate welfare). ",
    "Gap = (survey \u2212 ", na_label, ") / ", na_label, ". ",
    ppp_label, "."
  )

  pp |>
    plotly::layout(
      title = list(
        text = paste0(
          "<b>", na_label, "\u2013survey welfare gap by GDP per capita</b>",
          country_label
        ),
        font = list(size = 15)
      ),
      xaxis = list(
        title     = paste0("<b>GDP per capita (PPP, log scale, ", ppp_label, ")</b>"),
        type      = "log",
        tickvals  = MTG_GDPPC_TICKS,
        ticktext  = MTG_GDPPC_TICKS,
        gridcolor = "rgba(200,200,200,0.3)",
        zeroline  = FALSE
      ),
      yaxis = list(
        title     = paste0("<b>", na_label, "\u2013survey gap</b>"),
        gridcolor = "rgba(200,200,200,0.3)",
        zeroline  = FALSE
      ),
      annotations = list(list(
        text      = subtitle,
        x         = 0,
        y         = -0.12,
        xref      = "paper",
        yref      = "paper",
        showarrow = FALSE,
        xanchor   = "left",
        font      = list(size = 10, color = "grey40")
      )),
      legend = list(
        orientation = "h",
        x = 0.5, y = -0.2,
        xanchor = "center", yanchor = "top"
      ),
      hovermode     = "closest",
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin        = list(l = 80, r = 60, t = 80, b = 100)
    )
}


# ---------------------------------------------------------------------------
# Chart 2: Gini sensitivity to gap-allocation rule
# ---------------------------------------------------------------------------

#' Plot sensitivity of Gini coefficient to gap-allocation scenario (Chart 2)
#'
#' @description Scatter plot comparing the survey Gini (x-axis) to the
#'   gap-adjusted Gini (y-axis) across countries and gap-share scenarios.
#'   A 45-degree reference line (y = x) separates countries where the adjustment
#'   raises inequality (above line) from those where it lowers it (below line).
#'   Points are coloured by scenario (share of NA–survey gap allocated to the
#'   top tail). The selected country is highlighted.
#'
#' @param data A data.table produced by `data-raw/d_yk.R`. Must contain columns
#'   `country_code`, `country_name`, `year`, `region_code`, `region_name`,
#'   `share`, and the Gini columns implied by `na_type` and `ppp_vintage`.
#' @param select_country Character. Country name to highlight. May be `NULL`.
#' @param na_type Character. One of `"hfce"` (default) or `"gdp"`.
#' @param ppp_vintage Character. One of `"2021"` (default) or `"2017"`.
#' @param show_latest_only Logical. If `TRUE`, only each country's most recent
#'   survey year (per `country_code × welfare_type`) is shown. Default `FALSE`
#'   shows all years.
#'
#' @return A [plotly::plot_ly()] object.
#'
#' @examples
#' \dontrun{
#' load("data/d_yk.rda")
#' plot_mtg_gini_sensitivity(d_yk, select_country = "India")
#' }
#'
#' @importFrom plotly plot_ly add_trace add_segments add_annotations layout
#' @importFrom data.table as.data.table
#' @noRd
plot_mtg_gini_sensitivity <- function(data,
                                      select_country  = NULL,
                                      na_type         = c("hfce", "gdp"),
                                      ppp_vintage     = c("2021", "2017"),
                                      show_latest_only = FALSE) {

  na_type     <- match.arg(na_type)
  ppp_vintage <- match.arg(ppp_vintage)

  # -- Column names ---------------------------------------------------------
  gini_survey_col <- paste0("gini_survey_", ppp_vintage)
  gini_adj_col    <- paste0("gini_", na_type, "_adj_", ppp_vintage)
  na_label        <- if (na_type == "hfce") "HFCE" else "GDP"
  ppp_label       <- paste0(ppp_vintage, " PPP")

  dt <- as.data.table(data)

  # Optionally restrict to each country's latest survey year (per welfare_type)
  if (show_latest_only) {
    dt <- dt[is_latest == TRUE]
  }

  # Drop rows where either Gini is NA
  dt <- dt[!is.na(get(gini_survey_col)) & !is.na(get(gini_adj_col))]

  if (nrow(dt) == 0L) {
    return(
      plotly::plot_ly() |>
        plotly::add_annotations(
          text      = "No Gini data available for the selected options.",
          x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(size = 16, color = "grey50")
        ) |>
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }

  # -- Dynamic share levels (read from data, never hardcoded) ---------------
  share_levels <- sort(unique(dt$share))
  n_shares     <- length(share_levels)

  # Sequential colour palette from light to dark (low share = light)
  share_palette <- grDevices::colorRampPalette(
    c("#c6dbef", "#084594")
  )(n_shares)
  names(share_palette) <- as.character(share_levels)

  # -- Identify selected country -------------------------------------------
  selected_codes <- character(0)
  if (!is.null(select_country) && nchar(select_country) > 0) {
    selected_codes <- dt[country_name == select_country, unique(country_code)]
  }

  # -- Axis limits ----------------------------------------------------------
  all_gini_vals <- c(dt[[gini_survey_col]], dt[[gini_adj_col]])
  axis_min      <- max(0,    floor(min(all_gini_vals, na.rm = TRUE)  * 10) / 10 - 0.02)
  axis_max      <- min(1,    ceiling(max(all_gini_vals, na.rm = TRUE) * 10) / 10 + 0.02)

  # -- Build plot -----------------------------------------------------------
  pp <- plotly::plot_ly()

  # 45-degree reference line y = x
  pp <- pp |>
    plotly::add_segments(
      x = axis_min, xend = axis_max,
      y = axis_min, yend = axis_max,
      line       = list(color = "grey60", width = 1.5, dash = "dash"),
      showlegend = FALSE,
      hoverinfo  = "skip",
      name       = "No change (y = x)"
    )

  # Points per share scenario
  for (i in seq_along(share_levels)) {
    sh     <- share_levels[i]
    col    <- share_palette[as.character(sh)]
    dt_sh  <- dt[share == sh]

    # Separate selected vs background
    dt_sh_bg <- dt_sh[!country_code %in% selected_codes]
    dt_sh_fg <- dt_sh[ country_code %in% selected_codes]

    tooltip_bg <- paste0(
      "<b>", dt_sh_bg$country_name, "</b> (", dt_sh_bg$country_code, ")<br>",
      "Region: ", dt_sh_bg$region_name, "<br>",
      "Year: ", dt_sh_bg$year, "<br>",
      "Share to top tail: ", sh, "%<br>",
      "Survey Gini: ", round(dt_sh_bg[[gini_survey_col]], 3), "<br>",
      na_label, "-adj Gini: ", round(dt_sh_bg[[gini_adj_col]], 3), "<br>",
      "Change: ", round(dt_sh_bg[[gini_adj_col]] - dt_sh_bg[[gini_survey_col]], 3)
    )

    if (nrow(dt_sh_bg) > 0) {
      pp <- pp |>
        plotly::add_trace(
          x    = dt_sh_bg[[gini_survey_col]],
          y    = dt_sh_bg[[gini_adj_col]],
          type = "scatter",
          mode = "markers",
          marker = list(
            size    = 7,
            color   = col,
            opacity = 0.55,
            line    = list(color = "rgba(255,255,255,0.4)", width = 0.5)
          ),
          text          = tooltip_bg,
          hovertemplate = "%{text}<extra></extra>",
          legendgroup   = as.character(sh),
          name          = paste0(sh, "%"),
          showlegend    = TRUE
        )
    }

    # Highlighted country
    if (nrow(dt_sh_fg) > 0) {
      tooltip_fg <- paste0(
        "<b>", dt_sh_fg$country_name, "</b> (", dt_sh_fg$country_code, ")<br>",
        "Region: ", dt_sh_fg$region_name, "<br>",
        "Year: ", dt_sh_fg$year, "<br>",
        "Share to top tail: ", sh, "%<br>",
        "Survey Gini: ", round(dt_sh_fg[[gini_survey_col]], 3), "<br>",
        na_label, "-adj Gini: ", round(dt_sh_fg[[gini_adj_col]], 3), "<br>",
        "Change: ", round(dt_sh_fg[[gini_adj_col]] - dt_sh_fg[[gini_survey_col]], 3)
      )
      pp <- pp |>
        plotly::add_trace(
          x    = dt_sh_fg[[gini_survey_col]],
          y    = dt_sh_fg[[gini_adj_col]],
          type = "scatter",
          mode = "markers",
          marker = list(
            size  = 14,
            color = col,
            line  = list(color = "black", width = 2)
          ),
          text          = tooltip_fg,
          hovertemplate = "%{text}<extra></extra>",
          legendgroup   = as.character(sh),
          name          = paste0(sh, "% (selected)"),
          showlegend    = FALSE
        )
    }
  }

  # -- Summary statistics annotation ----------------------------------------
  # Compute across currently displayed data (all share scenarios combined)
  mean_change   <- mean(dt[[gini_adj_col]] - dt[[gini_survey_col]], na.rm = TRUE)
  pct_above_diag <- mean(dt[[gini_adj_col]] > dt[[gini_survey_col]], na.rm = TRUE) * 100

  ann_text <- paste0(
    "Mean Gini change (all scenarios): ",
    sprintf("%+.3f", mean_change),
    "<br>% above diagonal (all scenarios): ",
    sprintf("%.0f%%", pct_above_diag)
  )

  pp <- pp |>
    plotly::add_annotations(
      text      = ann_text,
      x         = axis_max,
      y         = axis_min + 0.01,
      xref      = "x",
      yref      = "y",
      showarrow = FALSE,
      xanchor   = "right",
      yanchor   = "bottom",
      align     = "right",
      font      = list(size = 10, color = "grey30"),
      bgcolor   = "rgba(255,255,255,0.7)",
      borderpad = 4
    )

  # -- Layout ---------------------------------------------------------------
  country_label <- if (!is.null(select_country) && length(selected_codes) > 0) {
    paste0("<br><span style='font-size:11px; font-weight:normal;'>",
           "Highlighted: ", select_country, "</span>")
  } else {
    ""
  }

  pp |>
    plotly::layout(
      title = list(
        text = paste0(
          "<b>Sensitivity of Gini to gap-allocation scenario</b>",
          country_label
        ),
        font = list(size = 15)
      ),
      xaxis = list(
        title     = "<b>Gini coefficient (survey)</b>",
        range     = c(axis_min, axis_max),
        gridcolor = "rgba(200,200,200,0.3)",
        zeroline  = FALSE
      ),
      yaxis = list(
        title        = paste0("<b>Gini coefficient (", na_label, "-adjusted)</b>"),
        range        = c(axis_min, axis_max),
        scaleanchor  = "x",
        scaleratio   = 1,
        gridcolor    = "rgba(200,200,200,0.3)",
        zeroline     = FALSE
      ),
      legend = list(
        title       = list(text = "<b>Share of gap<br>to top tail</b>"),
        orientation = "v",
        x           = 1.02,
        y           = 1,
        xanchor     = "left",
        yanchor     = "top"
      ),
      annotations = list(list(
        text      = paste0(
          "Points above the diagonal: adjustment raises inequality. ",
          na_label, " aggregate, ", ppp_label, "."
        ),
        x = 0, y = -0.1,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "left",
        font = list(size = 10, color = "grey40")
      )),
      hovermode     = "closest",
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin        = list(l = 80, r = 150, t = 80, b = 90)
    )
}
