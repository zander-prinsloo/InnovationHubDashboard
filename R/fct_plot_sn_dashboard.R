#' Plot functions for the Subnational definition method
#'
#' @description Chart functions for comparing DB and DOU subnational
#'   poverty estimation methods. Includes a dumbbell chart for aggregate
#'   comparison and a range-bar chart showing granular sub-level spread.
#'
#' @noRd

# -- SN method colors --------------------------------------------------------
sn_method_colors <- c("db" = "#FF9800", "dou" = "#4EC2C0")
sn_method_fills  <- c("db" = "rgba(255,152,0,0.15)",
                       "dou" = "rgba(78,194,192,0.15)")


# -- Shared data prep helper ------------------------------------------------
#' Prepare fan/dumbbell data for a single economy
#'
#' @param data      Preprocessed d_sn (no default method, no headcount_default)
#' @param country_code ISO3 country code
#' @param selected_poverty_line One of "$2.15", "$3.65", "$6.85"
#'
#' @return A list with elements `points`, `fan`, `aggregate`
#' @noRd
prep_fan_data <- function(data, country_code, selected_poverty_line = "$2.15") {

  # Filter to country, poverty line, methods of interest, non-missing headcount
  d <- data |>
    collapse::fsubset(code == country_code &
                        poverty_line == selected_poverty_line &
                        method %in% c("db", "dou") &
                        !is.na(headcount_estimate))

  # Create display label for sub_level (empty -> reporting_level name)

  d <- d |>
    dplyr::mutate(
      level_label = dplyr::if_else(
        sub_level == "",
        stringr::str_to_title(reporting_level),
        stringr::str_to_title(sub_level)
      ),
      headcount_pct = headcount_estimate * 100,
      reporting_level = factor(reporting_level,
                               levels = c("urban", "national", "rural"),
                               ordered = TRUE),
      x_num = as.numeric(reporting_level),
      method_label = dplyr::case_when(
        method == "db"  ~ "DB",
        method == "dou" ~ "DOU"
      )
    )

  # Compute fan bounds per method x reporting_level
  fan <- d |>
    dplyr::summarise(
      y_min = min(headcount_pct),
      y_max = max(headcount_pct),
      .by = c(method, reporting_level, x_num, method_label)
    )

  # Aggregate line values (sub_level == "")
  agg <- d |>
    collapse::fsubset(sub_level == "") |>
    dplyr::select(method, reporting_level, x_num, method_label,
                  headcount_pct, level_label)

  list(points = d, fan = fan, aggregate = agg)
}


# ============================================================================
# Chart (i): Vertical dumbbell - aggregate method comparison
# ============================================================================
#' @param data      Preprocessed d_sn
#' @param country_code ISO3 code (used for filtering)
#' @param country_name Display name for titles (falls back to country_code)
#' @param selected_poverty_line Poverty line string
#' @noRd
plot_sn_dumbbell <- function(data,
                             country_code,
                             country_name = NULL,
                             selected_poverty_line = "$2.15") {

  if (is.null(country_name)) country_name <- country_code

  prep <- prep_fan_data(data, country_code, selected_poverty_line)
  agg  <- prep$aggregate |> dplyr::arrange(x_num)

  # Pivot to wide for segment drawing
  agg_wide <- agg |>
    dplyr::select(reporting_level, x_num, method, headcount_pct) |>
    tidyr::pivot_wider(names_from = method, values_from = headcount_pct) |>
    dplyr::mutate(
      delta     = db - dou,
      delta_lbl = paste0(dplyr::if_else(delta >= 0, "+", ""),
                         round(delta, 1), " pp"),
      mid_y     = (db + dou) / 2
    )

  # Slight horizontal offset so dots don't stack when values are close
  jitter <- 0.08
  method_offset <- c("dou" = -jitter, "db" = jitter)

  p <- plotly::plot_ly()

  # Grey connecting segments
  for (i in seq_len(nrow(agg_wide))) {
    row <- agg_wide[i, ]
    p <- p |>
      plotly::add_segments(
        x    = row$x_num + method_offset[["dou"]],
        xend = row$x_num + method_offset[["db"]],
        y    = row$dou,
        yend = row$db,
        line = list(color = "#363636", width = 3),
        showlegend = FALSE,
        hoverinfo = "skip"
      )
  }

  # DOU points
  agg_dou <- agg |> dplyr::filter(method == "dou")
  p <- p |>
    plotly::add_trace(
      x = agg_dou$x_num + method_offset[["dou"]],
      y = agg_dou$headcount_pct,
      type = "scatter", mode = "markers",
      marker = list(size = 15, color = sn_method_colors[["dou"]],
                    line = list(color = "white", width = 2)),
      text = paste0(
        "<b>DOU</b><br>",
        "Level: ", agg_dou$level_label, "<br>",
        "Headcount: ", round(agg_dou$headcount_pct, 1), "%"
      ),
      hovertemplate = "%{text}<extra></extra>",
      name = "DOU method", legendgroup = "dou"
    )

  # DB points
  agg_db <- agg |> dplyr::filter(method == "db")
  p <- p |>
    plotly::add_trace(
      x = agg_db$x_num + method_offset[["db"]],
      y = agg_db$headcount_pct,
      type = "scatter", mode = "markers",
      marker = list(size = 15, color = sn_method_colors[["db"]],
                    line = list(color = "white", width = 2)),
      text = paste0(
        "<b>DB</b><br>",
        "Level: ", agg_db$level_label, "<br>",
        "Headcount: ", round(agg_db$headcount_pct, 1), "%"
      ),
      hovertemplate = "%{text}<extra></extra>",
      name = "DB method", legendgroup = "db"
    )

  # Delta annotations
  for (i in seq_len(nrow(agg_wide))) {
    row <- agg_wide[i, ]
    p <- p |>
      plotly::add_annotations(
        x = row$x_num,
        y = row$mid_y,
        text = paste0("<b>", row$delta_lbl, "</b>"),
        showarrow = FALSE,
        xshift = 45,
        font = list(size   = 11,
                    color  = "#363636",
                    family = "sans-serif")
      )
  }

  p |>
    plotly::layout(
      title = list(
        text = paste0(
          "<b>Method Comparison \u2014 ", country_name, "</b><br>",
          "<span style='font-size:12px; font-weight:normal;'>",
          "Difference in headcount at ", selected_poverty_line,
          " poverty line between DB and DOU methods</span>"
        ),
        font = list(size = 16, family = "sans-serif"),
        x = 0
      ),
      xaxis = list(
        title     = "",
        tickvals  = 1:3,
        ticktext  = c("<b>Urban</b>", "<b>National</b>", "<b>Rural</b>"),
        tickfont  = list(size = 13, family = "sans-serif"),
        zeroline  = FALSE,
        gridcolor = "rgba(200,200,200,0.3)"
      ),
      yaxis = list(
        title      = "<b>Headcount (%)</b>",
        titlefont  = list(size = 13),
        ticksuffix = "%",
        tickfont   = list(size = 12, family = "sans-serif"),
        zeroline   = FALSE,
        gridcolor  = "rgba(200,200,200,0.3)"
      ),
      legend = list(
        orientation = "h",
        font = list(size = 12),
        x = 0.5, y = -0.12,
        xanchor = "center", yanchor = "top"
      ),
      hovermode     = "closest",
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin        = list(l = 80, r = 60, t = 100, b = 80)
    )
}


# ============================================================================
# Chart (ii): Dodged range bars - spread + method comparison
# ============================================================================
#' @param data      Preprocessed d_sn
#' @param country_code ISO3 code (used for filtering)
#' @param country_name Display name for titles (falls back to country_code)
#' @param selected_poverty_line Poverty line string
#' @noRd
plot_sn_range_bars <- function(data,
                               country_code,
                               country_name = NULL,
                               selected_poverty_line = "$2.15") {

  if (is.null(country_name)) country_name <- country_code

  prep <- prep_fan_data(data, country_code, selected_poverty_line)
  fan  <- prep$fan
  agg  <- prep$aggregate
  pts  <- prep$points

  dodge <- 0.12
  method_offset <- c("dou" = -dodge, "db" = dodge)

  p <- plotly::plot_ly()

  for (m in c("dou", "db")) {

    col     <- sn_method_colors[[m]]
    m_label <- if (m == "db") "DB" else "DOU"
    off     <- method_offset[[m]]

    fan_m <- fan |> dplyr::filter(method == m) |> dplyr::arrange(x_num)
    agg_m <- agg |> dplyr::filter(method == m) |> dplyr::arrange(x_num)
    pts_m <- pts |> dplyr::filter(method == m)

    # Range bars (vertical segments from min to max)
    for (i in seq_len(nrow(fan_m))) {
      row <- fan_m[i, ]
      if (row$y_max > row$y_min) {
        p <- p |>
          plotly::add_segments(
            x = row$x_num + off, xend = row$x_num + off,
            y = row$y_min, yend = row$y_max,
            line = list(color = col, width = 5),
            showlegend = FALSE, hoverinfo = "skip",
            legendgroup = m
          )
      }
    }

    # Sub-level points (filled, lighter, with per-point above/below labels)
    sub_pts <- pts_m |> dplyr::filter(sub_level != "")
    if (nrow(sub_pts) > 0) {
      sub_fill <- if (m == "db") "rgba(255,152,0,0.4)" else "rgba(78,194,192,0.4)"

      p <- p |>
        plotly::add_trace(
          x = sub_pts$x_num + off, y = sub_pts$headcount_pct,
          type = "scatter", mode = "markers",
          marker = list(
            size = 9, color = sub_fill,
            line = list(color = col, width = 1.5)
          ),
          hovertext = paste0(
            "<b>", m_label, "</b><br>",
            "Sub-level: ", sub_pts$level_label, "<br>",
            "Headcount: ", round(sub_pts$headcount_pct, 1), "%"
          ),
          hovertemplate = "%{hovertext}<extra></extra>",
          showlegend = FALSE, legendgroup = m
        )

      # Per-point annotations: above if max of its level, below if min
      for (i in seq_len(nrow(sub_pts))) {
        pt      <- sub_pts[i, ]
        lvl_pts <- pts_m |> dplyr::filter(reporting_level == pt$reporting_level)
        is_max  <- pt$headcount_pct == max(lvl_pts$headcount_pct)
        yshift  <- if (is_max) 12 else -12

        p <- p |>
          plotly::add_annotations(
            x = pt$x_num + off, y = pt$headcount_pct,
            text = pt$level_label,
            showarrow = FALSE,
            yshift = yshift,
            font = list(size = 9, color = "#666666", family = "sans-serif"),
            xanchor = "center"
          )
      }
    }

    # Aggregate points (filled, larger - focal point)
    p <- p |>
      plotly::add_trace(
        x = agg_m$x_num + off, y = agg_m$headcount_pct,
        type = "scatter", mode = "markers",
        marker = list(
          size = 14, color = col,
          line = list(color = "white", width = 2)
        ),
        text = paste0(
          "<b>", m_label, "</b><br>",
          "Level: ", agg_m$level_label, "<br>",
          "Headcount: ", round(agg_m$headcount_pct, 1), "%"
        ),
        hovertemplate = "%{text}<extra></extra>",
        name = paste0(m_label, " method"),
        legendgroup = m, showlegend = TRUE
      )
  }

  # Connecting lines between comparable aggregates across methods + delta
  for (lvl in c("urban", "national", "rural")) {
    agg_dou <- agg |> dplyr::filter(method == "dou", reporting_level == lvl)
    agg_db  <- agg |> dplyr::filter(method == "db",  reporting_level == lvl)
    if (nrow(agg_dou) == 0 || nrow(agg_db) == 0) next

    x_dou <- agg_dou$x_num + method_offset[["dou"]]
    x_db  <- agg_db$x_num  + method_offset[["db"]]

    p <- p |>
      plotly::add_segments(
        x = x_dou, xend = x_db,
        y = agg_dou$headcount_pct, yend = agg_db$headcount_pct,
        line = list(color = "#363636", width = 1.5),
        showlegend = FALSE, hoverinfo = "skip"
      )

    delta <- agg_db$headcount_pct - agg_dou$headcount_pct
    delta_lbl <- paste0(dplyr::if_else(delta >= 0, "+", ""),
                        round(delta, 1), " pp")
    mid_y <- (agg_dou$headcount_pct + agg_db$headcount_pct) / 2

    p <- p |>
      plotly::add_annotations(
        x = x_db, y = mid_y,
        text = paste0("<b>", delta_lbl, "</b>"),
        showarrow = FALSE,
        xshift = 30,
        font = list(size = 10, color = "#363636", family = "sans-serif")
      )
  }

  p |>
    plotly::layout(
      title = list(
        text = paste0(
          "<b>Estimate Spread by Method \u2014 ", country_name, "</b><br>",
          "<span style='font-size:12px; font-weight:normal;'>",
          "Range of headcount estimates at ", selected_poverty_line,
          " poverty line within each reporting level</span>"
        ),
        font = list(size = 16, family = "sans-serif"),
        x = 0
      ),
      xaxis = list(
        title     = "",
        tickvals  = 1:3,
        ticktext  = c("<b>Urban</b>", "<b>National</b>", "<b>Rural</b>"),
        tickfont  = list(size = 13, family = "sans-serif"),
        zeroline  = FALSE,
        gridcolor = "rgba(200,200,200,0.3)"
      ),
      yaxis = list(
        title      = "<b>Headcount (%)</b>",
        titlefont  = list(size = 13),
        ticksuffix = "%",
        tickfont   = list(size = 12, family = "sans-serif"),
        zeroline   = FALSE,
        gridcolor  = "rgba(200,200,200,0.3)"
      ),
      legend = list(
        orientation = "h",
        font = list(size = 12),
        x = 0.5, y = -0.12,
        xanchor = "center", yanchor = "top"
      ),
      hovermode     = "closest",
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin        = list(l = 110, r = 80, t = 100, b = 80)
    )
}
