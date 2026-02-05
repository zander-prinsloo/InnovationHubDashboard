#' plot_interactive_dashboard
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plot_single_country <- function(data,
                                select_country,
                                select_method) {
  # Define poverty line levels
  poverty_levels <- c("$2.15", "$3.65", "$6.85")
  poverty_line_positions <- setNames(1:3, poverty_levels)

  select_year <- fifelse(select_method == "Welfare conversion", 2022, 2019)
  
  # Prepare tidy data
  dt_country <- data |>
    fsubset(year == select_year) |>
    fsubset(country_name == select_country) |>
    tidyr::pivot_longer(cols = c(headcount_default, headcount_estimate),
                        names_to = "method",
                        values_to = "headcount") |>
    as.data.frame()  # Convert to data.frame to ensure compatibility
  
  # Now use dplyr for the mutations to ensure compatibility
  dt_country <- dt_country |>
    dplyr::mutate(
      method = dplyr::recode(method,
                             headcount_default = "PIP",
                             headcount_estimate = "Alternative"),
      poverty_line = factor(poverty_line, levels = poverty_levels),
      x = poverty_line_positions[as.character(poverty_line)],
      label = paste0(round(headcount, 1), "%"),
      tooltip_text = paste0(
        "<b>", method, " methodology</b><br>",
        "Poverty line: ", poverty_line, "<br>",
        "Headcount: ", round(headcount, 1), "%"
      )
    )

  # Prepare arrow data - pivot wider FIRST, then mutate
  dt_arrows <- dt_country |>
    dplyr::select(x, poverty_line, method, headcount) |>
    tidyr::pivot_wider(names_from = method, values_from = headcount) |>
    as.data.frame()
  
  # Only process arrows if we have data
  if (nrow(dt_arrows) > 0) {
    # Ensure PIP and Alternative are numeric vectors
    dt_arrows$PIP <- as.numeric(dt_arrows$PIP)
    dt_arrows$Alternative <- as.numeric(dt_arrows$Alternative)
    
    # Now add derived columns after the pivot using base R/dplyr
    dt_arrows$x_pos <- dt_arrows$x + 0.06
    dt_arrows$diff <- dt_arrows$Alternative - dt_arrows$PIP
    dt_arrows$tooltip_arrow <- paste0(
      "<b>", as.character(dt_arrows$poverty_line), "</b><br>",
      "PIP: ", round(dt_arrows$PIP, 1), "%<br>",
      "Alternative: ", round(dt_arrows$Alternative, 1), "%<br>",
      "Difference: ", ifelse(dt_arrows$diff >= 0, "+", ""), round(dt_arrows$diff, 1), " pp"
    )
  }
  
  # Determine y-axis range with padding
  # Make sure headcount column exists and has values
  if (!"headcount" %in% names(dt_country) || all(is.na(dt_country$headcount))) {
    # Fallback to default range if no data
    y_limits <- c(-5, 100)
  } else {
    y_min <- min(dt_country$headcount, na.rm = TRUE)
    y_max <- max(dt_country$headcount, na.rm = TRUE)
    
    # Add extra space above for the difference labels (approximately 3-4 units)
    # and ensure bottom has space for markers at 0%
    y_range_calc <- y_max - y_min
    y_padding_bottom <- max(3, y_range_calc * 0.12)  # At least 3% padding at bottom for full markers
    y_padding_top <- max(5, y_range_calc * 0.15)     # At least 5% padding at top for labels
    
    y_limits <- c(max(0, y_min - y_padding_bottom), y_max + y_padding_top)
    
  }
  
  y_limits[1] <- y_limits[1] - 0.5
  # ─── Build plotly figure ────────────────────────────────────────────────
  
  pp <- plotly::plot_ly()
  
  # Add arrows first (as segments with annotations)
  for (i in seq_len(nrow(dt_arrows))) {
    arrow_row <- dt_arrows[i, ]
    
    pp <- pp |>
      plotly::add_segments(
        x = arrow_row$x_pos,
        xend = arrow_row$x_pos,
        y = arrow_row$PIP,
        yend = arrow_row$Alternative,
        line = list(color = "#5A91C1", width = 2),
        showlegend = FALSE,
        hoverinfo = "text",
        text = arrow_row$tooltip_arrow
      ) |>
      plotly::add_annotations(
        x = arrow_row$x_pos,
        y = arrow_row$Alternative,
        ax = arrow_row$x_pos,
        ay = arrow_row$PIP,
        xref = "x", yref = "y",
        axref = "x", ayref = "y",
        showarrow = TRUE,
        arrowhead = 2,
        arrowsize = 1,
        arrowwidth = 2,
        arrowcolor = "#5A91C1",
        text = ""
      )
  }
  
  # Add lines and points by method
  methods <- c("PIP", "Alternative")
  method_colors <- c("PIP" = "grey40", "Alternative" = "#0070BB")
  
  for (meth in methods) {
    dt_method <- dt_country |>
      dplyr::filter(method == meth)
    
    # Add line
    pp <- pp |>
      plotly::add_trace(
        data = dt_method,
        x = ~x,
        y = ~headcount,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = method_colors[meth], width = 2.5),
        marker = list(
          size = 10,
          color = method_colors[meth],
          line = list(color = "white", width = 1.5)
        ),
        text = ~tooltip_text,
        hovertemplate = "%{text}<extra></extra>",
        name = paste0(meth, " methodology"),
        legendgroup = meth,
        showlegend = TRUE
      )
  }
  
  # Add difference labels above the higher point
  if (nrow(dt_arrows) > 0) {
    for (i in seq_len(nrow(dt_arrows))) {
      arrow_row <- dt_arrows[i, ]
      # Place label slightly above the higher of the two points
      label_y <- max(arrow_row$PIP, arrow_row$Alternative) + 1.5
      
      pp <- pp |>
        plotly::add_annotations(
          x = arrow_row$x_pos,
          y = label_y,
          text = paste0(ifelse(arrow_row$diff >= 0, "+", ""), round(arrow_row$diff, 1), " pp"),
          showarrow = FALSE,
          font = list(size = 10, color = "#5A91C1")
        )
    }
  }
  
  # ─── Layout configuration ────────────────────────────────────────────
  
  title_text <- paste0(
    "<b>Poverty rates using different approaches to ",
    tolower(select_method),
    "</b>"
  )
  
  subtitle_text <- paste0(
    select_country, " in ", select_year, " using 2017 $PPP"
  )
  
  pp <- pp |>
    plotly::layout(
      title = list(
        text = paste0(title_text, "<br><span style='font-size:11px; font-weight:normal;'>", 
                      subtitle_text, "</span>"),
        font = list(size = 15)
      ),
      xaxis = list(
        title = "<b>Poverty Line</b>",
        tickmode = "array",
        tickvals = 1:3,
        ticktext = poverty_levels,
        range = c(0.5, 3.5),
        gridcolor = "rgba(200,200,200,0.3)",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "<b>Poverty Headcount (%)</b>",
        ticksuffix = "%",
        range = y_limits,
        gridcolor = "rgba(200,200,200,0.3)",
        zeroline = FALSE
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.15,
        xanchor = "center",
        yanchor = "top"
      ),
      hovermode = "closest",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 80, t = 100, b = 100)
    )
  
  return(pp)
}





prep_changes <- function(data, select_country = NULL) {

  select_code <- data |>
    filter(country_name == select_country)
  select_code <- select_code$code |>
    unique()
  dt_db <- data |>
    select(country_name,
           code,
           poverty_line,
           region_code,
           headcount_default,
           headcount_estimate) |>
    mutate(poverty_line = factor(poverty_line,
                                 levels = c("$2.15",
                                            "$3.65",
                                            "$6.85"))) |>
    mutate(diff = round(abs(headcount_default - headcount_estimate),
                        1))
  countries_keep <-
    dt_db |>
    group_by(code) |>
    summarise(mean_diff = mean(diff)) |>
    ungroup() |>
    arrange(desc(mean_diff)) |>
    slice_max(order_by = mean_diff,
              n        = 25) |>
    ungroup()
  countries_keep <- countries_keep$code |>
    unique()
  if (is.null(select_country)) {
    countries_keep <- c(countries_keep)
  } else {
    countries_keep <- c(countries_keep, select_code)
  }
  dt_db <- dt_db |>
    fsubset(code %in% countries_keep)

  # Order countries within each facet
  dt_db <- dt_db |>
    group_by(poverty_line) |>
    mutate(country_name = fct_reorder(country_name,
                                      headcount_default)) |>
    ungroup() |>
    group_by(code, poverty_line) |>
    mutate(minpov = min(headcount_default,
                        headcount_estimate)) |>
    arrange(desc(minpov)) |>
    mutate(highlight = ifelse(code == select_code, T, F))

  dt_db
}

plot_changes <- function(data,
                         select_country = NULL,
                         select_method  = c("Household allocation")) {

  select_method <- tolower(select_method)
  data <- prep_changes(data, select_country)
  
  # Define region colors
  region_colors <- c(
    "OHI" = "#34A7F2",
    "SSA" = "#FF9800",
    "MNA" = "#664AB6",
    "SAS" = "#4EC2C0",
    "EAS" = "#F3578E",
    "LAC" = "#0C7C68",
    "ECA" = "#AA0000",
    "WLD" = "#081079"
  )

  # Make plot
  plot <- ggplot(data,
                 aes(y = country_name)) +
    geom_dumbbell(
      aes(x      = headcount_default,
          xend   = headcount_estimate,
          colour = region_code),
      size        = 1.2,
      size_x      = 3,
      size_xend   = 3,
      colour_x    = "black",
      colour_xend = "steelblue"
    ) +
    scale_colour_manual(
      name = "Region",
      values = region_colors
    )
  if (is.null(select_country)) {
    plot <- plot +
      geom_text(
        aes(x     = pmax(headcount_default,
                         headcount_estimate) + 1.5,
            label = paste0(diff, " pp")),
        size  = 3,
        hjust = 0,
        color = "gray30"
      )
  } else {
    plot <- plot +
      # grey labels for all the non–highlighted countries
      geom_text(
        data = subset(data, !highlight),
        aes(x     = pmax(headcount_default,
                         headcount_estimate) + 1.5,
            label = paste0(diff, " pp")),
        size  = 3,
        hjust = 0,
        color = "gray30"
      ) +

      # big red label for the one highlighted country
      geom_text(
        data = subset(data, highlight),
        aes(x     = pmax(headcount_default,
                         headcount_estimate) + 1.5,
            label = paste0(diff, " pp")),
        size  = 5,      # larger
        hjust = 0,
        color = "red"   # red
      )
  }

  plot <- plot +
    facet_grid(. ~ poverty_line) +
    scale_x_continuous(
      labels = label_percent(scale = 1),
      expand = expansion(mult = c(0, 0.25))
    ) +
    labs(
      title    = glue("Absolute difference in poverty rate at three poverty lines when changing the {select_method} "),
      subtitle = glue("Top 25 countries with the biggest difference, highlighting {select_country}"),
      x        = "Poverty rate (%)",
      y        = NULL,
      caption  = "Values are rounded percentages. Data supplied by authors."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.spacing      = unit(1.5,
                                "lines"),
      strip.text.x       = element_text(size = 11,
                                        face = "bold"),
      plot.title         = element_text(size = 16,
                                        face = "bold"),
      plot.subtitle      = element_text(size = 12),
      axis.text.y        = element_text(size = 9),
      axis.title.x       = element_text(face = "bold"),
      legend.position    = "right",
      legend.title       = element_text(face = "bold", size = 11),
      legend.text        = element_text(size = 10),
      plot.caption       = element_text(size  = 9,
                                        color = "grey30"))

  plot

}


#' Create scatter plot comparing default and alternative poverty headcount estimates
#'
#' @description Produces a plotly scatter plot showing the relationship between
#'   default and alternative poverty headcount estimates across countries and
#'   poverty lines. The identity line (y=x) helps identify perfect agreement,
#'   while tolerance bands (±3 pp) highlight meaningful divergence.
#'
#' @param data A data.table or data.frame containing poverty estimates
#' @param select_year Numeric year to filter data. If NULL, uses most common year
#' @param log_x Logical, whether to use log scale for x-axis
#' @param log_y Logical, whether to use log scale for y-axis
#' @param poverty_line_filter Character, either "all" or specific poverty line ("$2.15", "$3.65", "$6.85")
#'
#' @return A plotly object with interactive scatter plot
#'
#' @noRd
plot_scatter <- function(data, select_year = NULL, log_x = FALSE, log_y = FALSE, poverty_line_filter = "all") {
  
  # ─── 1) Data preparation ────────────────────────────────────────────────────
  
  # Filter to specified year, or use most common year if not specified
  if (is.null(select_year)) {
    # Determine the most common year in the dataset
    year_counts <- data |>
      collapse::fcount(year)
    # Order by count descending and get most common year
    select_year <- year_counts$year[which.max(year_counts$N)]
  }
  
  # Filter data to selected year and remove any missing values
  dt_plot <- data |>
    collapse::fsubset(year == select_year) |>
    collapse::fsubset(!is.na(headcount_default) & !is.na(headcount_estimate))
  
  # Filter by poverty line if specified
  if (poverty_line_filter != "all") {
    dt_plot <- dt_plot |>
      collapse::fsubset(poverty_line == poverty_line_filter)
  }
  
  # Ensure poverty_line is an ordered factor with consistent levels
  poverty_levels <- c("$2.15", "$3.65", "$6.85")
  dt_plot <- dt_plot |>
    collapse::fmutate(
      poverty_line = factor(poverty_line, 
                           levels = poverty_levels,
                           ordered = TRUE)
    )
  
  # Standardize region column name if needed
  if (!"region_name" %in% names(dt_plot) && "region_code" %in% names(dt_plot)) {
    dt_plot <- dt_plot |>
      collapse::fmutate(region_name = region_code)
  } else if (!"region_name" %in% names(dt_plot)) {
    dt_plot <- dt_plot |>
      collapse::fmutate(region_name = "Unknown")
  }
  
  # Calculate difference (alternative - default) in percentage points
  dt_plot <- dt_plot |>
    collapse::fmutate(
      diff_pp = round(headcount_estimate - headcount_default, 1)
    )
  
  # Create custom tooltip text
  dt_plot <- dt_plot |>
    collapse::fmutate(
      text_tooltip = paste0(
        "<b>", country_name, "</b> (", code, ")<br>",
        "Region: ", region_name, "<br>",
        "Year: ", year, "<br>",
        "Poverty line: ", poverty_line, "<br>",
        "Default: ", round(headcount_default, 1), "%<br>",
        "Alternative: ", round(headcount_estimate, 1), "%<br>",
        "Difference: ", ifelse(diff_pp >= 0, "+", ""), diff_pp, " pp"
      )
    )
  
  # ─── 2) Define plot boundaries ─────────────────────────────────────────────
  
  # Set axis limits to 0-100%
  axis_min <- 0
  axis_max <- 100
  
  # ─── 3) Color and shape mappings ───────────────────────────────────────────
  
  # Define color palette for regions
  region_colors <- c(
    "OHI" = "#34A7F2",  # Other High Income
    "SSA" = "#FF9800",  # Sub-Saharan Africa
    "MNA" = "#664AB6",  # Middle East & North Africa
    "SAS" = "#4EC2C0",  # South Asia
    "EAS" = "#F3578E",  # East Asia & Pacific
    "LAC" = "#0C7C68",  # Latin America & Caribbean
    "ECA" = "#AA0000",  # Europe & Central Asia
    "WLD" = "#081079"   # World
  )
  
  # Define shape symbols for poverty lines (plotly symbol codes)
  poverty_shapes <- c(
    "$2.15" = "circle",
    "$3.65" = "triangle-up",
    "$6.85" = "square"
  )
  
  # ─── 4) Build plotly scatter plot directly ────────────────────────────────
  
  # Initialize empty plotly figure
  pp <- plotly::plot_ly()
  
  # Add identity line (y = x) in black
  pp <- pp |>
    plotly::add_segments(
      x = axis_min, xend = axis_max,
      y = axis_min, yend = axis_max,
      line = list(color = "black", width = 2),
      showlegend = FALSE,
      hoverinfo = "skip",
      name = "Identity line"
    )
  
  # Add tolerance bands (y = x ± 3) in grey, dashed
  pp <- pp |>
    plotly::add_segments(
      x = axis_min, xend = axis_max,
      y = axis_min + 3, yend = axis_max + 3,
      line = list(color = "grey", width = 1, dash = "dash"),
      showlegend = FALSE,
      hoverinfo = "skip",
      opacity = 0.5
    ) |>
    plotly::add_segments(
      x = axis_min, xend = axis_max,
      y = axis_min - 3, yend = axis_max - 3,
      line = list(color = "grey", width = 1, dash = "dash"),
      showlegend = FALSE,
      hoverinfo = "skip",
      opacity = 0.5
    )
  
  # Add scatter points by region and poverty line
  regions <- unique(dt_plot$region_name)
  poverty_lines <- poverty_levels
  
  first_region <- TRUE
  for (region in regions) {
    for (pline in poverty_lines) {
      # Filter data for this combination
      dt_subset <- dt_plot |>
        collapse::fsubset(region_name == region & poverty_line == pline)
      
      if (nrow(dt_subset) > 0) {
        # Determine color and shape
        color <- region_colors[region]
        if (is.na(color)) color <- "#7f7f7f"
        shape <- poverty_shapes[pline]
        
        # Add trace
        pp <- pp |>
          plotly::add_trace(
            data = dt_subset,
            x = ~headcount_default,
            y = ~headcount_estimate,
            type = "scatter",
            mode = "markers",
            marker = list(
              size = 8,
              color = color,
              symbol = shape,
              line = list(color = "white", width = 0.5),
              opacity = 0.7
            ),
            text = ~text_tooltip,
            hovertemplate = "%{text}<extra></extra>",
            legendgroup = region,
            legendgrouptitle = if (first_region && pline == poverty_lines[1]) {
              list(text = "<b>Region</b>")
            } else {
              list(text = "")
            },
            name = region,
            showlegend = (pline == poverty_lines[1])  # Show legend only for first poverty line
          )
        
        if (pline == poverty_lines[1]) first_region <- FALSE
      }
    }
  }
  
  # Add separate traces for poverty line shapes (for shape legend)
  # Use invisible points outside plot range to show in legend only
  for (i in seq_along(poverty_lines)) {
    pline <- poverty_lines[i]
    pp <- pp |>
      plotly::add_trace(
        x = c(-999),  # Outside visible range
        y = c(-999),  # Outside visible range
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 10,
          color = "rgba(100, 100, 100, 1)",
          symbol = poverty_shapes[pline],
          line = list(color = "rgba(255, 255, 255, 0.8)", width = 1)
        ),
        legendgroup = "poverty_lines",
        legendgrouptitle = if (i == 1) list(text = "<b>Poverty Line</b>") else list(text = ""),
        name = pline,
        showlegend = TRUE,
        hoverinfo = "none",
        visible = TRUE
      )
  }
  
  # ─── 5) Configure layout ───────────────────────────────────────────────────
  
  # Configure axis settings based on log scale options
  xaxis_config <- list(
    title = "<b>Default Methodology Headcount (%)</b>",
    ticksuffix = "%",
    scaleanchor = "y",
    scaleratio = 1,
    gridcolor = "rgba(200,200,200,0.3)",
    zeroline = FALSE,
    fixedrange = FALSE,
    constrain = "domain"
  )
  
  yaxis_config <- list(
    title = "<b>Alternative Methodology Headcount (%)</b>",
    ticksuffix = "%",
    gridcolor = "rgba(200,200,200,0.3)",
    zeroline = FALSE,
    fixedrange = FALSE,
    constrain = "domain"
  )
  
  # Set axis type and range based on log scale settings
  if (log_x) {
    xaxis_config$type <- "log"
    xaxis_config$range <- c(log10(0.1), log10(105))  # 0.1% to 105% on log scale
  } else {
    xaxis_config$type <- "linear"
    xaxis_config$range <- c(-5, 105)
    xaxis_config$autorange <- FALSE
  }
  
  if (log_y) {
    yaxis_config$type <- "log"
    yaxis_config$range <- c(log10(0.1), log10(105))  # 0.1% to 105% on log scale
  } else {
    yaxis_config$type <- "linear"
    yaxis_config$range <- c(-5, 105)
    yaxis_config$autorange <- FALSE
  }
  
  pp <- pp |>
    plotly::layout(
      title = list(
        text = "<b>Default vs Alternative Poverty Headcount Estimates</b>",
        font = list(size = 15)
      ),
      xaxis = xaxis_config,
      yaxis = yaxis_config,
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 1,
        xanchor = "left",
        yanchor = "top"
      ),
      hovermode = "closest",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 150, t = 100, b = 80)
    )
  
  # Return plotly object
  return(pp)
}


#' Plot rankings (Bland-Altman diagnostic)
#'
#' @description Creates a Bland-Altman diagnostic plot comparing two poverty 
#'   headcount methodologies. Shows signed differences vs mean headcount across
#'   three poverty lines, with bias lines, limits of agreement, and smooth trends.
#'
#' @param data A data.table or data.frame containing poverty estimates with columns:
#'   - country_name, code (country identifiers)
#'   - region_code, region_name (regional grouping)
#'   - year (survey year)
#'   - poverty_line (one of "$2.15", "$3.65", "$6.85")
#'   - headcount_default (PIP methodology, in percent 0-100)
#'   - headcount_estimate (Alternative methodology, in percent 0-100)
#' @param select_country Character, optional country name to highlight
#' @param poverty_line_filter Character, either "all" or specific poverty line
#'
#' @return A plotly object
#'
#' @details
#' The Bland-Altman plot is a diagnostic tool that separates:
#' - Average bias: whether Alternative is systematically above/below PIP
#' - Scale effects: whether disagreement grows with poverty level
#' - Outliers: countries where differences are unusually large
#' - Line-specific behavior: whether bias differs by poverty line
#'
#' Each point represents a country at a specific poverty line.
#' - Y-axis: Signed difference (Alternative - PIP) in percentage points
#' - X-axis: Mean headcount across both methods (%)
#' - Colors: Region
#' - Reference lines: Bias (mean difference) and Limits of Agreement (mean ± 1.96*SD)
#'
#' @noRd
plot_rankings <- function(data, select_country = NULL, poverty_line_filter = "all") {
  
  # ─── 1) Data preparation ────────────────────────────────────────────────────────────
  
  # Filter to complete cases only
  dt_plot <- data |>
    collapse::fsubset(!is.na(headcount_default) & !is.na(headcount_estimate))
  
  # Filter by poverty line if specified
  if (poverty_line_filter != "all") {
    dt_plot <- dt_plot |>
      collapse::fsubset(poverty_line == poverty_line_filter)
  }
  
  if (nrow(dt_plot) == 0) {
    return(
      plotly::plot_ly() |>
        plotly::add_annotations(
          text = "No data available",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 20)
        ) |>
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }
  
  # Compute derived variables (assuming data is in percent 0-100)
  dt_plot <- dt_plot |>
    collapse::fmutate(
      mean_headcount = (headcount_default + headcount_estimate) / 2,
      diff_pp = headcount_default - headcount_estimate,  # PIP minus Alternative
      abs_diff_pp = abs(diff_pp)
    )
  
  # Ensure poverty_line is a factor with correct order
  poverty_levels <- c("$2.15", "$3.65", "$6.85")
  dt_plot <- dt_plot |>
    collapse::fmutate(
      poverty_line = factor(poverty_line, levels = poverty_levels, ordered = TRUE)
    )
  
  # Standardize region column if needed
  if (!"region_name" %in% names(dt_plot) && "region_code" %in% names(dt_plot)) {
    dt_plot <- dt_plot |>
      collapse::fmutate(region_name = region_code)
  }
  
  # Mark selected country for highlighting
  if (!is.null(select_country)) {
    dt_plot <- dt_plot |>
      collapse::fmutate(
        is_selected = country_name == select_country
      )
  } else {
    dt_plot <- dt_plot |>
      collapse::fmutate(is_selected = FALSE)
  }
  
  # Create tooltip text
  dt_plot <- dt_plot |>
    collapse::fmutate(
      tooltip_text = paste0(
        "<b>", country_name, "</b> (", code, ")<br>",
        "Region: ", region_name, "<br>",
        "Poverty line: ", poverty_line, "<br>",
        "PIP headcount: ", sprintf("%.1f%%", headcount_default), "<br>",
        "Alternative headcount: ", sprintf("%.1f%%", headcount_estimate), "<br>",
        "Mean headcount: ", sprintf("%.1f%%", mean_headcount), "<br>",
        "Difference (PIP - Alt): ", sprintf("%.1f pp", diff_pp), "<br>",
        "Absolute difference: ", sprintf("%.1f pp", abs_diff_pp)
      )
    )
  
  # ─── 2) Compute statistics ────────────────────────────────────────────────────────────
  
  # These are computed but will be displayed in the sidebar, not on plot
  bias <- mean(dt_plot$diff_pp, na.rm = TRUE)
  sd_diff <- sd(dt_plot$diff_pp, na.rm = TRUE)
  loa_lower <- bias - 1.96 * sd_diff
  loa_upper <- bias + 1.96 * sd_diff
  
  # ─── 3) Determine axis limits ─────────────────────────────────────────────────────────────────
  
  # Y-axis: responsive to actual data range (not symmetric)
  y_min_data <- min(dt_plot$diff_pp, na.rm = TRUE)
  y_max_data <- max(dt_plot$diff_pp, na.rm = TRUE)
  
  # Add padding (about 10% on each side, minimum 2pp)
  y_range <- y_max_data - y_min_data
  y_padding <- max(2, y_range * 0.1)
  
  y_limits <- c(y_min_data - y_padding, y_max_data + y_padding)
  
  # X-axis: dynamic based on data, with some padding
  x_min <- max(0, min(dt_plot$mean_headcount, na.rm = TRUE) - 5)
  x_max <- min(100, max(dt_plot$mean_headcount, na.rm = TRUE) + 5)
  x_range <- c(x_min, x_max)
  
  # ─── 4) Color palette ────────────────────────────────────────────────────────────
  
  region_colors <- c(
    "OHI" = "#34A7F2",
    "SSA" = "#FF9800",
    "MNA" = "#664AB6",
    "SAS" = "#4EC2C0",
    "EAS" = "#F3578E",
    "LAC" = "#0C7C68",
    "ECA" = "#AA0000",
    "WLD" = "#081079"
  )
  
  # ─── 5) Build plotly figure ────────────────────────────────────────────────────────────
  
  pp <- plotly::plot_ly()
  
  # Add reference line at y = 0
  pp <- pp |>
    plotly::add_segments(
      x = 0, xend = 100,
      y = 0, yend = 0,
      line = list(color = "black", width = 2),
      showlegend = FALSE,
      hoverinfo = "skip"
    )
  
  # Add bias line
  pp <- pp |>
    plotly::add_segments(
      x = 0, xend = 100,
      y = bias, yend = bias,
      line = list(color = "#0070BB", width = 2),
      showlegend = FALSE,
      hoverinfo = "skip"
    )
  
  # Add LoA lines
  pp <- pp |>
    plotly::add_segments(
      x = 0, xend = 100,
      y = loa_upper, yend = loa_upper,
      line = list(color = "#0070BB", width = 1.5, dash = "dash"),
      showlegend = FALSE,
      hoverinfo = "skip"
    ) |>
    plotly::add_segments(
      x = 0, xend = 100,
      y = loa_lower, yend = loa_lower,
      line = list(color = "#0070BB", width = 1.5, dash = "dash"),
      showlegend = FALSE,
      hoverinfo = "skip"
    )
  
  # Add text annotations for the reference lines (on right side)
  pp <- pp |>
    plotly::add_annotations(
      x = x_max - 2,
      y = bias,
      text = sprintf("Mean diff: %.1f pp", bias),
      showarrow = FALSE,
      xanchor = "right",
      yanchor = "bottom",
      font = list(size = 10, color = "#0070BB")
    ) |>
    plotly::add_annotations(
      x = x_max - 2,
      y = loa_upper,
      text = sprintf("Upper LoA: %.1f pp", loa_upper),
      showarrow = FALSE,
      xanchor = "right",
      yanchor = "bottom",
      font = list(size = 10, color = "#0070BB")
    ) |>
    plotly::add_annotations(
      x = x_max - 2,
      y = loa_lower,
      text = sprintf("Lower LoA: %.1f pp", loa_lower),
      showarrow = FALSE,
      xanchor = "right",
      yanchor = "bottom",
      font = list(size = 10, color = "#0070BB")
    )
  
  # Add scatter points by region
  regions <- unique(dt_plot$region_name)
  
  for (region in regions) {
    # Regular points for this region
    dt_region <- dt_plot |>
      collapse::fsubset(region_name == region & !is_selected)
    
    if (nrow(dt_region) > 0) {
      color <- region_colors[region]
      if (is.na(color)) color <- "#7f7f7f"
      
      pp <- pp |>
        plotly::add_trace(
          data = dt_region,
          x = ~mean_headcount,
          y = ~diff_pp,
          type = "scatter",
          mode = "markers",
          marker = list(
            size = 8,
            color = color,
            opacity = 0.7,
            line = list(color = "white", width = 0.5)
          ),
          text = ~tooltip_text,
          hovertemplate = "%{text}<extra></extra>",
          name = region,
          legendgroup = region,
          showlegend = TRUE
        )
    }
    
    # Highlighted points for selected country in this region
    dt_selected_region <- dt_plot |>
      collapse::fsubset(region_name == region & is_selected)
    
    if (nrow(dt_selected_region) > 0) {
      color <- region_colors[region]
      if (is.na(color)) color <- "#7f7f7f"
      
      pp <- pp |>
        plotly::add_trace(
          data = dt_selected_region,
          x = ~mean_headcount,
          y = ~diff_pp,
          type = "scatter",
          mode = "markers",
          marker = list(
            size = 14,
            color = color,
            symbol = "circle",
            line = list(color = "black", width = 2)
          ),
          text = ~tooltip_text,
          hovertemplate = "%{text}<extra></extra>",
          name = region,
          legendgroup = region,
          showlegend = FALSE
        )
    }
  }
  
  # ─── 6) Layout configuration ────────────────────────────────────────────────────────────
  
  # Create title with optional subtitle for selected country
  title_text <- "<b>Difference plot of PIP vs Alternative poverty headcount</b>"
  if (!is.null(select_country) && any(dt_plot$is_selected)) {
    title_text <- paste0(
      title_text,
      "<br><span style='font-size:12px; font-weight:normal;'>Points for ",
      select_country,
      " are shown in bold</span>"
    )
  }
  
  pp <- pp |>
    plotly::layout(
      title = list(
        text = title_text,
        font = list(size = 15)
      ),
      xaxis = list(
        title = "<b>Mean headcount across methods (%)</b>",
        range = x_range,
        gridcolor = "rgba(200,200,200,0.3)",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "<b>Difference (PIP − Alternative) (pp)</b>",
        range = y_limits,
        gridcolor = "rgba(200,200,200,0.3)",
        zeroline = FALSE
      ),
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 1,
        xanchor = "left",
        yanchor = "top",
        title = list(text = "<b>Region</b>")
      ),
      hovermode = "closest",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 150, t = 100, b = 80)
    )
  
  return(pp)
}
