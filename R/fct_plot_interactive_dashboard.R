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
  poverty_levels <- c("$2.15",
                      "$3.65",
                      "$6.85")

  poverty_line_positions <- setNames(1:3,
                                     poverty_levels)

  select_year <- fifelse(select_method == "Welfare conversion",
                         2022,
                         2019)
  sltmethod <- tolower(select_method)
  # Prepare tidy data
  dt_country <- data |>
    fsubset(year == select_year) |>
    fsubset(country_name == select_country) |>
    tidyr::pivot_longer(cols      = c(headcount_default, headcount_estimate),
                        names_to  = "method",
                        values_to = "headcount") |>
    fmutate(
      method = dplyr::recode(method,
                             headcount_default = "Default",
                             headcount_estimate = "New"),
      poverty_line = factor(poverty_line, levels = poverty_levels),
      x            = poverty_line_positions[as.character(poverty_line)],
      label        = paste0(round(headcount, 1), "%"))

  # Prepare arrow data
  dt_arrows <- dt_country |>
    fselect(x,
            poverty_line,
            method,
            headcount) |>
    tidyr::pivot_wider(names_from  = method,
                       values_from = headcount) |>
    fmutate(x = x + 0.06)

  # Create plot
  plot <- ggplot(dt_country,
                 aes(x     = x,
                     y     = headcount,
                     group = method,
                     color = method)) +
    geom_segment(data = dt_arrows,
                 aes(x    = x,
                     xend = x,
                     y    = Default,
                     yend = New),
                 inherit.aes = FALSE,
                 arrow = arrow(length = unit(0.25, "cm")),
                 color = "#5A91C1",
                 size = 1) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3.5, stroke = 0.5, shape = 21, fill = "white") +
    geom_text(aes(label = label,
                  vjust = ifelse(method == "Default", -1.2, 2)),
              size = 3.5,
              show.legend = FALSE) +
    scale_x_continuous(breaks = 1:3, labels = poverty_levels) +
    scale_y_continuous(labels = label_number(suffix = "%"),
                       #limits = c(0, 105),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual(values = c("Default" = "grey40", "New" = "#0070BB"),
                       labels = c("Default methodology", "New methodology")) +
    labs(
      title = glue(
        "Poverty Headcount in <b style='color:#2c7fb8'>{select_country}</b> by <span style='color:#2c7fb8;'>New</span> vs <span style='color:grey40;'>Default</span> Methodology"
      ),
      subtitle = glue("New methodology uses {tolower(select_method)} and is compared to default approach in PIP, all in 2017 $PPP."),
      x = "Poverty Line",
      y = "Poverty Headcount (%)",
      color = "Methodology",
      caption = glue("Values are rounded percentages. Data supplied by authors.")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x  = element_blank(),
      axis.title.x        = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y        = element_text(face = "bold", margin = margin(r = 10)),
      plot.title          = element_markdown(size = 15, face = "bold"),
      plot.subtitle       = element_text(size = 11),
      plot.caption        = element_text(size = 9, color = "grey40"),
      legend.position     = "bottom",
      plot.title.position = "plot"
    ) +
    guides(color = guide_legend(override.aes = list(size = 4)))

  return(plot)

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


#' Plot rankings (placeholder)
#'
#' @description Placeholder function for rankings plot
#'
#' @param data A data.table or data.frame containing poverty estimates
#'
#' @return A ggplot object
#'
#' @noRd
plot_rankings <- function(data) {
  # Simple placeholder plot
  ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_text(label = "Rankings plot\nComing soon...", size = 8) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Rankings Plot")
}











