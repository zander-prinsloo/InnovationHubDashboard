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
    pivot_longer(cols      = c(headcount_default, headcount_estimate),
                 names_to  = "method",
                 values_to = "headcount") |>
    fmutate(
      method = recode(method,
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

  # Make plot
  plot <- ggplot(data,
                 aes(y = country_name)) +
    geom_dumbbell(
      aes(x    = headcount_default,
          xend = headcount_estimate),
      size        = 1.2,
      size_x      = 3,
      size_xend   = 3,
      colour      = "gray70",
      colour_x    = "black",
      colour_xend = "steelblue"
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
    facet_grid(region_code ~ poverty_line,
               scales = "free_y",
               space = "free_y",
               switch = "y") +
    scale_x_continuous(
      labels = label_percent(scale = 1),
      expand = expansion(mult = c(0, 0.25))
    ) +
    labs(
      title    = glue("Absolute difference in poverty rate at three poverty lines when changing the {select_method} "),
      subtitle = glue("Top 25 countries with the biggest difference by region, highlighting {select_country}"),
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
      strip.placement    = "outside",
      strip.text.y.left  = element_text(angle = 0,
                                        face  = "bold",
                                        size  = 12,
                                        hjust = 1),
      strip.background.y = element_rect(fill  = "#f0f0f0",
                                        color = NA),
      strip.text.x       = element_text(size = 11,
                                        face = "bold"),
      plot.title         = element_text(size = 16,
                                        face = "bold"),
      plot.subtitle      = element_text(size = 12),
      axis.text.y        = element_text(size = 9),
      axis.title.x       = element_text(face = "bold"),
      legend.position    = "none",
      plot.caption       = element_text(size  = 9,
                                        color = "grey30"))

  plot

}











