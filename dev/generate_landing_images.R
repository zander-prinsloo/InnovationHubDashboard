# =============================================================================
# Generate static landing page images
# =============================================================================
# Run this script manually from the project root whenever underlying data
# changes. It creates four PNG files in inst/app/www/ used by the Home page.
#
# Images produced:
#   landing_dm.png   — DM: Welfare conversion (Argentina, single-country)
#   landing_stb.png  — STB: Household allocation (changes / dumbbell)
#   landing_sn.png   — SN: Subnational definition (range bars, Niger)
#   landing_yk.png   — YK: NA–Survey gap adjustment (Gini sensitivity)
#
# Usage:
#   source("dev/generate_landing_images.R")
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(collapse)
library(scales)
library(ggtext)
library(glue)
library(stringr)
library(forcats)

# ── Load data ----------------------------------------------------------------
load("data/d_dm.rda")
load("data/d_stb.rda")
load("data/d_sn.rda")
load("data/d_yk.rda")

# Preprocess SN data (mirrors app_server.R)
country_lookup <- data.frame(
  code = c("AGO","BFA","BGD","TCD","CIV","COL","EGY","ETH","GAB",
           "GHA","GIN","GNB","LSO","MRT","MWI","NER","SEN","TZA","UGA","VNM"),
  country_name = c("Angola","Burkina Faso","Bangladesh","Chad",
                   "Côte d'Ivoire","Colombia","Egypt","Ethiopia","Gabon",
                   "Ghana","Guinea","Guinea-Bissau","Lesotho","Mauritania",
                   "Malawi","Niger","Senegal","Tanzania","Uganda","Vietnam")
)

data_sn <- d_sn |>
  fsubset(method != "default") |>
  fselect(-c(headcount_default, population_share_default, welfare_type)) |>
  merge(country_lookup, by = "code", all.x = TRUE)


# =============================================================================
# 1) DM: ggplot version of plot_single_country for Argentina
# =============================================================================
gg_single_country <- function(data, select_country, select_method) {

  poverty_levels <- c("$2.15", "$3.65", "$6.85")
  poverty_positions <- setNames(1:3, poverty_levels)
  select_year <- if (select_method == "Welfare conversion") 2022 else 2019

  dt <- data |>
    filter(year == select_year, country_name == select_country) |>
    pivot_longer(cols = c(headcount_default, headcount_estimate),
                 names_to = "method", values_to = "headcount") |>
    mutate(
      method = recode(method,
                      headcount_default = "PIP",
                      headcount_estimate = "Alternative"),
      poverty_line = factor(poverty_line, levels = poverty_levels),
      x = poverty_positions[as.character(poverty_line)]
    )

  # Arrow data
  dt_arrows <- dt |>
    select(x, poverty_line, method, headcount) |>
    pivot_wider(names_from = method, values_from = headcount) |>
    mutate(
      diff = Alternative - PIP,
      diff_label = paste0(ifelse(diff >= 0, "+", ""), round(diff, 1), " pp"),
      label_y = pmax(PIP, Alternative) + 1.5,
      x_pos = x + 0.06
    )

  method_colors <- c("PIP" = "grey40", "Alternative" = "#0070BB")

  ggplot() +
    # Arrow segments
    geom_segment(
      data = dt_arrows,
      aes(x = x_pos, xend = x_pos, y = PIP, yend = Alternative),
      color = "#5A91C1", linewidth = 0.8,
      arrow = arrow(length = unit(0.15, "cm"), type = "closed")
    ) +
    # Difference labels
    geom_text(
      data = dt_arrows,
      aes(x = x_pos, y = label_y, label = diff_label),
      size = 3.5, color = "#5A91C1"
    ) +
    # Lines + points by method
    geom_line(
      data = dt, aes(x = x, y = headcount, color = method, group = method),
      linewidth = 1
    ) +
    geom_point(
      data = dt, aes(x = x, y = headcount, color = method),
      size = 4, shape = 21, fill = "white", stroke = 1.5
    ) +
    scale_color_manual(
      values = method_colors,
      labels = c("Alternative" = "Alternative methodology",
                 "PIP" = "PIP methodology")
    ) +
    scale_x_continuous(
      breaks = 1:3,
      labels = poverty_levels,
      limits = c(0.5, 3.5)
    ) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = paste0("Poverty rates using different approaches to\n",
                     tolower(select_method)),
      subtitle = paste0(select_country, " in ", select_year, " using 2017 $PPP"),
      x = "Poverty Line",
      y = "Poverty Headcount (%)",
      color = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "grey40"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}


# =============================================================================
# 2) SN: ggplot version of plot_sn_range_bars for Niger
# =============================================================================
gg_sn_range_bars <- function(data, country_code, country_name = NULL,
                             selected_poverty_line = "$2.15") {

  if (is.null(country_name)) country_name <- country_code

  sn_method_colors <- c("db" = "#FF9800", "dou" = "#4EC2C0")

  # Prep data (mirrors prep_fan_data)
  d <- data |>
    filter(
      code == country_code,
      poverty_line == selected_poverty_line,
      method %in% c("db", "dou"),
      !is.na(headcount_estimate)
    ) |>
    mutate(
      level_label = if_else(sub_level == "",
                            str_to_title(reporting_level),
                            str_to_title(sub_level)),
      headcount_pct = headcount_estimate * 100,
      reporting_level = factor(reporting_level,
                               levels = c("urban", "national", "rural"),
                               ordered = TRUE),
      x_num = as.numeric(reporting_level),
      method_label = case_when(method == "db" ~ "DB", method == "dou" ~ "DOU")
    )

  # Fan bounds
  fan <- d |>
    summarise(y_min = min(headcount_pct), y_max = max(headcount_pct),
              .by = c(method, reporting_level, x_num, method_label))

  # Aggregate (sub_level == "")
  agg <- d |>
    filter(sub_level == "") |>
    select(method, reporting_level, x_num, method_label, headcount_pct, level_label)

  # Sub-level points
  sub_pts <- d |> filter(sub_level != "")

  dodge <- 0.15
  method_offset <- c("dou" = -dodge, "db" = dodge)

  fan <- fan |> mutate(x_dodge = x_num + method_offset[method])
  agg <- agg |> mutate(x_dodge = x_num + method_offset[method])
  sub_pts <- sub_pts |> mutate(x_dodge = x_num + method_offset[method])

  # Connecting segments between aggregate points across methods
  agg_wide <- agg |>
    select(reporting_level, x_num, method, headcount_pct) |>
    pivot_wider(names_from = method, values_from = headcount_pct) |>
    mutate(
      delta = db - dou,
      delta_lbl = paste0(if_else(delta >= 0, "+", ""), round(delta, 1), " pp"),
      mid_y = (db + dou) / 2,
      x_dou = x_num + method_offset[["dou"]],
      x_db = x_num + method_offset[["db"]]
    )

  p <- ggplot() +
    # Range bars
    geom_segment(
      data = fan |> filter(y_max > y_min),
      aes(x = x_dodge, xend = x_dodge, y = y_min, yend = y_max, color = method),
      linewidth = 4, alpha = 0.5
    ) +
    # Sub-level points
    geom_point(
      data = sub_pts,
      aes(x = x_dodge, y = headcount_pct, color = method),
      size = 3, alpha = 0.5, shape = 16
    ) +
    # Aggregate points
    geom_point(
      data = agg,
      aes(x = x_dodge, y = headcount_pct, fill = method),
      size = 5, shape = 21, color = "white", stroke = 1.5
    ) +
    # Connecting segments
    geom_segment(
      data = agg_wide,
      aes(x = x_dou, xend = x_db, y = dou, yend = db),
      color = "#363636", linewidth = 0.8
    ) +
    # Delta labels
    geom_text(
      data = agg_wide,
      aes(x = x_db + 0.15, y = mid_y, label = delta_lbl),
      size = 3, fontface = "bold", color = "#363636"
    ) +
    # Sub-level labels
    {
      if (nrow(sub_pts) > 0) {
        sub_labels <- sub_pts |>
          group_by(method, reporting_level) |>
          mutate(
            is_max = headcount_pct == max(headcount_pct),
            vjust_val = if_else(is_max, -0.8, 1.8)
          ) |>
          ungroup()
        geom_text(
          data = sub_labels,
          aes(x = x_dodge, y = headcount_pct, label = level_label,
              vjust = vjust_val),
          size = 2.5, color = "#666666"
        )
      }
    } +
    scale_color_manual(
      values = sn_method_colors,
      labels = c("db" = "DB method", "dou" = "DOU method"),
      guide = guide_legend(order = 1)
    ) +
    scale_fill_manual(
      values = sn_method_colors,
      labels = c("db" = "DB method", "dou" = "DOU method"),
      guide = guide_legend(order = 1)
    ) +
    scale_x_continuous(
      breaks = 1:3,
      labels = c("Urban", "National", "Rural")
    ) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(
      title = paste0("Estimate Spread by Method — ", country_name),
      subtitle = paste0("Range of headcount estimates at ", selected_poverty_line,
                        " poverty line within each reporting level"),
      x = NULL,
      y = "Headcount (%)",
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "grey40"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  p
}


# =============================================================================
# 3) STB: use existing plot_changes (already ggplot) for $2.15 only
# =============================================================================
# Source the function from R/
source("R/fct_plot_interactive_dashboard.R")


# =============================================================================
# Generate and save all four images
# =============================================================================
output_dir <- "inst/app/www"

# DM: Argentina, Welfare conversion
p_dm <- gg_single_country(d_dm, "Argentina", "Welfare conversion")
ggsave(
  file.path(output_dir, "landing_dm.png"),
  plot = p_dm, width = 8, height = 5, dpi = 150, bg = "white"
)

# STB: $2.15 only, Angola highlighted
d_stb_215 <- d_stb |> filter(poverty_line == "$2.15")
p_stb <- plot_changes(
  data           = d_stb_215,
  select_country = "Angola",
  select_method  = "Household allocation",
  title          = "Difference between the <span style='color:black;'>**PIP**</span> and <span style='color:steelblue;'>**alternative**</span> estimates"
)
ggsave(
  file.path(output_dir, "landing_stb.png"),
  plot = p_stb, width = 9, height = 5, dpi = 150, bg = "white"
)

# SN: Niger, granular range bars, $2.15
p_sn <- gg_sn_range_bars(data_sn, "NER", "Niger", "$2.15")
ggsave(
  file.path(output_dir, "landing_sn.png"),
  plot = p_sn, width = 8, height = 5, dpi = 150, bg = "white"
)

# =============================================================================
# 4) YK: ggplot version of Gini sensitivity (cross-country) chart
#    HFCE, 2021 PPP, all years, no country highlighted
# =============================================================================

gg_yk_gini_sensitivity <- function(data) {
  # -- Prepare data -----------------------------------------------------------
  # Use HFCE-adjusted Gini with 2021 PPP; retain all survey years.
  # Drop rows where either Gini column is NA.
  dt <- data.table::as.data.table(data)
  dt <- dt[
    !is.na(gini_survey_2021) & !is.na(gini_hfce_adj_2021)
  ]

  # Dynamic share levels and sequential blue palette (light → dark)
  share_levels <- sort(unique(dt$share))
  n_shares     <- length(share_levels)
  share_palette <- grDevices::colorRampPalette(c("#c6dbef", "#084594"))(n_shares)
  names(share_palette) <- as.character(share_levels)

  # Convert share to factor for discrete colour scale
  dt[, share_fct := factor(share, levels = share_levels)]

  # Compute axis limits: symmetric, rounded to nearest 0.05
  all_vals  <- c(dt$gini_survey_2021, dt$gini_hfce_adj_2021)
  ax_min    <- max(0,   floor(min(all_vals, na.rm = TRUE)  / 0.05) * 0.05 - 0.02)
  ax_max    <- min(1, ceiling(max(all_vals, na.rm = TRUE)  / 0.05) * 0.05 + 0.02)

  # -- Build plot -------------------------------------------------------------
  ggplot(
    data = as.data.frame(dt),
    aes(
      x     = gini_survey_2021,
      y     = gini_hfce_adj_2021,
      color = share_fct
    )
  ) +
    # 45-degree reference line (no change)
    geom_abline(
      slope     = 1,
      intercept = 0,
      color     = "grey60",
      linetype  = "dashed",
      linewidth = 0.8
    ) +
    # Scatter points
    geom_point(
      alpha = 0.55,
      size  = 2.2
    ) +
    # Colour scale matching plotly version
    scale_color_manual(
      values = share_palette,
      name   = "Share of gap\nto top tail"
    ) +
    # Force equal axes
    coord_fixed(
      ratio = 1,
      xlim  = c(ax_min, ax_max),
      ylim  = c(ax_min, ax_max)
    ) +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(
      title    = "Sensitivity of Gini to gap-allocation scenario",
      subtitle = "HFCE aggregate, 2021 PPP — all survey years",
      x        = "Gini coefficient (survey)",
      y        = "Gini coefficient (HFCE-adjusted)",
      caption  = "Points above the diagonal: adjustment raises inequality."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(size = 11, color = "grey40"),
      plot.caption     = element_text(size = 9, color = "grey50"),
      legend.position  = "right",
      legend.title     = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

p_yk <- gg_yk_gini_sensitivity(d_yk)
ggsave(
  file.path(output_dir, "landing_yk.png"),
  plot   = p_yk,
  width  = 8,
  height = 6,
  dpi    = 150,
  bg     = "white"
)

message("Landing page images saved to: ", output_dir)
