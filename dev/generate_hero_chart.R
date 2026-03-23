# =============================================================================
# Generate hero welfare gap SVG for the homepage hero section
# =============================================================================
# Run this script manually from the project root whenever d_yk data changes.
# It creates a single SVG file used as the decorative visual in the hero.
#
# Output:
#   inst/app/www/hero_welfare_gap.svg
#
# Design intent (v4 — compositional refinement):
#   A confident branded graphic inspired by the PIP hero. The point cloud uses
#   all survey years (~2000 pts) to form a coherent visual mass rather than
#   scattered marks. One dominant curve (slimmer and more precise than v3),
#   one bright focal dot at the curve minimum. Faint white axis lines along
#   the bottom and left edges act as a structural L-frame — anchoring the
#   composition without reading as analytical chart furniture. All in a single
#   cool blue family against the dark navy hero (#002244).
#
#   Layer order (bottom → top):
#     1. Dense all-years point cloud (very low alpha — textural mass)
#     2. Combined LOESS curve across all welfare types (dominant, bright)
#     3. Focal glow ring at the curve minimum (large, low alpha — the "halo")
#     4. Focal dot at the curve minimum (solid, near-white — the anchor)
#
# To regenerate:
#   source("dev/generate_hero_chart.R")
# =============================================================================

library(ggplot2)
library(data.table)

# ── Constants ----------------------------------------------------------------

# Single cool-blue colour family for all chart elements, on dark navy (#002244).
HERO_COLOR_POINT <- "#8CC8FF"   # cool sky blue — textural point cloud (all survey years)
HERO_COLOR_CURVE <- "#C8E8FF"   # lighter blue-white — dominant trend curve
HERO_COLOR_FOCAL <- "#FFFFFF"   # pure white — focal dot at the curve minimum

# Alpha values.
# The cloud is very soft — texture only, not data.
# The curve is the primary visual element — strong and deliberate.
# The focal dot is fully opaque; the glow ring behind it is low alpha.
HERO_ALPHA_POINT      <- 0.20   # sparse cloud: recessive
HERO_ALPHA_CURVE      <- 0.85   # dominant curve: confident
HERO_ALPHA_FOCAL_GLOW <- 0.18   # glow halo behind the focal dot

# GDP per capita tick values for the log scale. Axis text is stripped but the
# scale needs breaks to space the data correctly across the visual area.
HERO_GDPPC_BREAKS <- c(1, 2, 5, 10, 20, 50, 100, 200, 400)

# Output dimensions (inches). Panoramic aspect ratio matching the 520px column.
HERO_WIDTH  <- 7.0
HERO_HEIGHT <- 3.5

# ── Load data ----------------------------------------------------------------

load("data/d_yk.rda")   # creates object d_yk

# ── Prepare data ------------------------------------------------------------

dt <- data.table::as.data.table(d_yk)

# Deduplicate to one row per country × year × welfare_type so no gap value
# is double-counted across share scenarios.
dt <- unique(dt, by = c("country_code", "year", "welfare_type"))

# Drop rows where gap or GDP pc is NA or non-finite.
dt <- dt[
  !is.na(gap_hfce_2021)    & is.finite(gap_hfce_2021) &
    !is.na(gdp_pc_ppp_2021) & is.finite(gdp_pc_ppp_2021)
]

# Dense cloud: all survey years across all welfare types (~2000 points).
# Using the full dataset produces a coherent visual mass that fills the
# composition and reads as a textural field rather than scattered marks.
dt_points <- dt

# Curve dataset: all survey years, welfare types pooled.
# A single combined LOESS across all data produces a cleaner, more
# authoritative curve than two separate per-type lines.
dt_curve <- dt

# ── Compute focal dot --------------------------------------------------------
# Fit a LOESS on the log10-transformed GDP per capita column so that the
# smoothing mirrors what ggplot2 does on the log10 x-scale.
# The formula references a literal column name so predict() can find it.
dt_curve_df <- as.data.frame(dt_curve)
dt_curve_df$log10_gdp <- log10(dt_curve_df$gdp_pc_ppp_2021)

loess_fit <- stats::loess(
  gap_hfce_2021 ~ log10_gdp,
  data = dt_curve_df,
  span = 0.75
)

# Predict across the observed x range on the log scale.
x_log_seq <- seq(
  from       = min(dt_curve_df$log10_gdp),
  to         = max(dt_curve_df$log10_gdp),
  length.out = 500L
)
y_pred <- stats::predict(loess_fit, newdata = data.frame(
  log10_gdp = x_log_seq
))

# The focal dot sits at the minimum predicted gap value.
focal_idx <- which.min(y_pred)
focal_x   <- 10^x_log_seq[focal_idx]   # back-transform from log10
focal_y   <- y_pred[focal_idx]

dt_focal <- data.frame(x = focal_x, y = focal_y)

# ── Build chart --------------------------------------------------------------

p <- ggplot() +

  # ── Layer 1: dense all-years point cloud --------------------------------
  # Very low alpha so the cloud reads as soft texture, not individual surveys.
  geom_point(
    data  = as.data.frame(dt_points),
    aes(x = gdp_pc_ppp_2021, y = gap_hfce_2021),
    shape = 16,
    size  = 1.4,
    color = HERO_COLOR_POINT,
    alpha = HERO_ALPHA_POINT
  ) +

  # ── Layer 2: combined LOESS curve (dominant visual element) --------------
  # All welfare types pooled — one authoritative curve.
  # span = 0.75 matches the analytical chart. se = FALSE removes the
  # confidence band (unwanted visual weight).
  geom_smooth(
    data      = as.data.frame(dt_curve),
    aes(x = gdp_pc_ppp_2021, y = gap_hfce_2021),
    method    = "loess",
    formula   = y ~ x,
    se        = FALSE,
    span      = 0.75,
    linewidth = 1.8,    # was 2.2 — slimmer, more precise, less padded
    color     = HERO_COLOR_CURVE,
    alpha     = HERO_ALPHA_CURVE
  ) +

  # ── Layer 3: focal glow ring (halo behind the anchor dot) ---------------
  # A large, very low-alpha circle painted first so the solid dot sits on top.
  # Creates the PIP-style "glowing dot" effect without external packages.
  geom_point(
    data  = dt_focal,
    aes(x = x, y = y),
    shape = 16,
    size  = 10,
    color = HERO_COLOR_FOCAL,
    alpha = HERO_ALPHA_FOCAL_GLOW
  ) +

  # ── Layer 4: focal anchor dot --------------------------------------------
  # Fully opaque near-white point that anchors the eye at the curve minimum.
  geom_point(
    data  = dt_focal,
    aes(x = x, y = y),
    shape = 16,
    size  = 4.5,
    color = HERO_COLOR_FOCAL,
    alpha = 1.0
  ) +

  # ── Scales ---------------------------------------------------------------
  scale_x_log10(breaks = HERO_GDPPC_BREAKS) +

  # ── Theme ----------------------------------------------------------------
  # theme_minimal() base so we can retain axis lines selectively.
  # Every other element is explicitly removed so the graphic stays clean.
  # The two axis lines (bottom x, left y) form a faint L-shaped structural
  # frame that anchors the composition — structural depth without gridlines.
  theme_minimal() +
  theme(
    # Faint white axis lines — structural anchors, not chart furniture.
    # Alpha ~0.12 keeps them subliminal: you feel the structure, not see it.
    axis.line.x.bottom = element_line(
      colour    = scales::alpha("white", 0.12),
      linewidth = 0.4
    ),
    axis.line.y.left = element_line(
      colour    = scales::alpha("white", 0.12),
      linewidth = 0.4
    ),
    # Strip all gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_blank(),
    # Strip all axis text, titles, and ticks
    axis.title  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    # No legend
    legend.position  = "none",
    # Transparent backgrounds
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin      = margin(4, 8, 4, 4)   # slight right padding for breathing room
  )

# ── Save SVG -----------------------------------------------------------------

output_path <- "inst/app/www/hero_welfare_gap.svg"

ggsave(
  filename = output_path,
  plot     = p,
  width    = HERO_WIDTH,
  height   = HERO_HEIGHT,
  device   = "svg",
  bg       = "transparent"
)

message("Hero welfare gap SVG saved to: ", output_path)
