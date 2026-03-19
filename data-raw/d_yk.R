## code to prepare `d_yk` dataset (MTG method — NA–Survey gap adjustment)
##
## Source:  data/initial_data.dta
## Output:  data/d_yk.rda
##
## Schema after prep:
##   country_code, year, share (25/50/75/100 only), welfare_type, total_pop
##   region_code   (WDI iso3c: SSF ECS MEA LCN EAS SAS NAC)
##   region_name   (WDI full name, e.g. "Sub-Saharan Africa")
##   country_name  (joined from wbstats)
##   mean_survey_2021, mean_hfce_2021, mean_gdp_2021
##   mean_survey_2017, mean_hfce_2017, mean_gdp_2017
##   gdp_pc_ppp_2021, hfce_pc_ppp_2021
##   gdp_pc_ppp_2017, hfce_pc_ppp_2017
##   gini_survey_2021, gini_hfce_adj_2021, gini_gdp_adj_2021
##   gini_survey_2017, gini_hfce_adj_2017, gini_gdp_adj_2017
##   gap_hfce_2021, gap_gdp_2021  (signed relative gap: (survey - NA) / NA)
##   gap_hfce_2017, gap_gdp_2017
##   is_latest     (TRUE for each country's most recent survey year)

library(data.table)
library(haven)
library(wbstats)

# ---------------------------------------------------------------------------
# 1. Load raw data
# ---------------------------------------------------------------------------
# Expected columns documented in schema above; fail informatively if missing.
d_raw <- haven::read_dta("data/initial_data.dta") |>
  as.data.table()

# Drop Stata-specific labelled class from all columns
d_raw <- d_raw[, lapply(.SD, function(x) {
  if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x
})]

# Rename Stata _merge column (illegal in R)
if ("_merge" %in% names(d_raw)) {
  data.table::setnames(d_raw, "_merge", "merge_flag")
}

# Validate required columns exist
required_cols <- c(
  "country_code", "year", "share", "region_code", "welfare_type",
  "mean_survey_2021", "mean_hfce_2021", "mean_gdp_2021",
  "mean_survey_2017", "mean_hfce_2017", "mean_gdp_2017",
  "gdp_pc_ppp_2021", "hfce_pc_ppp_2021",
  "gdp_pc_ppp_2017", "hfce_pc_ppp_2017",
  "gini_survey_2021", "gini_hfce_adj_2021", "gini_gdp_adj_2021",
  "gini_survey_2017", "gini_hfce_adj_2017", "gini_gdp_adj_2017"
)
missing_cols <- setdiff(required_cols, names(d_raw))
if (length(missing_cols) > 0) {
  stop(
    "data/initial_data.dta is missing expected columns: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# 2. Build WDI country + region lookup
# ---------------------------------------------------------------------------
# wbstats::wb_countries() uses:
#   iso3c       -> country ISO3 code (matches country_code in d_raw)
#   country     -> country name
#   region_iso3c -> WDI region code (SSF, ECS, MEA, LCN, EAS, SAS, NAC)
#   region      -> WDI region name
#
# NOTE: wbstats is only called here at data-prep time, not at app runtime.
# TODO(P3-reproducibility): Consider caching wb_countries() result to a small
# CSV in data-raw/ so d_yk.rda can be rebuilt without internet access and
# country name mappings remain stable across runs.
wb_lookup <- wbstats::wb_countries() |>
  as.data.table()

# Keep only true countries (those with a non-NA region_iso3c)
wb_lookup <- wb_lookup[!is.na(region_iso3c), .(
  country_code  = iso3c,
  country_name  = country,
  region_name   = region
)]

# ---------------------------------------------------------------------------
# 3. Join country names and region names onto raw data
# ---------------------------------------------------------------------------
# region_code in d_raw is already WDI iso3c (SSF, ECS, etc.) — keep as-is.
d_yk <- merge(
  d_raw,
  wb_lookup,
  by       = "country_code",
  all.x    = TRUE,
  sort     = FALSE
)

# ---------------------------------------------------------------------------
# 4. Compute signed relative gap columns
# ---------------------------------------------------------------------------
# gap = (survey_mean - NA_mean) / NA_mean
# Negative values → survey > NA aggregate (survey over-reports relative to NA)
# Positive values → NA > survey (survey under-reports relative to NA)
d_yk[, gap_hfce_2021 := (mean_survey_2021 - hfce_pc_ppp_2021) / hfce_pc_ppp_2021]
d_yk[, gap_gdp_2021  := (mean_survey_2021 - gdp_pc_ppp_2021)  / gdp_pc_ppp_2021]
d_yk[, gap_hfce_2017 := (mean_survey_2017 - hfce_pc_ppp_2017) / hfce_pc_ppp_2017]
d_yk[, gap_gdp_2017  := (mean_survey_2017 - gdp_pc_ppp_2017)  / gdp_pc_ppp_2017]

# ---------------------------------------------------------------------------
# 5. Flag latest survey year per country × welfare_type
# ---------------------------------------------------------------------------
# is_latest = TRUE for the most recent year for each country_code × welfare_type.
# This ensures Chart 1 shows each country's latest income AND latest consumption
# point separately when "latest year only" is selected.
# When multiple share values exist for the same country-year-welfare_type,
# all those rows get is_latest = TRUE.
d_yk[, is_latest := year == max(year), by = .(country_code, welfare_type)]

# ---------------------------------------------------------------------------
# 5b. Restrict to quartile-boundary shares (25, 50, 75, 100)
# ---------------------------------------------------------------------------
# The source data contains share in increments of 5 (5, 10, ..., 100 + NA).
# The Gini sensitivity chart only needs quartile boundaries. Filtering here
# reduces the dataset size (~4× fewer rows) and avoids runtime filtering.
d_yk <- d_yk[share %in% c(25L, 50L, 75L, 100L)]

# ---------------------------------------------------------------------------
# 6. Select and order columns
# ---------------------------------------------------------------------------
cols_keep <- c(
  "country_code", "country_name", "year", "share",
  "region_code", "region_name", "welfare_type", "total_pop",
  "mean_survey_2021", "mean_hfce_2021", "mean_gdp_2021",
  "mean_survey_2017", "mean_hfce_2017", "mean_gdp_2017",
  "gdp_pc_ppp_2021", "hfce_pc_ppp_2021",
  "gdp_pc_ppp_2017", "hfce_pc_ppp_2017",
  "gini_survey_2021", "gini_hfce_adj_2021", "gini_gdp_adj_2021",
  "gini_survey_2017", "gini_hfce_adj_2017", "gini_gdp_adj_2017",
  "gap_hfce_2021", "gap_gdp_2021",
  "gap_hfce_2017", "gap_gdp_2017",
  "is_latest",
  "hfce_adj", "gdp_adj", "has_nas_data", "hfce_extrapolated"
)
# Only keep columns that exist (guard against schema drift)
cols_keep <- intersect(cols_keep, names(d_yk))
d_yk <- d_yk[, ..cols_keep]

# Set key for efficient lookups in the app
data.table::setkey(d_yk, country_code, year, share)

# ---------------------------------------------------------------------------
# 7. Save
# ---------------------------------------------------------------------------
usethis::use_data(d_yk, overwrite = TRUE)

# Also copy to inst/app/data/ so the Shiny app (which loads via app_sys())
# picks up the same file at runtime.
file.copy("data/d_yk.rda", "inst/app/data/d_yk.rda", overwrite = TRUE)
