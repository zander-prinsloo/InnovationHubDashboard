## code to prepare `wb_country_names` dataset
##
## Source:  wbstats::wb_countries() — WDI country metadata
## Output:  data/wb_country_names.rda
##          inst/app/data/wb_country_names.rda
##
## Schema:
##   country_code  (chr) — ISO3C code, e.g. "ALB"
##   country_name  (chr) — WDI country display name, e.g. "Albania"
##
## Usage:
##   Provides an authoritative iso3 → country_name lookup for the entire app,
##   replacing hardcoded per-module lookups.  All countries that appear in
##   PIP / MTG fst files are covered by the WDI list.
##
## Re-run this script whenever country names need to be refreshed (rare).

library(data.table)
library(wbstats)

# ---------------------------------------------------------------------------
# 1. Fetch WDI country metadata
# ---------------------------------------------------------------------------
# wb_countries() returns one row per country/aggregate.  We keep only true
# countries — identified by having a non-NA `region_iso3c` field, which
# excludes World Bank aggregate groups (e.g. "World", "High income", etc.).
wb_raw <- wbstats::wb_countries()

required_cols <- c("iso3c", "country", "region_iso3c")
missing <- setdiff(required_cols, names(wb_raw))
if (length(missing) > 0L) {
  rlang::abort(
    paste0(
      "wbstats::wb_countries() is missing expected columns: ",
      paste(missing, collapse = ", ")
    )
  )
}

# ---------------------------------------------------------------------------
# 2. Filter and rename
# ---------------------------------------------------------------------------
wb_country_names <- as.data.table(wb_raw)[
  !is.na(region_iso3c),
  .(
    country_code = iso3c,
    country_name = country
  )
]

# Validate no NAs slipped through
if (anyNA(wb_country_names$country_code) || anyNA(wb_country_names$country_name)) {
  rlang::abort("wb_country_names contains unexpected NA values after filtering.")
}

if (nrow(wb_country_names) < 200L) {
  rlang::abort(
    paste0(
      "wb_country_names has only ", nrow(wb_country_names),
      " rows — expected 200+. Check wbstats output."
    )
  )
}

# ---------------------------------------------------------------------------
# 3. Key for fast lookups in the app
# ---------------------------------------------------------------------------
data.table::setkey(wb_country_names, country_code)

# ---------------------------------------------------------------------------
# 4. Save
# ---------------------------------------------------------------------------
usethis::use_data(wb_country_names, overwrite = TRUE)

# Also copy to inst/app/data/ so the Shiny app (loaded via app_sys()) picks
# up the same file at runtime without requiring internet access.
ok <- file.copy("data/wb_country_names.rda", "inst/app/data/wb_country_names.rda",
                overwrite = TRUE)
if (!ok) {
  rlang::abort(
    "Failed to copy wb_country_names.rda to inst/app/data/. ",
    "Check that inst/app/data/ exists and is writable."
  )
}

cli::cli_inform(c(
  "v" = "wb_country_names saved: {nrow(wb_country_names)} countries.",
  "i" = "Files written to data/ and inst/app/data/."
))
