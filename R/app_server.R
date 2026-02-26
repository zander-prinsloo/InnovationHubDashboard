#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # 1a) load each dataset
  load("data/d_dm.rda")   # creates object d_dm
  load("data/d_stb.rda")  # creates object d_stb
  load("data/d_sn.rda")   # creates object d_sn

  dm_metadata  <- readxl::read_excel("data/dm_metadata.xlsx")
  stb_metadata <- readxl::read_excel("data/stb_metadata.xlsx")
  sn_metadata  <- readxl::read_excel("data/sn_metadata.xlsx")

  library(fastverse)
  #library(fst)
  library(tidyverse)
  library(ggtext)
  library(ggrepel)
  #library(viridis)
  #library(ggalt)  # for geom_dumbbell
  library(scales)
  library(glue)
  library(grid)

  # 1b) preprocess d_sn: remove default method, drop unneeded cols, join country_name
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

  # 1c) create cross-country SN dataset: pivot DB→headcount_default, DOU→headcount_estimate
  data_sn_cross <- d_sn |>
    fsubset(sub_level == "" & method %in% c("db", "dou")) |>
    fselect(code, region_code, year, reporting_level, poverty_line, method, headcount_estimate) |>
    tidyr::pivot_wider(
      names_from  = method,
      values_from = headcount_estimate,
      names_prefix = "hc_"
    ) |>
    dplyr::mutate(
      headcount_default  = hc_db  * 100,
      headcount_estimate = hc_dou * 100
    ) |>
    dplyr::select(-hc_db, -hc_dou) |>
    merge(country_lookup, by = "code", all.x = TRUE) |>
    as.data.frame()

  # 1d) combine, dropping the unwanted cols
  data_cntry_plot <- rowbind(d_stb |>
                               fselect(-c(pip_vintage,
                                          welfare_type)),
                             d_dm |>
                               fselect(-c(welfare_type,
                                          gini_default,
                                          gini_estimate)))

  # 1e) call your module, passing it the combined data
  mod_interactive_dashboard_server(
    id            = "interactive_dashboard_1",
    data_dm       = d_dm,
    data_stb      = d_stb,
    data_sn       = data_sn,
    data_sn_cross = data_sn_cross,
    dm_metadata   = dm_metadata,
    stb_metadata  = stb_metadata,
    sn_metadata   = sn_metadata
  )
}

