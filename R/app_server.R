#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # packages
  #---------------------------
  library(fastverse)
  library(fst)
  library(tidyverse)
  library(ggtext)
  library(ggrepel)
  library(viridis)
  library(ggalt)  # for geom_dumbbell
  library(scales)
  library(glue)
  library(grid)

  # load titles
  titles <- list(
    main_title   = "Comparing poverty estimates for Colombia",
    subtitle_use = "Default PIP methodology at the $2.15 international poverty line",
    caption_use  = "Estimates supplied by authors; not strictly comparable across vintages.")

  # load data
  dir <- "C:/Users/wb612474/OneDrive - WBG/innovation_hub/ih-visualisations"
  dt_pip <- pipr::get_stats(ppp_version = 2017)
  dt_stb <-
    read.fst(path = fs::path(dir,
                             "data",
                             "stettehbaah_2024-08-16.fst"))
  dt_sho <-
    read.fst(path = fs::path(dir,
                             "data",
                             "snakamura2_2024-08-30.fst")) |>
    fmutate(poverty_line = paste0("$",
                                  poverty_line)) |>
    fmutate(headcount_estimate = headcount_estimate*100) |>
    joyn::joyn(y = dt_pip |>
                 fselect(country_name,
                         code = country_code) |>
                 funique(),
               reportvar  = F,
               match_type = "m:1",
               keep       = "left")

  dt_dm <-
    read.fst(path = fs::path(dir,
                             "data",
                             "dmahler_2024-08-15.fst")) |>
    fmutate(headcount_default  = 100*headcount_default,
            headcount_estimate = 100*headcount_estimate) |>
    joyn::joyn(y = dt_pip |>
                 fselect(country_name,
                         code = country_code) |>
                 funique(),
               reportvar  = F,
               match_type = "m:1",
               keep       = "left") |>
    fmutate(poverty_line = paste0("$",
                                  poverty_line))

  # Some data checks
  #---------------------------
  dt_stb |>
    fsubset(code == "COL") |>
    fselect(code,
            year,
            poverty_line,
            headcount_default,
            headcount_estimate)
  dt_sho |>
    fsubset(code == "COL") |>
    fselect(code,
            year,
            reporting_level,
            sub_level,
            method,
            poverty_line,
            headcount_estimate) |>
    fsubset(poverty_line == "$2.15") |>
    fsubset(!is.na(headcount_estimate))
  dt_pip |>
    fsubset(country_code      == "COL" &
              year            == 2015 &
              reporting_level == "national") |>
    fselect(country_code,
            year,
            headcount)

  dt_dm |>
    fsubset(code == "COL")
  common_countries <-
    intersect(dt_stb$code,
              dt_dm$code)

  rowbind(dt_stb |>
            fsubset(code %in% common_countries &
                      poverty_line == "$2.15") |>
            fselect(code,
                    year,
                    headcount_default,
                    headcount_estimate),
          dt_dm |>
            fsubset(code %in% common_countries &
                      poverty_line == "$2.15") |>
            fselect(code,
                    year,
                    headcount_default,
                    headcount_estimate)) |>
    fgroup_by(code) |>
    fsummarise(sqrdiff = fmean(abs(headcount_default - headcount_estimate))) |>
    arrange(-sqrdiff) |>
    head(10)
  dt_stb |>
    fsubset(code %in% common_countries &
              poverty_line == "$2.15") |>
    fselect(code, year, headcount_default, headcount_estimate) |>
    fgroup_by(code) |>
    fsummarise(sqrdiff = fmean(abs(headcount_default - headcount_estimate))) |>
    arrange(-sqrdiff) |>
    head(10)

  dt_dm |>
    fsubset(code %in% common_countries &
              poverty_line == "$2.15") |>
    fselect(code, year, headcount_default, headcount_estimate) |>
    fgroup_by(code) |>
    fsummarise(sqrdiff = fmean(abs(headcount_default - headcount_estimate))) |>
    arrange(-sqrdiff) |>
    head(10)

  # Single data set for Colombia
  cntry <- "COL"
  pline <- "$2.15"

  d2019 <- dt_stb |>
    fsubset(code == cntry &
              poverty_line == pline) |>
    pivot(how = "longer",
          ids = c("code",
                  "year",
                  "poverty_line",
                  "country_name",
                  "region_code",
                  "welfare_type",
                  "pip_vintage",
                  "reporting_level")) |>
    fmutate(method = "hh_allocation") |>
    fmutate(label  = fifelse(variable == "headcount_default",
                             paste0(round(value, 2),
                                    " (per capita allocation)"),
                             paste0(round(value, 2),
                                    " (square root allocation)")))

  d2022 <- dt_dm |>
    fselect(-c(gini_default,
               gini_estimate)) |>
    fsubset(code == cntry &
              poverty_line == pline) |>
    pivot(how = "longer",
          ids = c("code",
                  "year",
                  "poverty_line",
                  "country_name",
                  "region_code",
                  "welfare_type",
                  "reporting_level")) |>
    fmutate(method = "consumption_conversion") |>
    fmutate(label = fifelse(variable == "headcount_default",
                            paste0(round(value, 2),
                                   " (income)"),
                            paste0(round(value, 2),
                                   " (consumption)")))
  d2015 <- dt_sho |>
    fsubset(code == cntry &
              poverty_line == pline) |>
    fmutate(variable = fifelse(reporting_level == "national",
                               "headcount_default",
                               "headcount_estimate"),
            level   = fifelse(sub_level == "",
                              reporting_level,
                              sub_level)) |>
    fsubset(!(reporting_level == "national" & method == "dou")) |>
    fselect(-reporting_level) |>
    frename(reporting_level = level) |>
    frename(value = headcount_estimate) |>
    fsubset(!is.na(value)) |>
    fmutate(label = paste0(round(value, 2),
                           " (",
                           #method, ", ",
                           reporting_level,
                           ")"))

  dcol <-
    rowbind(d2022,
            d2019 |>
              fselect(-pip_vintage),
            d2015 |>
              fselect(names(d2022))) |>
    fmutate(label_not = fifelse(variable == "headcount_default",
                                paste0(round(value, 2)),
                                label))


  mod_country_deepdives_multiple_methods_server(
    id     = "dd_col",
    d_all  = dcol,
    titles = titles
  )
}
