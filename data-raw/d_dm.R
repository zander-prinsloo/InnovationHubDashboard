## code to prepare `d_dm` dataset goes here

library(fst)
library(fastverse)

dir <- "C:/Users/wb612474/OneDrive - WBG/innovation_hub/temp_data_dashboard"
# load data


d_dm <-
  read.fst(path = fs::path(dir,
                           #"data",
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

usethis::use_data(d_dm, overwrite = TRUE)
