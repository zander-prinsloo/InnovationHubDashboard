## code to prepare `d_sn` dataset goes here

library(fst)
library(fastverse)

dir <- "C:/Users/wb612474/OneDrive - WBG/innovation_hub/temp_data_dashboard"

d_sn <-
  read.fst(path = fs::path(dir, "snakamura2_2024-08-30.fst")) |>
  fmutate(poverty_line = paste0("$",
                                poverty_line))
usethis::use_data(d_sn, overwrite = TRUE)
