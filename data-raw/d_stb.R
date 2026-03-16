## code to prepare `d_stb` dataset goes here
library(fst)
library(fastverse)

dir <- "C:/Users/wb612474/OneDrive - WBG/innovation_hub/temp_data_dashboard"
# load data
d_stb <-
  read.fst(path = fs::path(dir,
                           "stettehbaah_2024-08-16.fst"))
usethis::use_data(d_stb, overwrite = TRUE)
