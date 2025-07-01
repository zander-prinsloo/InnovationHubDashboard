#
#
# This code is only temporary




# # load data
# dt_pip <- pipr::get_stats(ppp_version = 2017)
# dt_stb <-
#   read.fst(path = fs::path(dir,
#                            #"data",
#                            "stettehbaah_2024-08-16.fst"))
# dt_sho <-
#   read.fst(path = fs::path(dir,
#                            #"data",
#                            "snakamura2_2024-08-30.fst")) |>
#   fmutate(poverty_line = paste0("$",
#                                 poverty_line)) |>
#   fmutate(headcount_estimate = headcount_estimate*100) |>
#   joyn::joyn(y = dt_pip |>
#                fselect(country_name,
#                        code = country_code) |>
#                funique(),
#              reportvar  = F,
#              match_type = "m:1",
#              keep       = "left")
#
# dt_dm <-
#   read.fst(path = fs::path(dir,
#                            #"data",
#                            "dmahler_2024-08-15.fst")) |>
#   fmutate(headcount_default  = 100*headcount_default,
#           headcount_estimate = 100*headcount_estimate) |>
#   joyn::joyn(y = dt_pip |>
#                fselect(country_name,
#                        code = country_code) |>
#                funique(),
#              reportvar  = F,
#              match_type = "m:1",
#              keep       = "left") |>
#   fmutate(poverty_line = paste0("$",
#                                 poverty_line))
#
#
# # Single data set for Colombia
# cntry <- "COL"
# pline <- "$2.15"
#
# d2019 <- dt_stb |>
#   fsubset(code == cntry &
#             poverty_line == pline) |>
#   pivot(how = "longer",
#         ids = c("code",
#                 "year",
#                 "poverty_line",
#                 "country_name",
#                 "region_code",
#                 "welfare_type",
#                 "pip_vintage",
#                 "reporting_level")) |>
#   fmutate(method = "hh_allocation") |>
#   fmutate(label  = fifelse(variable == "headcount_default",
#                            paste0(round(value, 2),
#                                   " (per capita allocation)"),
#                            paste0(round(value, 2),
#                                   " (square root allocation)")))
#
# d2022 <- dt_dm |>
#   fselect(-c(gini_default,
#              gini_estimate)) |>
#   fsubset(code == cntry &
#             poverty_line == pline) |>
#   pivot(how = "longer",
#         ids = c("code",
#                 "year",
#                 "poverty_line",
#                 "country_name",
#                 "region_code",
#                 "welfare_type",
#                 "reporting_level")) |>
#   fmutate(method = "consumption_conversion") |>
#   fmutate(label = fifelse(variable == "headcount_default",
#                           paste0(round(value, 2),
#                                  " (income)"),
#                           paste0(round(value, 2),
#                                  " (consumption)")))
# d2015 <- dt_sho |>
#   fsubset(code == cntry &
#             poverty_line == pline) |>
#   fmutate(variable = fifelse(reporting_level == "national",
#                              "headcount_default",
#                              "headcount_estimate"),
#           level   = fifelse(sub_level == "",
#                             reporting_level,
#                             sub_level)) |>
#   fsubset(!(reporting_level == "national" & method == "dou")) |>
#   fselect(-reporting_level) |>
#   frename(reporting_level = level) |>
#   frename(value = headcount_estimate) |>
#   fsubset(!is.na(value)) |>
#   fmutate(label = paste0(round(value, 2),
#                          " (",
#                          #method, ", ",
#                          reporting_level,
#                          ")"))
#
# dcol <-
#   rowbind(d2022,
#           d2019 |>
#             fselect(-pip_vintage),
#           d2015 |>
#             fselect(names(d2022))) |>
#   fmutate(label_not = fifelse(variable == "headcount_default",
#                               paste0(round(value, 2)),
#                               label))
#
# write_fst(dcol,
#           path = fs::path(dir,
#                           "dcol.fst"))
#
