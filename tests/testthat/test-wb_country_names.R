test_that("wb_country_names.rda loads and has expected structure", {
  rda_path <- system.file("app/data/wb_country_names.rda", package = "InnovationHubDashboard")
  skip_if(nchar(rda_path) == 0L, "wb_country_names.rda not found — run data-raw/wb_country_names.R")

  env <- new.env(parent = emptyenv())
  load(rda_path, envir = env)

  expect_true(
    exists("wb_country_names", envir = env),
    info = "Object 'wb_country_names' must exist in .rda"
  )

  wb <- env$wb_country_names
  expect_true(data.table::is.data.table(wb))
  expect_true(all(c("country_code", "country_name") %in% names(wb)))
  expect_equal(ncol(wb), 2L)
  expect_gt(nrow(wb), 200L)
  expect_false(anyNA(wb$country_code))
  expect_false(anyNA(wb$country_name))
})

test_that("wb_country_names contains expected countries", {
  rda_path <- system.file("app/data/wb_country_names.rda", package = "InnovationHubDashboard")
  skip_if(nchar(rda_path) == 0L, "wb_country_names.rda not found")

  env <- new.env(parent = emptyenv())
  load(rda_path, envir = env)
  wb <- env$wb_country_names

  # A representative set of countries that must be present
  expected_codes <- c("ALB", "AGO", "BGD", "COL", "ETH", "VNM", "ZAF", "IND", "BRA")
  expect_true(
    all(expected_codes %in% wb$country_code),
    info = paste("Missing codes:", paste(setdiff(expected_codes, wb$country_code), collapse = ", "))
  )
})

test_that("wb_country_names country_code values are 3-character upper-case strings", {
  rda_path <- system.file("app/data/wb_country_names.rda", package = "InnovationHubDashboard")
  skip_if(nchar(rda_path) == 0L, "wb_country_names.rda not found")

  env <- new.env(parent = emptyenv())
  load(rda_path, envir = env)
  wb <- env$wb_country_names

  expect_true(all(nchar(wb$country_code) == 3L))
  expect_true(all(wb$country_code == toupper(wb$country_code)))
})

test_that("wb_country_names has no duplicate country_code values", {
  rda_path <- system.file("app/data/wb_country_names.rda", package = "InnovationHubDashboard")
  skip_if(nchar(rda_path) == 0L, "wb_country_names.rda not found")

  env <- new.env(parent = emptyenv())
  load(rda_path, envir = env)
  wb <- env$wb_country_names

  expect_equal(nrow(wb), data.table::uniqueN(wb$country_code))
})
