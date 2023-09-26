#library(testthat)

test_that("get_non_external_station_ids() works", {

  f <- wasserportal:::get_non_external_station_ids

  expect_error(f())

  portal_url <- "https://wasserportal.berlin.de"

  station_data <- kwb.utils::noFactorDataFrame(
    Messstellennummer = as.character(1:4),
    Betreiber =       c("any", "any", "Land Berlin", "Land Berlin"),
    stammdaten_link = c("any", portal_url, "any", portal_url)
  )

  expect_identical(f(station_data), "4")

})
