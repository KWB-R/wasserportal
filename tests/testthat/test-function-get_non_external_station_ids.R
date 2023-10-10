#library(testthat)

test_that("get_non_external_station_ids() works", {

  f <- wasserportal:::get_non_external_station_ids

  expect_error(f())

  portal_url <- "https://wasserportal.berlin.de"

  station_data <- read.table(sep = ",", header = TRUE, text = "
    Messstellennummer,Betreiber,stammdaten_link
    1,any,any
    2,any,https://wasserportal.berlin.de
    3,Land Berlin,any
    4,Land Berlin,https://wasserportal.berlin.de
    5,,https://wasserportal.berlin.de"
  )

  is_empty <- station_data$Betreiber == ""

  expect_identical(f(station_data), "4")

  station_data$Betreiber[is_empty] <- NA

  expect_identical(f(station_data), "4")
})
