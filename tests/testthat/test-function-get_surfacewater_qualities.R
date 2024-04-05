#library(testthat)

test_that("get_surfacewater_qualities() works", {

  f <- wasserportal:::get_surfacewater_qualities

  expect_error(f())

  stations <- wasserportal::get_stations(type = "list", debug = FALSE)

  station_ids <- stations$surface_water.quality$Messstellennummer[1:2]

  expect_output(result <- f(station_ids))

  expect_s3_class(result, "data.frame")

  expect_identical(names(result), c(
    "Messstelle",
    "Messstellennummer",
    "Datum",
    "Parameter",
    "Entnahmetiefe [m]",
    "Messmethode",
    "Vorzeichen",
    "Wert",
    "Einheit",
    "Bestimmungsgrenze"
  ))

})
