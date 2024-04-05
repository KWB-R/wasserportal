library(testthat)

test_that("get_surfacewater_quality() works", {

  f <- wasserportal:::get_surfacewater_quality

  expect_error(f())

  stations <- wasserportal::get_stations(type = "list", debug = FALSE)

  station_id <- stations$surface_water.quality$Messstellennummer[1L]

  result <- f(station_id)

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
