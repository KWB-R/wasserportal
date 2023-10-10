test_that("get_daily_surfacewater_data() works", {

  f <- wasserportal:::get_daily_surfacewater_data

  expect_error(f())

  stations <- wasserportal::get_stations(
    type = c("list", "crosstable"),
    debug = FALSE
  )

  tmp <- stations$overview_list$surface_water.water_level[1L, ]
  stations$overview_list$surface_water.water_level <- tmp

  expect_warning(capture.output(result <- f(
    stations,
    variables = c(surface_water.water_level = "ows")
  )))

  expect_identical(
    names(result$surface_water.water_level),
    c("Messstellennummer", "Datum", "Tagesmittelwert", "Parameter", "Einheit")
  )

})
