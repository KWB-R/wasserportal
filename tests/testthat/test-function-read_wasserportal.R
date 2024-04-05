#library(testthat)
test_that("read_wasserportal() works", {

  f <- wasserportal::read_wasserportal

  expect_error(f())

  expect_error(
    f(
      station = "my_station",
      variables = "my_variable",
      stations_crosstable = data.frame(Messstellennummer = "my_station")
    ),
    "No such variable code"
  )

  expect_error(
    f(
      station = "my_station",
      variables = "my_variable",
      stations_crosstable = data.frame(
        Messstellennummer = "my_station",
        my_variable = "x"
      )
    ),
    "No such variable code"
  )

  expect_output(result <- f(
    station = c(my_station = "5865900"),
    variables = c(surface_water.water_level = "ows"),
    stations_crosstable = data.frame(
      Messstellennummer = "5865900",
      ows = "x"
    )
  ))

  expect_s3_class(result, "data.frame")

  expect_identical(names(result), c(
    "LocalDateTime",
    "UTCOffset",
    "surface_water.water_level"
  ))

})
