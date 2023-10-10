test_that("get_wasserportal_text() works", {

  f <- wasserportal:::get_wasserportal_text

  expect_error(f())

  expect_identical(
    "Reading 'my_variable' for station 1 (my_station)",
    f(
      station = 1L,
      variable = 2L,
      station_ids = c(my_station = 1L),
      variable_ids = c(my_variable = 2L)
    )
  )

})
