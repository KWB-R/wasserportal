test_that("get_station_variables() works", {

  f <- wasserportal:::get_station_variables

  expect_error(f())

  station_df <- data.frame(
    Messstellennummer = 1:3,
    Messstellenname = LETTERS[1:3],
    a = 2:4,
    b = 3:5
  )

  expect_identical(f(station_df), c("a", "b"))
})
