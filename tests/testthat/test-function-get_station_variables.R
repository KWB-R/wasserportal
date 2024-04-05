test_that("get_station_variables() works", {

  f <- wasserportal:::get_station_variables

  expect_error(f())

  df1 <- data.frame(
    Messstellennummer = 1:2,
    Messstellenname = c("a", "b"),
    my_var = c("x", NA)
  )

  df2 <- kwb.utils::renameColumns(df1, list(my_var = "gwq"))

  expect_error(f(df1), "No such variable code")

  result <- f(df2)

  expect_identical(result, c(groundwater.quality = "gwq"))
})
