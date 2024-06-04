#library(testthat)

test_that("wp_timeseries_data_to_list() works", {

  f <- wasserportal:::wp_timeseries_data_to_list

  expect_error(f())
  expect_error(suppressWarnings(f("no-such-file")))
})
