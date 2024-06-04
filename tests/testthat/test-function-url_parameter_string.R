#library(testthat)
test_that("url_parameter_string() works", {

  f <- wasserportal:::url_parameter_string

  expect_identical(f(), "")

  expect_identical(f(a = 1, b = 2), "a=1&b=2")
  expect_identical(f(a = 1, b = "abc"), "a=1&b=abc")
})
