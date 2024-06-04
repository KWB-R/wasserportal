#library(testthat)

test_that("columns_to_labels() works", {

  f <- wasserportal:::columns_to_labels

  expect_error(f())

  result <- f(data.frame(a = 1, b = "x"), c("a", "b"))

  expect_identical(result, c("a: 1, b: x"))

})
