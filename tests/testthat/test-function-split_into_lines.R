#library(testthat)
test_that("split_into_lines() works", {

  f <- wasserportal:::split_into_lines

  expect_error(f())
  expect_error(f(1))
  expect_error(f(c("a", "b")))
  expect_identical(f("a\nb"), c("a", "b"))
})
