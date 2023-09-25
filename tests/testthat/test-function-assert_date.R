test_that("assert_date() works", {

  f <- wasserportal:::assert_date

  expect_error(f())

  expect_identical(f(1), as.Date(1, origin = "1970-01-01"))

  expect_error(f("a"))
})
