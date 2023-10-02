test_that("warning_not_implemented() works", {

  f <- wasserportal:::warning_not_implemented

  expect_error(f())

  expect_warning(f("abc"))
})
