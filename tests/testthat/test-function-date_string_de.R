test_that("date_string_de() works", {

  f <- wasserportal:::date_string_de

  expect_error(f())

  expect_identical(f(as.Date("2023-09-25")), "25.09.2023")
})
