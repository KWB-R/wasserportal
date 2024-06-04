test_that("print_invalid_hrefs() works", {

  f <- wasserportal:::print_invalid_hrefs

  expect_error(f())

  expect_null(f(1))

  invalid <- c("a", "b", "c")

  expect_message(capture.output(f(structure(1, invalid = invalid))))
})
