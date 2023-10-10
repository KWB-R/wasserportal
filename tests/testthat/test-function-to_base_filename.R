test_that("to_base_filename() works", {

  f <- wasserportal:::to_base_filename

  expect_error(f())

  expect_identical(f("a_b"), "a-b")
  expect_identical(f("a.b"), "a_b")
  expect_identical(f("a_b.c"), "a-b_c")
})
