test_that("is_external_link() works", {

  f <- wasserportal:::is_external_link

  expect_error(f())

  expect_true(f("is-not-wasserportal-url"))
  expect_false(f(wasserportal:::wasserportal_base_url()))
})
