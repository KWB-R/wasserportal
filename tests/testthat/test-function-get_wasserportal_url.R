test_that("get_wasserportal_url() works", {

  f <- wasserportal:::get_wasserportal_url

  expect_error(f())

  expect_identical(
    f(123, 456),
    "https://wasserportal.berlin.de/station.php?sstation=123&anzeige=456d"
  )

})
