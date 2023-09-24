test_that("as_date_de() works", {

  f <- wasserportal:::as_date_de

  expect_error(f())

  expect_identical(
    f("31.12.2023"),
    as.Date("2023-12-31")
  )

  expect_identical(
    f(c("30.12.2023", "31.12.2023")),
    as.Date(c("2023-12-30", "2023-12-31"))
  )

})

