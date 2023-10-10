test_that("add_wasserportal_metadata() works", {

  f <- wasserportal:::add_wasserportal_metadata

  expect_error(f())

  expect_identical(
    f("anything", c("one", "two", "three")),
    structure("anything", metadata = "three")
  )

})
