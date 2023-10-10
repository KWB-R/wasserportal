test_that("get_wasserportal_variables() works", {

  result <- wasserportal:::get_wasserportal_variables()

  expect_type(result, "character")
  expect_true("Wassertemperatur" %in% names(result))
})
