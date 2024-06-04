test_that("get_wasserportal_stations() works", {

  result <- wasserportal:::get_wasserportal_stations()

  expect_type(result, "list")

  expect_true("MS_Sophienwerder" %in% names(result))
})
