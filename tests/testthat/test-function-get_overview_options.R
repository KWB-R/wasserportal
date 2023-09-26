test_that("get_overview_options() works", {

  result <- wasserportal:::get_overview_options()

  expect_identical(
    names(result),
    c("surface_water", "groundwater")
  )

  expect_true(all(sapply(result, is.list)))
})
