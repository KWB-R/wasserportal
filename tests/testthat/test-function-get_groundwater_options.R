test_that("get_groundwater_options() works", {

  result <- wasserportal:::get_groundwater_options()

  expect_identical(
    names(result),
    c("groundwater.level", "groundwater.quality")
  )
})

