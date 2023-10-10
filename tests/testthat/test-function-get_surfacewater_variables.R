test_that("get_surfacewater_variables() works", {

  result <- wasserportal:::get_surfacewater_variables()

  expect_type(result, "character")

  expect_true(all(startsWith(names(result), "surface_water.")))

})
