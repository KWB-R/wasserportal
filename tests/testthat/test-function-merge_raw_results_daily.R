test_that("merge_raw_results_daily() works", {

  f <- wasserportal:::merge_raw_results_daily

  expect_error(suppressWarnings(f()))

  dfs <- list(
    data.frame(a = 1:2),
    data.frame(b = 2:3)
  )

  expect_warning(result <- f(dfs))

  expect_identical(result, dfs)
})
