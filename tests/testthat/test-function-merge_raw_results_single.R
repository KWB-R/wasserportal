#library(testthat)

test_that("merge_raw_results_single() works", {

  f <- wasserportal:::merge_raw_results_single

  expect_error(f())

  df1 <- data.frame(
    LocalDateTime = Sys.time(),
    a = 1
  )

  df2 <- data.frame(
    LocalDateTime = Sys.time(),
    a = 2
  )

  df3 <- data.frame(
    LocalDateTime = Sys.time(),
    a = 3
  )

  dfs <- list(df1, df2, df3)

  f(dfs, variables = "a", include_raw_time = FALSE)

})

