test_that("get_url_and_body_for_groundwater_data_download() works", {

  f <- wasserportal:::get_url_and_body_for_groundwater_data_download

  expect_error(f())

  result <- f(stype = 1, type = "daily", from_date = "2000", station = 1)

  expect_type(result, "list")
  expect_identical(names(result), c("url", "body"))
  expect_identical(result$body, list())
})
