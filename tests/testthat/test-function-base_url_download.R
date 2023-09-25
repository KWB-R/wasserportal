test_that("base_url_download() works", {

  result <- wasserportal:::base_url_download()

  expect_length(result, 1L)
  expect_type(result, "character")
  expect_true(startsWith(result, "https://"))
})
