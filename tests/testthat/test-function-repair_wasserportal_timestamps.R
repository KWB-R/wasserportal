test_that("repair_wasserportal_timestamps() works", {

  f <- wasserportal:::repair_wasserportal_timestamps

  expect_error(f())
})
