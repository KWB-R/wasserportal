test_that("stop_if_not_all_in() works", {

  f <- wasserportal:::stop_if_not_all_in

  expect_error(f())

  expect_error(f("a", c("b", "c"), type = "animal"), "No such animal")
})
