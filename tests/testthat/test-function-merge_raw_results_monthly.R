#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2023-09-23 23:10:31.576169.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("merge_raw_results_monthly() works", {

  f <- wasserportal:::merge_raw_results_monthly

  expect_error(suppressWarnings(f()))

  expect_warning(result <- f(123))
  expect_identical(result, 123)

})

