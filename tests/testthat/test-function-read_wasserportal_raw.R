#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2023-09-23 23:10:33.492441.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("read_wasserportal_raw() works", {

  expect_error(
    wasserportal:::read_wasserportal_raw()
    # argument "station" is missing, with no default
  )

})

