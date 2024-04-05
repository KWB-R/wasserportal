test_that("wp_data_to_list() works", {

  f <- wasserportal:::wp_data_to_list

  expect_error(f())

  # f(
  #   overview_list_names = "no-such-file",
  #   target_dir = "abc",
  #   modify_filenames = identity,
  #   is_zipped = FALSE
  # )

})
