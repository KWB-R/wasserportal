test_that("get_wasserportal_stations_table() works", {

  result <- wasserportal:::get_wasserportal_stations_table()

  expect_true(kwb.utils::mainClass(result) == "tbl_df")
  expect_true("Messstellennummer" %in% names(result))

})
