test_that("get_stations() works", {

  f <- wasserportal:::get_stations

  expect_error(
    f(type = 1),
    regexp = "is.character\\(type\\)"
  )

  expect_error(
    f(type = "unsupported-type"),
    regexp = "all\\(type %in% expected_types\\)"
  )

  expect_error(
    f(type = c("list", "list")),
    regexp = "!anyDuplicated"
  )

  # Check output type "list"

  expect_output(result_list <- f(type = "list"))
  expect_type(result_list, "list")
  expect_true(all(grepl("^(surface_|ground)water", names(result_list))))

  # Check output type "data.frame"

  expected_names <- c(
    "key",
    "Messstellennummer",
    "Betreiber",
    "stammdaten_link"
  )

  expect_output(result_df <- f(type = "data.frame"))
  expect_true("data.frame" %in% class(result_df))
  expect_true(all(expected_names %in% names(result_df)))

  # Check output type "crosstable"

  expect_output(result_crosstable <- f(type = "crosstable"))
  expect_true("data.frame" %in% class(result_crosstable))
  expect_identical(unique(na.omit(unlist(result_crosstable[, -(1:2)]))), "x")

  # Check output of all types

  expect_output(result_all <- f())
  expect_identical(result_all, list(
    overview_list = result_list,
    overview_df = result_df,
    crosstable = result_crosstable
  ))

})
