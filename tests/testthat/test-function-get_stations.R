#library(testthat)

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

  expect_identical(
    names(result_all),
    c("overview_list", "overview_df", "crosstable")
  )

  # It is possible that new data arrived since the two calls of the function...
  # Which check fails?

  # Drop the Datum column and every column that follows it: these contain
  # the most recent measurement value(s) and can change between two
  # consecutive scrapes (e.g. Wasserstand, Wassertemperatur,
  # Klassifikation, ...).
  remove_measurements <- function(x) {
    position_date <- which(names(x) == "Datum")
    if (length(position_date) == 0L) return(x)
    x[, seq_len(position_date[1L] - 1L), drop = FALSE]
  }

  # Compare the list versions (without measurement columns)
  x <- result_all[["overview_list"]]
  y <- result_list

  expect_identical(names(x), names(y))

  expect_true(all(sapply(names(x), function(name) identical(
    remove_measurements(x[[name]]),
    remove_measurements(y[[name]])
  ))))

  # Compare the data frame versions
  x <- result_all[["overview_df"]]
  y <- result_df

  expect_identical(names(x), names(y))

  # The wide overview_df merges measurement columns from all 10 station
  # types (Datum, Wasserstand, Wassertemperatur, Klassifikation, ...).
  # All of those are real-time values and can change between two scrapes.
  # Compare only the structural columns that are stable across calls.
  stable_columns <- c(
    "key",
    "Messstellennummer",
    "Messstellenname",
    "Betreiber",
    "stammdaten_link",
    "Ganglinie",
    "water_body",
    "variable",
    "station_type"
  )

  for (column in intersect(stable_columns, names(x))) {
    if (!identical(x[[column]], y[[column]])) {
      stop("difference in column '", column, "'")
    }
  }

  # Compare crosstable versions
  expect_identical(result_all[["crosstable"]], result_crosstable)
})
