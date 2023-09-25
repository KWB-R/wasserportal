test_that("clean_timestamp_columns() works", {

  f <- wasserportal:::clean_timestamp_columns

  expect_error(f())
  expect_error(f(data.frame(no_such_column = 1)))
  expect_error(f(data.frame(Datum = 1)))

  data <- data.frame(Datum = "24.09.2023 12:00")

  result <- f(data, include_raw_time = FALSE)

  expect_identical(result, data.frame(
    LocalDateTime = data$Datum %>%
      as.POSIXct(format = "%d.%m.%Y %H:%M", tz = "Etc/GMT-1") %>%
      structure(tzone = "Europe/Berlin")
  ))

})
