#library(testthat)

test_that("get_wasserportal_masters_data() works", {

  f <- wasserportal:::get_wasserportal_masters_data

  expect_error(f())

  # Ask for a non-existing URL
  expect_message(capture.output(result <- f("no-such-url")), "Failed")
  expect_identical(dim(result), c(0L, 0L))

  skip_if_wasserportal_unreachable()

  # Find URLs for testing
  # urls <- wasserportal::get_stations("list") %>%
  #   kwb.utils::selectElements("surface_water.water_level") %>%
  #   dplyr::filter(Betreiber == "Land Berlin") %>%
  #   dplyr::pull(stammdaten_link)

  url <- "https://wasserportal.berlin.de/station.php?anzeige=i&thema=ows&station=5866301"

  expect_output(result <- f(url, run_parallel = FALSE), "Importing master data for 1")

  skip_if(nrow(result) == 0L, "wasserportal returned no master data for test station")

  expect_identical(names(result), c(
    "Nummer",
    "Name",
    "Gewaesser",
    "Betreiber",
    "Auspraegung",
    "Flusskilometer",
    "Pegelnullpunkt_m_NHN",
    "Rechtswert_UTM_33_N",
    "Hochwert_UTM_33_N"
  ))

})
