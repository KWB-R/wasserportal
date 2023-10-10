test_that("get_wasserportal_master_data() works", {

  f <- wasserportal::get_wasserportal_master_data

  expect_error(f())
  expect_error(f("no-such-url"), "refers to an external")
  expect_error(f("https://wasserportal.berlin.de/no-such-url"), "error 404")

  # wasserportal::get_wasserportal_stations_table()$stammdaten_link[1L]
  url <- "https://wasserportal.berlin.de/station.php?anzeige=i&thema=gws&station=1"
  result <- f(url)

  expect_identical(names(result), c(
    "Nummer",
    "Bezirk",
    "Betreiber",
    "Auspraegung",
    "Grundwasserleiter",
    "Gelaendeoberkante_GOK_m_ue_NHN",
    "Rohroberkante_m_ue_NHN",
    "Filteroberkante_m_u_GOK",
    "Filterunterkante_m_u_GOK",
    "Rechtswert_UTM_33_N",
    "Hochwert_UTM_33_N"
  ))

  expect_identical(nrow(result), 1L)
})
