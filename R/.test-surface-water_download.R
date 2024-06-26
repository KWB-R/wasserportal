if (FALSE)
{
  `%>%` <- magrittr::`%>%`

  stations_crosstable <- wasserportal::get_stations(type = "crosstable")

  stations_crosstable_bb <- stations_crosstable %>%
    dplyr::filter(stringr::str_detect(
      .data$Messstellennummer,
      pattern = "^[A-Z]{2}_"
    ))

  stations_crosstable_berlin <- stations_crosstable %>%
    dplyr::filter(stringr::str_detect(
      .data$Messstellennummer,
      pattern = "^[A-Z]{2}_",
      negate = TRUE
    ))

  stations_crosstable_berlin

  from_date <- "1900-01-01"

  sw_station_berlin_daily <- wasserportal::read_wasserportal_raw(
    station = stations_crosstable_berlin[1L, ] %>%
      kwb.utils::selectColumns("Messstellennummer"),
    variable = wasserportal::get_station_variables(stations_crosstable_berlin)[1],
    type = "daily",
    from_date = from_date,
    include_raw_time = TRUE,
    stations_crosstable = stations_crosstable
  )

  str(sw_station_berlin_daily)

  sw_stations_berlin_daily <- stations_crosstable_berlin %>%
    kwb.utils::selectColumns("Messstellennummer") %>%
    lapply(function(station) cat_and_run(
      sprintf("Fetching data for station '%s'", station),
      expr = wasserportal::read_wasserportal(
        station = station,
        type = "daily",
        from_date = from_date,
        include_raw_time = TRUE,
        stations_crosstable = stations_crosstable
      )
    )) %>%
    stats::setNames(
      kwb.utils::selectColumns(stations_crosstable, "Messstellennummer")
    )

  str(sw_stations_daily)
}
