if (FALSE) {
library(wasserportal)

stations <- wasserportal::get_stations()
stations_crosstable <-  stations$crosstable

stations_crosstable_bb <- stations_crosstable %>%
  dplyr::filter(stringr::str_detect(.data$Messstellennummer,
                                    pattern = "^[A-Z]{2}_"))

stations_crosstable_berlin <- stations_crosstable %>%
  dplyr::filter(stringr::str_detect(.data$Messstellennummer,
                                    pattern = "^[A-Z]{2}_",
                                    negate = TRUE))



station_crosstable_berlin <- stations_crosstable_berlin[1,]
stations_crosstable_berlin
from_date <- "1900-01-01"
sw_station_berlin_daily <- wasserportal::read_wasserportal_raw(
  station = station_crosstable_berlin$Messstellennummer,
  variable = get_station_variables(stations_crosstable_berlin)[1],
  type = "daily",
  from_date = from_date,
  include_raw_time = TRUE,
  stations_crosstable = stations_crosstable
)

str(sw_station_berlin_daily)



sw_stations_berlin_daily <- stats::setNames(lapply(stations_crosstable_berlin$Messstellennummer,
                   function(station) {
                     msg <- sprintf("Fetching data for station '%s'", station)
                     kwb.utils::catAndRun(msg, expr = {
                     wasserportal::read_wasserportal(
  station = station,
  type = "daily",
  from_date = from_date,
  include_raw_time = TRUE,
  stations_crosstable = stations_crosstable
)})}
), nm = stations_crosstable$Messstellennummer)

str(sw_stations_daily)


}

