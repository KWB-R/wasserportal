if (FALSE) {
library(wasserportal)

stations <- wasserportal::get_stations()

stations_crosstable <- stations$crosstable

station_crosstable <- stations_crosstable[1,]


read_wasserportal_raw(
  station = station_crosstable$Messstellennummer,
  variable = get_station_variables(station_crosstable)[2],
  type = "daily",
  from_date = from_date,
  include_raw_time = TRUE,
  stations_crosstable = stations_crosstable
)


sw_daily <- stats::setNames(lapply(stations_crosstable$Messstellennummer,
                   function(station) {
                     read_wasserportal(
  station = station,
  type = "daily",
  from_date = from_date,
  include_raw_time = TRUE,
  stations_crosstable = stations_crosstable
)}
), nm = stations_crosstable$Messstellennummer)
}
