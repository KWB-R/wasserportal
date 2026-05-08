# Download and Read Data from wasserportal.berlin.de

This function downloads and reads CSV files from wasserportal.berlin.de.

## Usage

``` r
read_wasserportal(
  station,
  variables = NULL,
  from_date = as.character(Sys.Date() - 90L),
  type = "single",
  include_raw_time = FALSE,
  stations_crosstable
)
```

## Arguments

- station:

  station number, as found in column "Messstellennummer" of the data
  frame returned by
  [`get_stations`](https://kwb-r.github.io/wasserportal/dev/reference/get_stations.md)`(type = "crosstable")`

- variables:

  vector of variable identifiers, as returned by
  [`get_station_variables`](https://kwb-r.github.io/wasserportal/dev/reference/get_station_variables.md)

- from_date:

  `Date` object (or string in format "yyyy-mm-dd" that can be converted
  to a `Date` object representing the first day for which to request
  data. Default: `as.character(Sys.Date() - 90L)`

- type:

  one of "single" (the default), "daily", "monthly"

- include_raw_time:

  if `TRUE` the original time column and the column with the corrected
  winter time are included in the output. The default is `FALSE`.

- stations_crosstable:

  data frame as returned by
  [`get_stations`](https://kwb-r.github.io/wasserportal/dev/reference/get_stations.md)`(type = "crosstable")`

## Value

data frame read from the CSV file that the download provides. IMPORTANT:
It is not yet clear how to interpret the timestamp, see example

## Details

The original timestamps (column `timestamps_raw` in the example below)
are not all plausible, e.g. "31.03.2019 03:00" appears twice! They are
corrected (column `timestamp_corr`) to represent a plausible sequence of
timestamps in Berlin Normal Time (UTC+01) Finally, a valid POSIXct
timestamp in timezone "Berlin/Europe" (UTC+01 in winter, UTC+02 in
summer) is created, together with the additional information on the UTC
offset (column `UTCOffset`, 1 in winter, 2 in summer).

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a list of available water quality stations and variables
stations_crosstable <- wasserportal::get_stations(type = "crosstable")

# Set the start date
from_date <- "2021-03-01"

# Read the timeseries (multiple variables for one station)
water_quality <- wasserportal::read_wasserportal(
  station = stations_crosstable$Messstellennummer[1L],
  from_date = from_date,
  include_raw_time = TRUE,
  stations_crosstable = stations_crosstable
)

# Look at the first few records
head(water_quality)

# Check the metadata
#kwb.utils::getAttribute(water_quality, "metadata")

# Set missing values to NA
water_quality[water_quality == -777] <- NA

# Look at the first few records again
head(water_quality)

### How was the original timestamp interpreted?

# Determine the days at which summer time starts and ends, respectively
from_year <- as.integer(substr(from_date, 1L, 4L))
switches <- kwb.datetime::date_range_CEST(from_year)

# Reformat to dd.mm.yyyy
switches <- kwb.datetime::reformatTimestamp(switches, "%Y-%m-%d", "%d.%m.%Y")

# Define a pattern to look for timestamps "around" the switches
pattern <- paste(switches, "0[1-4]", collapse = "|")

# Look at the data for these timestamps
water_quality[grepl(pattern, water_quality$timestamp_raw), ]

# The original timestamps (timestamps_raw) were not all plausible, e.g.
# for March 2019. This seems to have been fixed by the "wasserportal"!
sum(water_quality$timestamp_raw != water_quality$timestamp_corr)
} # }
```
