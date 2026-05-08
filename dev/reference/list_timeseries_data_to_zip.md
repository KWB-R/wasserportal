# Helper function: list timeseries data to zip

Helper function: list timeseries data to zip

## Usage

``` r
list_timeseries_data_to_zip(timeseries_data_list)
```

## Arguments

- timeseries_data_list:

  time series data in list form as retrieved by
  [`get_groundwater_data`](https://kwb-r.github.io/wasserportal/dev/reference/get_groundwater_data.md)

## Value

loops through list of data frames and uses list names as filenames

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- wasserportal::get_stations()

# Groundwater Time Series
gw_tsdata_list <- wasserportal::get_groundwater_data(stations)
gw_tsdata_files <- wasserportal::list_timeseries_data_to_zip(gw_tsdata_list)

# Surface Water Time Series
sw_tsdata_list <- wasserportal::get_daily_surfacewater_data(stations)
sw_tsdata_files <- wasserportal::list_timeseries_data_to_zip(sw_tsdata_list)
} # }
```
