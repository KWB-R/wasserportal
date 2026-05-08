# Get Groundwater Data

wrapper function to scrape all available raw data, i.e. groundwater
level and quality data and save in list

## Usage

``` r
get_groundwater_data(
  stations,
  groundwater_options = get_groundwater_options(),
  debug = TRUE,
  stations_list = NULL
)
```

## Arguments

- stations:

  list as retrieved by
  [`get_stations`](https://kwb-r.github.io/wasserportal/dev/reference/get_stations.md).
  Deprecated. Please use `stations_list` instead

- groundwater_options:

  as retrieved by
  [`get_groundwater_options`](https://kwb-r.github.io/wasserportal/dev/reference/get_groundwater_options.md)

- debug:

  print debug messages (default: TRUE)

- stations_list:

  list of station metadata as returned by
  [`get_stations`](https://kwb-r.github.io/wasserportal/dev/reference/get_stations.md)`(type = "list")`

## Value

list with elements "groundwater.level" and "groundwater.quality" data
frames

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- wasserportal::get_stations()
gw_data_list <- get_groundwater_data(stations)
str(gw_data_list)
} # }
```
