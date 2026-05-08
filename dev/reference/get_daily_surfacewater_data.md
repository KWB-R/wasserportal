# Get Daily Surfacewater Data: wrapper to scrape daily surface water data

Get Daily Surfacewater Data: wrapper to scrape daily surface water data

## Usage

``` r
get_daily_surfacewater_data(
  stations,
  variables = get_surfacewater_variables(),
  list2df = FALSE
)
```

## Arguments

- stations:

  stations as retrieved by by
  [`get_stations`](https://kwb-r.github.io/wasserportal/dev/reference/get_stations.md)

- variables:

  variables as retrieved by by
  [`get_surfacewater_variables`](https://kwb-r.github.io/wasserportal/dev/reference/get_surfacewater_variables.md)

- list2df:

  convert result list to data frame (default: FALSE)

## Value

list or data frame with all available data from Wasserportal

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- wasserportal::get_stations()
variables <- wasserportal::get_surfacewater_variables()
variables
sw_data_daily <- wasserportal::get_daily_surfacewater_data(stations, variables)
} # }
```
