# Get Surface Water Quality for Multiple Monitoring Stations

Get Surface Water Quality for Multiple Monitoring Stations

## Usage

``` r
get_surfacewater_qualities(station_ids, dbg = TRUE)
```

## Arguments

- station_ids:

  vector with ids of multiple (or one) monitoring stations

- dbg:

  print debug messages (default: TRUE)

## Value

data frame with water quality data for multiple monitoring stations

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- wasserportal::get_stations()
station_ids <- stations$overview_list$surface_water.quality$Messstellennummer
swq <- wasserportal::get_surfacewater_qualities(station_ids)
str(swq)
} # }
```
