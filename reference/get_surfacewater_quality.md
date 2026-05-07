# Get Surface Water Quality for One Monitoring Station

Get Surface Water Quality for One Monitoring Station

## Usage

``` r
get_surfacewater_quality(station_id)
```

## Arguments

- station_id:

  id of surface water measurement station

## Value

data frame with water quality data for one monitoring station

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- wasserportal::get_stations()
station_id <- stations$overview_list$surface_water.quality$Messstellennummer[1]
swq <- wasserportal::get_surfacewater_quality(station_id)
str(swq)
} # }
```
