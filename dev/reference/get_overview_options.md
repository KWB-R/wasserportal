# Wasserportal Berlin: get overview options for stations

Wasserportal Berlin: get overview options for stations

## Usage

``` r
get_overview_options()
```

## Value

list with shortcuts to station overview tables
(`wasserportal.berlin.de/messwerte.php?anzeige=tabelle&thema=<shortcut>`)

## Examples

``` r
get_overview_options()
#> $surface_water
#> $surface_water$water_level
#> [1] "ows"
#> 
#> $surface_water$flow
#> [1] "odf"
#> 
#> $surface_water$temperature
#> [1] "owt"
#> 
#> $surface_water$conductivity
#> [1] "olf"
#> 
#> $surface_water$ph
#> [1] "oph"
#> 
#> $surface_water$oxygen_concentration
#> [1] "oog"
#> 
#> $surface_water$oxygen_saturation
#> [1] "oos"
#> 
#> $surface_water$quality
#> [1] "opq"
#> 
#> 
#> $groundwater
#> $groundwater$level
#> [1] "gws"
#> 
#> $groundwater$quality
#> [1] "gwq"
#> 
#> 
```
