# Wasserportal Berlin: get stations overview table

Wasserportal Berlin: get stations overview table

## Usage

``` r
get_wasserportal_stations_table(
  type = get_overview_options()$groundwater$level,
  url_wasserportal = wasserportal_base_url()
)
```

## Arguments

- type:

  type of stations table to retrieve. Valid options defined in
  [`get_overview_options`](https://kwb-r.github.io/wasserportal/reference/get_overview_options.md),
  default: get_overview_options()\$groundwater\$level

- url_wasserportal:

  base url to Wasserportal berlin (default:
  [`wasserportal_base_url`](https://kwb-r.github.io/wasserportal/reference/wasserportal_base_url.md)

## Value

data frame with master data of selected monitoring stations

## Examples

``` r
types <- wasserportal::get_overview_options()
str(types)
#> List of 2
#>  $ surface_water:List of 8
#>   ..$ water_level         : chr "ows"
#>   ..$ flow                : chr "odf"
#>   ..$ temperature         : chr "owt"
#>   ..$ conductivity        : chr "olf"
#>   ..$ ph                  : chr "oph"
#>   ..$ oxygen_concentration: chr "oog"
#>   ..$ oxygen_saturation   : chr "oos"
#>   ..$ quality             : chr "opq"
#>  $ groundwater  :List of 2
#>   ..$ level  : chr "gws"
#>   ..$ quality: chr "gwq"
sw_l <- wasserportal::get_wasserportal_stations_table(type = types$surface_water$water_level)
str(sw_l)
#> tibble [113 × 10] (S3: tbl_df/tbl/data.frame)
#>  $ Messstellennummer: chr [1:113] "5865900" "5827103" "5870400" "5865300" ...
#>  $ Messstellenname  : chr [1:113] "Allee der Kosmonauten" "Allendestraße" "Alsenbrücke" "Am Bahndamm" ...
#>  $ Gewaesser        : chr [1:113] "M.-H.-Grenzgr." "Müggelspree" "Griebnitzkanal" "Wuhle" ...
#>  $ Betreiber        : chr [1:113] "Land Berlin" "Land Berlin" "Land Berlin" "Land Berlin" ...
#>  $ Datum            : chr [1:113] "04.06.2026 07:15" "03.03.2026" "04.06.2026" "25.01.2023 07:00" ...
#>  $ Wasserstand      : chr [1:113] "45.63" "32.30" "29.31" "32.31" ...
#>  $ Einheit          : chr [1:113] "m+NHN" "m+NHN" "m+NHN" "m+NHN" ...
#>  $ Ganglinien       : chr [1:113] "https://wasserportal.berlin.de/station.php?anzeige=g&thema=ows&station=5865900" "https://wasserportal.berlin.de/station.php?anzeige=g&thema=ows&station=5827103" "https://wasserportal.berlin.de/station.php?anzeige=g&thema=ows&station=5870400" "https://wasserportal.berlin.de/station.php?anzeige=g&thema=ows&station=5865300" ...
#>  $ Klassifikation   : chr [1:113] "niedrig" "inaktiv" "niedrig" "inaktiv" ...
#>  $ stammdaten_link  : chr [1:113] "https://wasserportal.berlin.de/station.php?anzeige=i&thema=ows&station=5865900" "https://wasserportal.berlin.de/station.php?anzeige=i&thema=ows&station=5827103" "https://wasserportal.berlin.de/station.php?anzeige=i&thema=ows&station=5870400" "https://wasserportal.berlin.de/station.php?anzeige=i&thema=ows&station=5865300" ...
```
