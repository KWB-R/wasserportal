# Wasserportal Berlin: get master data for a single station

Wasserportal Berlin: get master data for a single station

## Usage

``` r
get_wasserportal_master_data(master_url)
```

## Arguments

- master_url:

  url with master data for single station as retrieved by
  [`get_wasserportal_stations_table`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_stations_table.md)

## Value

data frame with metadata for selected station

## Examples

``` r
if (FALSE) { # \dontrun{
stations_list <- wasserportal::get_stations(type = "list")

# GW Station
master_url <- stations_list %>%
  kwb.utils::selectElements("groundwater.level") %>%
  kwb.utils::selectColumns("stammdaten_link")[1L]

get_wasserportal_master_data(master_url)

# SW Station

# Reduce  to monitoring stations maintained by Berlin
master_urls <- stations_list %>%
  kwb.utils::selectElements("surface_water.water_level") %>%
  dplyr::filter(.data$Betreiber == "Land Berlin") %>%
  dplyr::pull(.data$stammdaten_link)

get_wasserportal_master_data(master_urls[1L])
} # }
```
