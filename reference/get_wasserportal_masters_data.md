# Wasserportal Berlin: get master data for a multiple stations

Wasserportal Berlin: get master data for a multiple stations

## Usage

``` r
get_wasserportal_masters_data(master_urls, run_parallel = TRUE)
```

## Arguments

- master_urls:

  URLs to master data as found in column "stammdaten_link" of the data
  frame returned by
  [`get_stations`](https://kwb-r.github.io/wasserportal/reference/get_stations.md)`(type = "list")`

- run_parallel:

  default: TRUE

## Value

data frame with metadata for selected master urls

## Examples

``` r
if (FALSE) { # \dontrun{
stations_list <- wasserportal::get_stations(type = "list")

# Reduce  to monitoring stations maintained by Berlin
master_urls <- stations_list$surface_water.water_level %>%
  dplyr::filter(.data$Betreiber == "Land Berlin") %>%
  dplyr::pull(.data$stammdaten_link)

system.time(master_parallel <- get_wasserportal_masters_data(
  master_urls
))

system.time(master_sequential <- get_wasserportal_masters_data(
  master_urls,
  run_parallel = FALSE
))
} # }
```
