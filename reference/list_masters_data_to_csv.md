# Helper function: list masters data to csv

Helper function: list masters data to csv

## Usage

``` r
list_masters_data_to_csv(masters_data_list)
```

## Arguments

- masters_data_list:

  masters data in list form as retrieved by
  [`get_stations`](https://kwb-r.github.io/wasserportal/reference/get_stations.md)`(type = "list")`

## Value

loops through list of data frames and uses list names as filenames

## Examples

``` r
if (FALSE) { # \dontrun{
stations_list <- get_stations(type = "list")
masters_data_csv_files <- list_masters_data_to_csv(stations_list)
masters_data_csv_files
} # }
```
