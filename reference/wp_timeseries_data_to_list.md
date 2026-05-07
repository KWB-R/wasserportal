# Wasserportal Time Series Data: download and Import in R List

Wasserportal Time Series Data: download and Import in R List

## Usage

``` r
wp_timeseries_data_to_list(
  overview_list_names,
  target_dir = tempdir(),
  is_zipped = TRUE
)
```

## Arguments

- overview_list_names:

  names of elements in the list returned by
  [`get_stations`](https://kwb-r.github.io/wasserportal/reference/get_stations.md)`(type = "list")`

- target_dir:

  target directory for downloading data (default: tempdir())

- is_zipped:

  are the data to be downloaded zipped (default: TRUE)

## Value

downloads (zipped) data from wasserportal

## Examples

``` r
if (FALSE) { # \dontrun{
overview_list_names <- names(wasserportal::get_stations(type = "list"))
wp_timeseries_data_list <- wp_timeseries_data_to_list(overview_list_names)
} # }
```
