# Wasserportal Master Data: download and Import in R List

Wasserportal Master Data: download and Import in R List

## Usage

``` r
wp_masters_data_to_list(
  overview_list_names,
  target_dir = tempdir(),
  file_prefix = "stations_",
  is_zipped = FALSE
)
```

## Arguments

- overview_list_names:

  names of elements in the list returned by
  [`get_stations`](https://kwb-r.github.io/wasserportal/dev/reference/get_stations.md)`(type = "list")`

- target_dir:

  target directory for downloading data (default: tempdir())

- file_prefix:

  prefix given to file names

- is_zipped:

  are the data to be downloaded zipped (default: FALSE)

## Value

downloads csv master data from Wasserportal

## Examples

``` r
if (FALSE) { # \dontrun{
overview_list_names <- names(wasserportal::get_stations(type = "list"))
wp_masters_data_list <- wp_masters_data_to_list(overview_list_names)
} # }
```
