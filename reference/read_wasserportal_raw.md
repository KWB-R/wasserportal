# Read Wasserportal Raw

Read Wasserportal Raw

## Usage

``` r
read_wasserportal_raw(
  variable,
  station,
  from_date,
  type = "single",
  include_raw_time = FALSE,
  handle = NULL,
  stations_crosstable,
  api_version = 2L
)
```

## Arguments

- variable:

  variable

- station:

  station id

- from_date:

  start date

- type:

  one of "single", "daily", "monthly" (default: "single")

- include_raw_time:

  TRUE or FALSE (default: FALSE)

- handle:

  handle (default: NULL)

- stations_crosstable:

  data frame as returned by
  [`get_stations`](https://kwb-r.github.io/wasserportal/reference/get_stations.md)`(type = "crosstable")`

- api_version:

  1 integer number representing the version of wasserportal's API. 1L:
  before 2023, 2L: since 2023. Default: 2L

## Value

????
