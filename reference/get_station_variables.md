# Helper function: get available station variables

Helper function: get available station variables

## Usage

``` r
get_station_variables(station_df)
```

## Arguments

- station_df:

  data frame with one row per station and columns "Messstellennummer",
  "Messstellenname" and additional columns each of which represents a
  variable that is measured at that station. If the variable columns
  contain the value "x" it means that the corresponding variable is
  measured and the name of the column is contained in the returned
  vector of variable names.

## Value

returns names of available variables for station
