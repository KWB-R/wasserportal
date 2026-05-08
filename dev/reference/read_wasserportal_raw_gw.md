# read_wasserportal_raw_gw

read_wasserportal_raw_gw

## Usage

``` r
read_wasserportal_raw_gw(
  station = 149,
  stype = "gws",
  type = "single_all",
  from_date = "",
  include_raw_time = FALSE,
  handle = NULL,
  as_text = FALSE,
  dbg = FALSE
)
```

## Arguments

- station:

  station id

- stype:

  "gws" or "gwq"

- type:

  "single" or "single_all" (if stype = "gwq")

- from_date:

  (default: "")

- include_raw_time:

  default: FALSE

- handle:

  default: NULL

- as_text:

  if TRUE, the raw text that is returned by the HTTP request to the
  Wasserportal is returned by this function. Otherwise (the default) the
  raw text is tried to be interpreted as comma separated values and a
  corresponding data frame is returned. Use as_text = TRUE to analyse
  the raw text in case that an error occurs when trying to convert the
  text to a data frame.

- dbg:

  logical indicating whether or not to show debug messages. The default
  is FALSE

## Value

data.frame with values

## Examples

``` r
if (FALSE) { # \dontrun{
read_wasserportal_raw_gw(station = 149, stype = "gws")
read_wasserportal_raw_gw(station = 149, stype = "gwq")
} # }
```
