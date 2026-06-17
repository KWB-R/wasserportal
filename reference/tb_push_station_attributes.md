# Push Static Attributes of one Wasserportal Station to ThingsBoard

Sends station metadata (coordinates, level reference, operator, ...) as
client-side attributes to the ThingsBoard device attributes endpoint
(`/api/v1/{token}/attributes`). Attributes are key/value pairs without a
timestamp; ThingsBoard overwrites the previous value on every push.

## Usage

``` r
tb_push_station_attributes(
  attributes,
  device_token,
  host = Sys.getenv("TB_HOST", unset = "https://thingsboard.cloud")
)
```

## Arguments

- attributes:

  named list (or a one-row data frame, which is converted to a list).
  All entries must be JSON-serialisable scalars.

- device_token:

  ThingsBoard device access token.

- host:

  base URL of the ThingsBoard instance. Defaults to env var `TB_HOST` if
  set, otherwise `"https://thingsboard.cloud"`.

## Value

invisibly the number of attributes that were sent.

## Examples

``` r
if (FALSE) { # \dontrun{
tb_push_station_attributes(
  attributes = list(
    name = "Pegel Mueggelheim",
    latitude = 52.4291,
    longitude = 13.6450,
    pegelnullpunkt_m_NHN = 32.18
  ),
  device_token = Sys.getenv("TB_DEVICE_TOKEN_5867000")
)
} # }
```
