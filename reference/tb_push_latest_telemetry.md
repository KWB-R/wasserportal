# Push a Single "Latest" Telemetry Record (no Timestamp)

Sends a flat `{"key": value, ...}` JSON object to the ThingsBoard
telemetry endpoint, letting the server stamp it with the current time.
This is the simplest possible telemetry POST and is useful both as a
smoke test for the device-token auth path and as a fallback when the
bulk-with-ts format is rejected (some ThingsBoard Cloud Maker tier
configurations return an opaque HTTP 500 to the array-of-records form
even though the same device accepts attributes and `latest`-style single
records).

## Usage

``` r
tb_push_latest_telemetry(
  values,
  device_token,
  host = tb_default_host()
)
```

## Arguments

- values:

  named list (or named numeric vector) of telemetry key/value pairs.

- device_token:

  ThingsBoard device access token.

- host:

  base URL of the ThingsBoard instance. Defaults to env var `TB_HOST` if
  set, otherwise `"https://thingsboard.cloud"`.

## Value

invisibly the number of keys that were sent.

## Examples

``` r
if (FALSE) { # \dontrun{
tb_push_latest_telemetry(
  values = list(`GW-Stand` = 35.6, `Wassertemperatur` = 11.2),
  device_token = Sys.getenv("TB_DEVICE_TOKEN")
)
} # }
```
