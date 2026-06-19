# List the Telemetry Keys Currently Stored for a ThingsBoard Device

Wraps `GET /api/plugins/telemetry/DEVICE/{id}/keys/timeseries`. Useful
to discover what's actually in the device-side time-series store before
a wipe, or to compare against the `Parameter` column of the gh-pages
source data.

## Usage

``` r
tb_list_device_telemetry_keys(
  device_id,
  api_key = Sys.getenv("TB_API_KEY"),
  host = tb_default_host(),
  username = Sys.getenv("TB_USERNAME"),
  password = Sys.getenv("TB_PASSWORD")
)
```

## Arguments

- device_id:

  device UUID. Use
  [`tb_get_device_id`](https://kwb-r.github.io/wasserportal/reference/tb_get_device_id.md)
  to resolve a name.

- api_key:

  account-level API key. Defaults to env var `TB_API_KEY`.

- host:

  base URL of the ThingsBoard instance. Defaults to env var `TB_HOST` if
  set, otherwise `"https://thingsboard.cloud"`.

- username:

  ThingsBoard user for the username/password (JWT) login (self-hosted /
  Community Edition). Defaults to env var `TB_USERNAME`.

- password:

  ThingsBoard password. Defaults to env var `TB_PASSWORD`.

## Value

character vector of telemetry key names. May be of length 0.

## Examples

``` r
if (FALSE) { # \dontrun{
id <- tb_get_device_id("wasserportal-gw-6038")
tb_list_device_telemetry_keys(id)
} # }
```
