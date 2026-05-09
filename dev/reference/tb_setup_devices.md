# Create ThingsBoard Devices and Return their Access Tokens

Convenience wrapper for the initial setup against a fresh ThingsBoard
tenant. Uses an account-level **API key** (Bearer token) to:

1.  Create one device per name (or fetch the device if it already
    exists),

2.  Read each device's access token via the credentials endpoint.

The returned named character vector can be fed directly into
[`tb_push_station_telemetry`](https://kwb-r.github.io/wasserportal/dev/reference/tb_push_station_telemetry.md)
as `device_token`.

Generate the API key in the ThingsBoard UI under *Account \> Security \>
API keys \> Generate*.

## Usage

``` r
tb_setup_devices(
  station_ids,
  api_key = Sys.getenv("TB_API_KEY"),
  host = Sys.getenv("TB_HOST", unset = "https://thingsboard.cloud"),
  name_prefix = "wasserportal-",
  device_type = "wasserportal"
)
```

## Arguments

- station_ids:

  character vector of Wasserportal `Messstellennummer` values. Each
  becomes a ThingsBoard device named `paste0(name_prefix, station_id)`.

- api_key:

  account-level API key generated under *Account \> Security \> API keys
  \> Generate*. Sent in the `X-Authorization: ApiKey <key>` request
  header that ThingsBoard expects (not the standard
  `Authorization: Bearer ...`). Defaults to env var `TB_API_KEY`.

- host:

  base URL of the ThingsBoard instance, without trailing slash. Defaults
  to env var `TB_HOST` if set, otherwise `"https://thingsboard.cloud"`.
  Use `"https://eu.thingsboard.cloud"` for the EU cloud.

- name_prefix:

  prefix added in front of every station id when forming the ThingsBoard
  device name. Default `"wasserportal-"`.

- device_type:

  ThingsBoard device profile / type. Default `"wasserportal"`. The
  profile is auto-created on first use.

## Value

named character vector. Names are the input `station_ids`, values are
device access tokens.

## Examples

``` r
if (FALSE) { # \dontrun{
Sys.setenv(
  TB_HOST = "https://eu.thingsboard.cloud",
  TB_API_KEY = "<paste-your-API-key-here>"
)
tokens <- tb_setup_devices(c("149", "5867000", "5803900"))
tokens
} # }
```
