# Create ThingsBoard Devices and Return their Access Tokens

Convenience wrapper for the initial setup against a fresh ThingsBoard
tenant. Authenticates with either a username/password login (JWT – works
on every ThingsBoard edition, required for self-hosted Community
Edition) or an account-level API key (ThingsBoard Cloud), then:

1.  Create one device per name (or fetch the device if it already
    exists),

2.  Read each device's access token via the credentials endpoint.

The returned named character vector can be fed directly into
[`tb_push_station_telemetry`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md)
as `device_token`.

## Usage

``` r
tb_setup_devices(
  station_ids,
  api_key = Sys.getenv("TB_API_KEY"),
  host = tb_default_host(),
  name_prefix = "wasserportal-",
  device_type = "wasserportal",
  username = Sys.getenv("TB_USERNAME"),
  password = Sys.getenv("TB_PASSWORD")
)
```

## Arguments

- station_ids:

  character vector of Wasserportal `Messstellennummer` values. Each
  becomes a ThingsBoard device named `paste0(name_prefix, station_id)`.

- api_key:

  account-level API key (ThingsBoard Cloud only), generated under
  *Account \> Security \> API keys \> Generate*. Sent in the
  `X-Authorization: ApiKey <key>` request header. Defaults to env var
  `TB_API_KEY`. Ignored when `username` and `password` are supplied.

- host:

  base URL of the ThingsBoard instance, without trailing slash. Defaults
  to env var `TB_HOST` if set, otherwise `"https://thingsboard.cloud"`.
  Use `"https://eu.thingsboard.cloud"` for the EU cloud or e.g.
  `"https://dashboards.inowas.org"` for a self-hosted instance.

- name_prefix:

  prefix added in front of every station id when forming the ThingsBoard
  device name. Default `"wasserportal-"`.

- device_type:

  ThingsBoard device profile / type. Default `"wasserportal"`. The
  profile is auto-created on first use.

- username:

  ThingsBoard user for the username/password (JWT) login. Defaults to
  env var `TB_USERNAME`. When set together with `password` it takes
  precedence over `api_key` – this is the route to use for self-hosted
  Community Edition.

- password:

  ThingsBoard password. Defaults to env var `TB_PASSWORD`.

## Value

named character vector. Names are the input `station_ids`, values are
device access tokens.

## Details

Set `TB_USERNAME` + `TB_PASSWORD` for the login route, or generate an
API key in the ThingsBoard Cloud UI under *Account \> Security \> API
keys \> Generate* and set `TB_API_KEY`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Self-hosted ThingsBoard Community Edition (username/password login):
Sys.setenv(
  TB_HOST = "https://dashboards.inowas.org",
  TB_USERNAME = "me@example.org",
  TB_PASSWORD = "secret"
)
tokens <- tb_setup_devices(c("149", "5867000", "5803900"))

# ThingsBoard Cloud (account API key):
Sys.setenv(
  TB_HOST = "https://eu.thingsboard.cloud",
  TB_API_KEY = "<paste-your-API-key-here>"
)
tokens <- tb_setup_devices(c("149", "5867000", "5803900"))
} # }
```
