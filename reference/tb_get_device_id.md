# Look Up a ThingsBoard Device's UUID by Name

Lightweight read-only companion to
[`tb_setup_devices`](https://kwb-r.github.io/wasserportal/reference/tb_setup_devices.md):
when you only need a device's internal UUID (e.g. to call the
telemetry-delete endpoint), this returns it directly without touching
the access token or creating the device on the side. Returns
`NA_character_` when the device does not exist.

## Usage

``` r
tb_get_device_id(
  device_name,
  api_key = Sys.getenv("TB_API_KEY"),
  host = tb_default_host(),
  username = Sys.getenv("TB_USERNAME"),
  password = Sys.getenv("TB_PASSWORD")
)
```

## Arguments

- device_name:

  device name as shown in the ThingsBoard UI.

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

device UUID (character) or `NA_character_` if the lookup did not
resolve.

## Examples

``` r
if (FALSE) { # \dontrun{
tb_get_device_id("wasserportal-gw-6038")
} # }
```
