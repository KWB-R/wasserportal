# Delete All Time-Series Data for Selected Keys on a ThingsBoard Device

Wipes historical telemetry rows from ThingsBoard for the given device
and keys via
`DELETE /api/plugins/telemetry/DEVICE/{id}/timeseries/delete`. Pass
`keys = NULL` (the default) to clear every key the device currently
knows – the function then calls
[`tb_list_device_telemetry_keys`](https://kwb-r.github.io/wasserportal/dev/reference/tb_list_device_telemetry_keys.md)
first to discover them.

## Usage

``` r
tb_delete_device_telemetry(
  device_id,
  keys = NULL,
  api_key = Sys.getenv("TB_API_KEY"),
  host = Sys.getenv("TB_HOST", unset = "https://thingsboard.cloud"),
  delete_latest = TRUE
)
```

## Arguments

- device_id:

  device UUID.

- keys:

  character vector of telemetry keys to delete, or `NULL` to clear every
  key the device currently knows.

- api_key:

  account-level API key. Defaults to env var `TB_API_KEY`.

- host:

  base URL of the ThingsBoard instance. Defaults to env var `TB_HOST` if
  set, otherwise `"https://thingsboard.cloud"`.

- delete_latest:

  if `TRUE` (default) ThingsBoard also drops the cached "latest
  telemetry" entry so the device-detail tab in the UI immediately
  reflects the deletion. Set to `FALSE` to keep the latest value visible
  for keys that get repopulated by the next push anyway.

## Value

invisibly the number of keys submitted for deletion.

## Details

Server-side attributes (latitude, longitude, Bezirk, ...) and the device
itself are NOT touched, only the time-series telemetry store. Re-running
the demo push afterwards re-fills the cleared keys with the real
Wasserportal timestamps.

## Examples

``` r
if (FALSE) { # \dontrun{
id <- tb_get_device_id("wasserportal-gw-6038")
# wipe everything currently stored:
tb_delete_device_telemetry(id)
# wipe just the smoke-test GW-Stand value:
tb_delete_device_telemetry(id, keys = "GW-Stand")
} # }
```
