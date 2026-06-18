# Push Time Series of one Wasserportal Station to ThingsBoard

Sends long-format measurements of a single Wasserportal monitoring
station to the device telemetry endpoint of a ThingsBoard instance
(`/api/v1/{token}/telemetry`). Works against ThingsBoard Cloud (e.g. the
free Maker tier on `https://thingsboard.cloud`), self-hosted ThingsBoard
Community Edition and `https://demo.thingsboard.io` since the
device-token API is identical on all of them.

## Usage

``` r
tb_push_station_telemetry(
  data,
  device_token,
  ts_col = "Datum",
  value_col = "Messwert",
  key_col = "Parameter",
  single_key = "value",
  host = tb_default_host(),
  chunk_size = 100L,
  mode = c("single", "bulk"),
  throttle_seconds = NULL,
  max_active = 10L,
  verbose = TRUE
)
```

## Arguments

- data:

  data frame for **one** station, in the long format produced by
  [`get_groundwater_data`](https://kwb-r.github.io/wasserportal/reference/get_groundwater_data.md)
  (columns `Messstellennummer`, `Datum`, `Parameter`, `Einheit`,
  `Messwert`) or
  [`get_daily_surfacewater_data`](https://kwb-r.github.io/wasserportal/reference/get_daily_surfacewater_data.md)
  (`Messstellennummer`, `Datum`, `Tagesmittelwert`, `Parameter`,
  `Einheit`).

- device_token:

  ThingsBoard device access token (taken from the device detail view in
  the ThingsBoard UI).

- ts_col:

  name of the timestamp column. Default `"Datum"`.

- value_col:

  name of the numeric value column. Default `"Messwert"` (set to
  `"Tagesmittelwert"` for surface water data).

- key_col:

  name of the column whose values become telemetry keys. Default
  `"Parameter"`. Set to `NULL` to push `value_col` itself under a single
  fixed key (see `single_key`).

- single_key:

  telemetry key used when `key_col` is `NULL`. Default `"value"`.

- host:

  base URL of the ThingsBoard instance, without trailing slash. Defaults
  to env var `TB_HOST` if set, otherwise `"https://thingsboard.cloud"`.

- chunk_size:

  maximum number of timestamps per HTTP POST when `mode = "bulk"`.
  Default `100`. Ignored in single mode.

- mode:

  one of `"single"` (default) or `"bulk"`. The Maker free tier on
  ThingsBoard Cloud rejects the bulk array form with an opaque HTTP 500
  even though the same device accepts the per-record
  `{"ts": ms, "values": {...}}` object; single mode therefore POSTs each
  record on its own. Use `"bulk"` against self-hosted CE for the much
  faster array-of-records form. See
  [`tb_plan_defaults`](https://kwb-r.github.io/wasserportal/reference/tb_plan_defaults.md)
  for plan-aware presets that pick `mode`, `chunk_size` and
  `throttle_seconds` together.

- throttle_seconds:

  inter-request sleep, in seconds, between consecutive HTTP POSTs.
  `NULL` (default) picks `0.05` for `mode = "single"` and `0.1` for
  `mode = "bulk"`. Increase to stay safely below the per-second /
  per-minute transport rate limits of the target ThingsBoard plan; set
  to `0` to push as fast as the server permits (e.g. self-hosted CE).

- max_active:

  number of concurrent HTTP POSTs in single mode (passed to
  [`httr2::req_perform_parallel()`](https://httr2.r-lib.org/reference/req_perform_parallel.html)).
  Default `10`, which stays below the ThingsBoard Cloud Free tier's 50
  messages/second per-device transport rate limit. Ignored in bulk mode.

- verbose:

  print one line per chunk in bulk mode and one line per parallel batch
  in single mode (default `TRUE`).

## Value

invisibly the number of telemetry timestamps that were sent.

## Details

Long-format input is pivoted on the fly so that every distinct value of
`key_col` becomes one telemetry key inside ThingsBoard, sharing the same
timestamp.

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- wasserportal::get_stations()
gw <- wasserportal::get_groundwater_data(stations)
one_station <- dplyr::filter(
  gw$groundwater.level,
  .data$Messstellennummer == "149"
)
tb_push_station_telemetry(
  data = one_station,
  device_token = Sys.getenv("TB_DEVICE_TOKEN_149")
)
} # }
```
