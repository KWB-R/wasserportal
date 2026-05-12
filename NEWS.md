# wasserportal 0.5.0.9000 <small>(development version)</small>

* Add `tb_setup_devices()`, `tb_push_station_telemetry()` and
  `tb_push_station_attributes()` for shipping Wasserportal time series
  and master data into a ThingsBoard tenant via the device-token
  telemetry API. `tb_setup_devices()` bootstraps a fresh tenant from an
  account-level API key, so the rest of the workflow runs from R alone
* Add `vignettes/thingsboard-demo.Rmd` walking through the
  ThingsBoard Cloud free-tier (Maker) demo on `eu.thingsboard.cloud`,
  including the switch to self-hosted Community Edition
* Add `inst/scripts/push_to_thingsboard.R` consuming the daily JSON
  artefacts on the `gh-pages` branch (no Wasserportal scrape of its
  own). The script picks the five groundwater stations with the
  longest combined gwl + gwq history and the most distinct gwq
  parameters, uploads merged master data as device attributes and
  pushes both the level and quality time series as telemetry
* Convert `Rechtswert_UTM_33_N` / `Hochwert_UTM_33_N`
  (ETRS89 / UTM zone 33N, EPSG:25833) to WGS84 `latitude` /
  `longitude` attributes so ThingsBoard map widgets work out of the
  box
* Add `.github/workflows/thingsboard-push.yaml` running the script on
  push to `main` / `master` / `dev`, daily at 07:00 UTC and via
  `workflow_dispatch`. Credentials are read from the `TB_HOST` and
  `TB_API_KEY` repository secrets
* Authenticate `tb_setup_devices()` with the `X-Authorization: ApiKey
  <key>` request header that ThingsBoard expects for account-level
  API keys (the standard `Authorization: Bearer ...` and the
  JWT-style `X-Authorization: Bearer ...` variants both return
  HTTP 401)
* Drop pre-1970 timestamps inside `build_telemetry_payload()`. Some
  Wasserportal groundwater stations start in the 1950s, which yields
  negative epoch milliseconds (the Unix/POSIX epoch is defined as
  1970-01-01 UTC, see
  [IEEE Std 1003.1, "4.16 Seconds Since the Epoch"](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html#tag_04_16)).
  ThingsBoard transports `ts` as a Java `Long` of epoch milliseconds
  (see the
  [HTTP Device API](https://thingsboard.io/docs/reference/http-api/#publish-telemetry-data)
  reference); negative values are spec-legal but the Maker free tier
  observed in this branch responds with an opaque HTTP 500 to such
  posts. Filtering `ts_ms > 0` keeps the rest of the (post-1970)
  history flowing through. For station 3 this drops about 17 years
  of monthly groundwater level readings while preserving the
  remaining ~7800 values
* Wire a `tb_error_body()` helper into `httr2::req_error(body = ...)`
  on the telemetry and attributes calls so future ThingsBoard
  failures surface the JSON `message` field in the R error instead
  of the generic "HTTP 500 Internal Server Error" wrapper
* Add `tb_push_latest_telemetry()` for the simplest
  `{"key": value}` form (server-stamped time). Used in
  `inst/scripts/push_to_thingsboard.R` as a smoke test before the
  bulk push: the bulk array-of-records form returns an opaque
  HTTP 500 on the ThingsBoard Cloud Maker free tier even though
  the same device accepts attribute writes and the simpler
  per-record format
* Add a `mode` parameter to `tb_push_station_telemetry()`
  (`"single"` by default, `"bulk"` for self-hosted CE). Single mode
  POSTs each record as a standalone `{"ts": ms, "values": {...}}`
  object so historical telemetry actually goes through on Maker
  free; bulk mode keeps the previous fast array-per-chunk
  behaviour for self-hosted CE
* Add a `throttle_seconds` parameter to
  `tb_push_station_telemetry()` so the inter-request sleep can be
  tuned per ThingsBoard plan instead of being hardcoded. `NULL`
  (default) keeps the previous values (50 ms in single mode, 100
  ms in bulk mode); pass a non-zero number to slow down or `0` to
  push as fast as the server permits (e.g. self-hosted CE)
* Add `tb_plan_defaults()` and a matching `TB_PLAN` env var so the
  GH-Actions push picks `mode`, `chunk_size` and `throttle_seconds`
  from the per-device transport rate limits documented at
  <https://thingsboard.io/docs/paas/eu/subscriptions/>. Presets:
  `free` -> `single` mode (proven to work end-to-end on the Maker
  free tier); `free-bulk` -> bulk preset for Free with
  `chunk_size = 10` / `throttle_seconds = 1.0`; **confirmed not to
  work** on the public Cloud Maker tier as of 2026-05 -- the
  gateway returns the same empty-body HTTP 500 to a 10-record
  array as it did to the original 100-record one, so the array
  form is rejected regardless of payload size. Kept as a
  reproducible baseline. `prototype` / `pilot` / `startup` /
  `business` ->
  `bulk` with `chunk_size = 30` / `throttle_seconds = 1.0`
  (~30 dp/s, near the 2 000 dp/min per-device cap shared across
  all paid tiers); `ce` -> unlimited bulk for self-hosted
  Community Edition. Add `TB_TELEMETRY_MODE`, `TB_CHUNK_SIZE` and
  `TB_THROTTLE_SECONDS` env vars on top of `TB_PLAN` so individual
  values can be overridden without switching plans
* Expose the plan and the per-run knobs as `workflow_dispatch`
  inputs in `thingsboard-push.yaml` (`plan`, `station_ids`,
  `history_days`, `telemetry_types`) and document the
  workflow_dispatch input -> repository secret -> hardcoded
  default fallback chain in a header comment of the env block.
  The default plan is `free` (single mode, proven to work);
  `free-bulk` is exposed as a workflow_dispatch option but stays
  out of the cron path until ThingsBoard lifts the Maker
  array-form rejection
* Drop the `tb_push_latest_telemetry()` "smoke test" that
  `inst/scripts/push_to_thingsboard.R` ran per device before the bulk
  telemetry push. The smoke test posted one value per station with
  `{"key": value}` (no timestamp -- server stamped with the current
  wall-clock time), originally as a fail-fast probe for the Maker
  free-tier auth/payload path. The visible side effect was a stale
  "GW-Stand = <last historical value> @ <push time>" row that drowned
  out the real most-recent measurement in the device's *Latest
  telemetry* view. The bulk historical push fails on its own first
  POST anyway, so the safety net was redundant. `tb_push_latest_telemetry()`
  itself stays as an exported helper for ad-hoc connectivity probes
* Add `tb_get_device_id()`, `tb_list_device_telemetry_keys()` and
  `tb_delete_device_telemetry()` for read-only device discovery and
  selective telemetry cleanup against the ThingsBoard plugin API
  (`GET /api/tenant/devices`, `GET /api/plugins/telemetry/DEVICE/{id}/keys/timeseries`,
  `DELETE /api/plugins/telemetry/DEVICE/{id}/timeseries/delete`). All
  three accept `TB_HOST` / `TB_API_KEY` from the environment so they
  can be called from a fresh R session without explicit credentials.
  Pass `keys = NULL` to `tb_delete_device_telemetry()` to wipe every
  key the device currently stores; server-side attributes (latitude,
  longitude, Bezirk, ...) are left in place so the map widget keeps
  working after a wipe. Stale rows from the now-removed smoke test
  can also be cleared interactively in the ThingsBoard UI
  (Device > Latest telemetry > tick the row > trash icon)
* Add `inst/extdata/thingsboard-dashboard.json`, an importable
  ThingsBoard dashboard for the demo: an OpenStreetMap of the
  five Berlin groundwater stations, a master-data entities
  table and two time-series charts (groundwater level, selected
  quality parameters). All four widgets discover the
  `wasserportal-gw-*` devices via an `entityName`-prefix alias so
  the import works without hardcoding device IDs. The
  dashboard-level timewindow uses a fixed four-year window from
  `2023-01-01 UTC` to `2027-01-01 UTC` so the charts load
  quickly (the previous 130-year `1970..2100` window made
  ThingsBoard show an indefinite loading spinner whenever the
  time-window was adjusted); widen it from the dashboard
  time-window selector if you need more history. The map widget
  uses the modern `typeFullFqn = "system.map"` reference together
  with the `latKeyName = "latitude"` / `lngKeyName = "longitude"`
  settings binding that the `system.map` widget accepts as a
  stable backward-compatible attribute mapping, so markers render
  right after import (an earlier `markers` array variant with
  `xKey` / `yKey` left the map empty against the same lat/lon
  attributes)
* Speed up `mode = "single"` with `httr2::req_perform_parallel()`.
  The previous sequential one-POST-at-a-time loop was network-bound
  at ~1.2 records/s for the GWQ push (~5 h per station for the full
  history); concurrent posting with `max_active = 10` lifts that to
  ~10 records/s. `tb_push_station_telemetry()` gains a `max_active`
  parameter; `tb_plan_defaults()` returns it per plan (default `10`
  for Free, `1` elsewhere); the script reads `TB_MAX_ACTIVE` from
  env / repo secrets through the same `env_or()` plan-fallback chain.
  Pace concurrent batches one-`max_active`-group at a time and retry
  on transient HTTP 500/502/503/504 with exponential backoff, so the
  Free tier's 600 messages/minute sustained per-device limit doesn't
  trip the gateway after ~35 s at 48 records/s (the symptom we hit
  with the initial implementation)
* Send one telemetry record per `(timestamp, key, value)` triple in
  `mode = "single"` instead of grouping every Parameter that
  shares a timestamp into a single record. Wasserportal
  groundwater quality data has ~30 analytes per sampling event;
  the resulting "fat" `values` dicts produced an opaque empty-body
  HTTP 500 on Cloud Maker even though the same keys went through
  one at a time (see `tb_push_latest_telemetry()` smoke tests).
  `build_telemetry_payload()` gains a `group_by_ts` parameter
  (default `TRUE`); the push function flips it off in single mode
  and keeps grouping in bulk mode for compact array chunks
* Sanitise telemetry keys before serialising the values dict.
  Wasserportal groundwater quality parameters such as
  `Leitfaehigkeit 25 grd C vor Ort`, `Wasserst. (ROK) vor`,
  `pH-Wert (Feld)` or `Temperatur (Wasser)` triggered an opaque
  HTTP 500 on the Maker free tier when used as raw JSON keys
  (after the level data already pushed cleanly). The new
  `sanitize_tb_key()` helper folds umlauts, drops parentheses
  and replaces spaces / dots / commas with underscores so quality
  data goes through too. Add a `TB_TELEMETRY_TYPES` env var
  (`"gwl,gwq"` by default) so a partial retry can skip the slow
  level re-push and only re-do the quality push

# [wasserportal 0.5.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.5.0) <small>2026-05-07</small>

* Modernize GitHub Actions workflows: use `r-lib/actions/setup-r-dependencies@v2`
  and `r-lib/actions/check-r-package@v2` on `ubuntu-latest` instead of the
  deprecated v2/`ubuntu-20.04`/`r-hub/sysreqs` toolchain
* Bump JavaScript actions to Node-24-compatible versions
  (`actions/checkout@v5`, `actions/upload-artifact@v5`) and set
  `FORCE_JAVASCRIPT_ACTIONS_TO_NODE24=true` so transitive `r-lib/actions/*@v2`
  steps opt into Node 24 as well, ahead of the June 2nd 2026 deprecation of
  Node 20 on GitHub Actions runners
* Add Claude Code review workflows (`claude.yaml`, `claude-code-review.yaml`)
* `get_wasserportal_master_data()`: match the new HTML5 markup of the
  master-data table (`<caption>Pegel Berlin</caption>` instead of the legacy
  `summary="Pegel Berlin"` attribute)
* Decode wasserportal pages explicitly as `windows-1252`. The pages declare
  UTF-8 in `<meta charset>` but the server actually emits Latin-1 bytes
  (e.g. `0xE4` for `ä`); trusting the meta declaration left those bytes
  mis-marked as UTF-8 and broke `subst_special_chars()`'s `ä→ae` /
  `ü→ue` substitutions on Windows R
* Bypass `rvest::html_table()` and `xml2::xml_text(trim = TRUE)` in
  `get_wasserportal_master_data()` and `get_wasserportal_stations_table()`:
  both delegate to a `sub("^[[:space:] ]+", ...)` pass that fails on Windows
  R when the cell text contains umlauts. Tables are now extracted directly
  via `xml2` and trimmed with a locale-safe `gsub(..., useBytes = TRUE)`
  helper (`trim_bytes()`)
* Make `get_stations()` and `get_wasserportal_masters_data()` resilient when
  parallel workers cannot fetch a station overview: load the `wasserportal`
  namespace into the cluster and drop `try-error` results before
  `data.table::rbindlist()` / `dplyr::left_join()`
* Make live-HTTP tests skip gracefully when `wasserportal.berlin.de` is
  unreachable from the test host (CRAN, sandboxed CI)
* Update `get_wasserportal_masters_data()` test expectations to include the
  new `Anmerkung` column that wasserportal added to surface-water master data

# [wasserportal 0.4.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.4.0) <small>2024-04-05</small>

* New feature: add support for downloading all available surface water quality 
data for one or multiple monitoring stations. For details see `get_surfacewater_qualities()` 
* Bugfix for groundwater level and quality due to new Wasserportal API 
* Add project [AD4GD](https://www.kompetenz-wasser.de/de/forschung/projekte/ad4gd) 
as funder


# [wasserportal 0.3.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.3.0) <small>2023-02-19</small>

* Fix errors in GitHub actions: use actions from branches `v2`, `v3`, not from 
  `master`
* Fix errors in tutorial.Rmd
* Fix errors in documentation
* Do not run examples that use parallel processing
* `get_stations()`: add argument `n_cores`
* `get_wasserportal_stations_table()`: Use new (three letter) variable codes 
* `read_wasserportal_raw()`: adapt request to new API version, add argument 
  `api_version`
* `read_wasserportal_raw_gw()`: adapt request to new API version
* Clean code, mainly to reduce duplicated duplication and to improve readability
  - check for more errors
  - use "safe" element selection
  - use more helper functions
  - use helper functions in vignettes
  - improve names

# [wasserportal 0.2.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.2.0) <small>2022-09-08</small>

* Add functions for exporting time series data to `zip` files (`wp_masters_data_to_list()`) 
and master data to `csv` files (`wp_timeseries_data_to_list()`), which will be 
uploaded to [https://kwb-r.github.io/wasserportal](https://kwb-r.github.io/wasserportal)/`<filename>`

* In addition `import` functions for downloading and importing the datasets above 
into R as lists were added (`list_timeseries_data_to_zip()`, `list_masters_data_to_csv()`)

* Code cleaning by `@hsonne` started

* Fix `master data` requests by using the `master_url` instead of `station_id`, 
as the latter was not unique. Now functions `get_wasserportal_master_data()` and 
it wrapper function `get_wasserportal_masters_data()` require the `master_url` 
instead of `station_id` as input parameter. The function `get_stations` now adds 
the column `stammdaten_link` as additional column for each sublist element of the 
sublist `overview_list`. 

* Fix to scrape  `groundwater level` data from all available monitoring stations (instead of only 5!) and export to `.csv` file. In addition switch also to `.csv`
export for `groundwater quality` instead of `.json` due to reduced storage space 
([stations_gwq_data.json](https://github.com/KWB-R/wasserportal/blob/gh-pages/stations_gwq_data.json) file is already 98.4 MB large.

* Add functions (`get_daily_surfacewater_data()`) and adapt article 
[Surface Water](../articles/surface-water.html) for scraping all available daily 
surface water data and exporting to one `.csv` file for each parameter (containing
all monitoring stations)

* Deactivate *PROMISCES* workflows (see [wasserportal v0.1.0](#wasserportal-010-2022-06-01)), due to failing Zenodo download. Will be 
moved into own R package, most properly [kwb.promisces](https://github.com/kwb-r/kwb.promisces).


# [wasserportal 0.1.1](https://github.com/KWB-R/wasserportal/releases/tag/v0.1.1) <small>2022-06-09</small>

* Fix bug in `get_wasserportal_stations_table()` now correctly naming parameter 
`temperature` (formerly incorrectly `level`)
* Fix [Surface Water](../articles/surface-water.html) article
* Adapt Zenodo DOI badge to cite always latest release

# [wasserportal 0.1.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.1.0) <small>2022-06-01</small>

R package for scraping `groundwater` data (`groundwater level` and `quality`) from [Wasserportal Berlin](https://wasserportal.berlin.de). Please note that the 
support for scraping `surface water` monitoring stations is currently very limited!

**Functions:**

* `get_stations()`: returns metadata for all available monitoring stations
* `get_wasserportal_masters_data()`: get master data for selected `station_ids` 
* `read_wasserportal_raw_gw()`: enables the download of `groundwater data`. 
Checkout the [Tutorial](../articles/tutorial.html) article how to use it for downloading [one](../articles/tutorial.html#download-and-plotting-one-station) or [multiple](../articles/tutorial.html#download-and-plotting-multiple-stations) 
stations at once.
* `read_wasserportal()`: works for `surface water` monitoring stations, but is 
outdated, as it is based on an outdated static file with station/variable names 
(i.e. only for `11` instead of `82` stations currently provided!) instead of 
relying on new metadata provided online. This will be fixed within the next release. For progress on this issue checkout [#21](https://github.com/KWB-R/wasserportal/issues/21)

**Workflows:**

- [Tutorial](../articles/tutorial.html) article how to download groundwater level 
and quality data


- **Further Usage** by combining previously scraped (see 
[tutorial](../articles/tutorial.html) above) data and performing some analysis: 

  * [Groundwater](../articles/groundwater.html), e.g. creating a map with GW level 
  trends
  
  * Two workflows ([REACH UBA](../articles/promisces_reach-uba.html), [Norman List](../articles/promisces_norman-lists.html)) created within the project [PROMISCES](https://www.kompetenz-wasser.de/en/forschung/projekte/promisces) for assessing  prevalence and the spatial distribution of **p**ersistent, **m**obile and **t**oxic (PMT) substances in the Berlin groundwater, based on different PMT lists, i.e. [REACH UBA](../articles/promisces_reach-uba.html) or [Norman List](../articles/promisces_norman-lists.html).

# wasserportal 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`


