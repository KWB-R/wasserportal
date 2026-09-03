# Changelog

## [wasserportal 0.7.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.7.0) 2026-06-19

- Add
  [`tb_login()`](https://kwb-r.github.io/wasserportal/reference/tb_login.md)
  and username/password (JWT) authentication across the ThingsBoard
  tenant-API helpers
  ([`tb_setup_devices()`](https://kwb-r.github.io/wasserportal/reference/tb_setup_devices.md),
  [`tb_get_device_id()`](https://kwb-r.github.io/wasserportal/reference/tb_get_device_id.md),
  [`tb_list_device_telemetry_keys()`](https://kwb-r.github.io/wasserportal/reference/tb_list_device_telemetry_keys.md),
  [`tb_delete_device_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_delete_device_telemetry.md)).
  Self-hosted ThingsBoard Community Edition
  (e.g. `https://dashboards.inowas.org`) has no account-level API keys –
  that is a ThingsBoard Cloud convenience – so it can only be reached
  via `POST /api/auth/login` (username + password), which returns a
  short-lived JWT sent as `X-Authorization: Bearer <token>`. Each helper
  now resolves its auth header via an internal `tb_auth_header()`: set
  `TB_USERNAME` + `TB_PASSWORD` (these take precedence over `TB_API_KEY`
  when both are present) and, for self-hosted instances, `TB_PLAN=ce`
  (bulk mode, no throttling). The account-level API-key path
  (ThingsBoard Cloud) keeps working unchanged, and the device-token
  telemetry push (`/api/v1/{token}/telemetry`) is identical on all
  editions. The `thingsboard-push.yaml` workflow reads the two new
  credentials from the `TB_USERNAME` / `TB_PASSWORD` repository secrets.
- Make the station selection of `inst/scripts/push_to_thingsboard.R`
  configurable for full (non-demo) pushes: `TB_MAX_DEVICES=0` lifts the
  5-device cap (push every candidate station), and a new
  `TB_STATION_SCOPE` chooses which groundwater stations qualify – `both`
  (default: level AND quality, the proven demo set), `any` (level OR
  quality), `gwl` / `gwq` (has that series, possibly both) or `gwl-only`
  / `gwq-only` (has only that series). Both knobs are exposed as
  `thingsboard-push.yaml` repository secrets and `workflow_dispatch`
  inputs. Distinct gwq parameters per station are now counted once via
  [`split()`](https://rdrr.io/r/base/split.html) +
  [`vapply()`](https://rdrr.io/r/base/lapply.html) instead of a
  per-station table rescan, so scoring the full several-hundred-station
  pool stays fast. The helper returns a plain named integer vector
  (matching the idiom already used in `R/get_stations.R` and
  `R/inspect_gh_pages_zips.R`) instead of the 1-D array that an
  intermediate [`tapply()`](https://rdrr.io/r/base/tapply.html)
  implementation produced.
- Update the Kompetenzzentrum Wasser Berlin (KWB) author logo in
  `_pkgdown.yml` to the new brand asset
  (`logos.kompetenz-wasser.io/KWB_Logo_M_Blau_RGB.svg`).
- Declare `Depends: R (>= 4.1.0)` – the package (notably
  `R/push_to_thingsboard.R` and `inst/scripts/push_to_thingsboard.R`)
  uses the native `|>` pipe, which `R CMD check` otherwise flags as an
  undeclared dependency – and drop the unused `LazyData` field (there is
  no `data/` directory, so `R CMD build` omitted it anyway).
- Warn in `tb_auth_header()` when only one of `TB_USERNAME` /
  `TB_PASSWORD` is set and a leftover `TB_API_KEY` causes a silent
  fallback to the Cloud API-key path. The typical misconfiguration
  (workflow secret missing on one of the two JWT credentials with a
  stale `TB_API_KEY` still populated) used to surface only as a generic
  `auth: account API key` log line; the new
  [`warning()`](https://rdrr.io/r/base/warning.html) calls out the
  misconfiguration so the user can fix the missing secret instead of
  chasing a wrong-credentials failure further downstream. The pure Cloud
  (only `TB_API_KEY`) and pure JWT (both `TB_USERNAME` and
  `TB_PASSWORD`) paths stay quiet, and the existing
  [`stop()`](https://rdrr.io/r/base/stop.html) for the no-credentials
  case is unchanged.
- Clarify the station-selection diagnostic in
  `inst/scripts/push_to_thingsboard.R`: the row previously labelled
  `with gwl AND gwq` is now `in both master files AND has both series`
  because it is the only row that intersects `master_gwl` and
  `master_gwq` (strict), while the per-series rows (`with gwl data`,
  `with gwq data`) count against the union of the two master files
  (relaxed). Stations that appear in only one master file but have rows
  in both gwl and gwq data were being counted in the per-series totals
  but not in the intersect total, so the displayed numbers did not add
  up the way a reader expected when the two master files don’t perfectly
  overlap. An inline comment documents the intentional asymmetry.
- Make
  [`tb_login()`](https://kwb-r.github.io/wasserportal/reference/tb_login.md)
  more robust against flaky upstreams: widen the retry predicate from
  the `httr2` default (HTTP 429 / 503 only) to
  `{408, 429, 500, 502, 503, 504}` and bump `max_tries` from 3 to 4,
  matching the predicate already used by
  [`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md).
  `POST /api/auth/login` is idempotent, so retrying is safe; this keeps
  [`tb_setup_devices()`](https://kwb-r.github.io/wasserportal/reference/tb_setup_devices.md)
  from aborting on a cold-start 500 / 502 / 504 from a self-hosted
  ThingsBoard sitting behind nginx or a load balancer. Also document the
  trade-off that a non-2xx response surfaces an excerpt of the server’s
  response body (via `tb_error_body()`, up to ~800 chars) in the R error
  and `req_retry()` retry messages – stock ThingsBoard only echoes the
  error description, so the password does not leak, but operators of
  self-hosted instances whose reverse proxy echoes request fields back
  in the error body should mask the relevant secrets in their CI config.
- Validate the numeric `TB_*` environment variables (`TB_MAX_DEVICES`,
  `TB_HISTORY_DAYS`, `TB_CHUNK_SIZE`, `TB_THROTTLE_SECONDS`,
  `TB_MAX_ACTIVE`) up front in `inst/scripts/push_to_thingsboard.R` and
  abort with a clear message when a value is not a number, instead of
  letting an `NA` crash a downstream `if (x > 0)` only after every
  device attribute set has already been pushed. The message points out
  the usual cause: `.Renviron` does not support inline `# comments`, so
  `TB_HISTORY_DAYS = 7 # ...` otherwise coerces to `NA`.
- Clean up the public signature of
  [`tb_list_device_telemetry_keys()`](https://kwb-r.github.io/wasserportal/reference/tb_list_device_telemetry_keys.md)
  by dropping the `auth` argument that 0.7.0 had briefly added for the
  chained-call case. The single in-package consumer
  ([`tb_delete_device_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_delete_device_telemetry.md))
  now calls an internal
  `tb_list_device_telemetry_keys_impl(device_id, auth, host)` that takes
  the pre-resolved `X-Authorization` header, so the one-round-trip
  saving is preserved without mixing “pass me credentials” and “skip
  credentials, here’s the header” in the same exported function. Removes
  the silent precedence where `auth = "Bearer ..."` together with
  `api_key` / JWT credentials would have ignored the latter without
  warning.
- Improve the station-selection block of
  `inst/scripts/push_to_thingsboard.R` in two ways. First, flag orphan
  stations – IDs that have rows in `gwl_data` / `gwq_data` but are
  missing from both master files – with a
  [`message()`](https://rdrr.io/r/base/message.html) listing the count
  and the first few IDs. Every scope intersects its candidate set with
  `master_union`, so those orphans are silently dropped from the
  candidate pool; without the message a master / data drift would be
  invisible in the diagnostic counts. Second, add a new
  `in either master AND has both series` row to the diagnostic block
  (computed as `master_union ∩ ids_gwl ∩ ids_gwq`) so that the row-sum
  identity `with_gwl + with_gwq - both = only_gwl + only_gwq + both`
  actually holds for readers scanning the message; the existing strict
  row is renamed to `strict: in both masters AND both series` and gets
  an inline `(strict: master_intersect)` annotation so the intentional
  asymmetry against `master_union` stays visible.

## [wasserportal 0.6.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.6.0) 2026-06-17

- Wrap each
  [`httr2::req_perform_parallel()`](https://httr2.r-lib.org/reference/req_perform_parallel.html)
  batch in
  [`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md)
  `mode = "single"` in a batch-level retry loop (4 attempts with
  `2 / 4 / 8 s` backoff). The per-request `retry_on_failure = TRUE`
  added in the previous bullet recovers from a curl-level error on a
  *fresh* libcurl handle, but when the upstream load balancer silently
  drops a connection in the curl pool the dead handle stays poisoned
  across all four configured per-request retries: every retry hits the
  same dead handle and dies with “Send failure: Broken pipe” within
  milliseconds, the resulting curl condition bubbles up through
  `req_perform_parallel()` and aborts the whole station (observed in the
  wild after only ~2240/13039 records on station 7045 on 2026-05-13
  09:45, 3 s between last good POST and the abort – no perceptible retry
  pause). Retrying the *batch* as a whole forces httr2 to allocate a new
  connection on the next attempt and is safe because the underlying
  `(ts, key)` telemetry POSTs are idempotent on the ThingsBoard side – a
  re-POST of an already accepted record overwrites itself with the same
  value, never creates a duplicate row.

- Pass `retry_on_failure = TRUE` to every
  [`httr2::req_retry()`](https://httr2.r-lib.org/reference/req_retry.html)
  call in `R/push_to_thingsboard.R` (single-mode and bulk telemetry,
  attributes, latest telemetry, telemetry delete). The default
  `req_retry()` only retries HTTP responses with selected status codes;
  transport-layer dropouts that error out before the request produces a
  response (TCP “Broken pipe”, peer-closed TLS session, brief DNS
  hiccups) used to bubble straight up through
  [`httr2::req_perform_parallel()`](https://httr2.r-lib.org/reference/req_perform_parallel.html)
  and abort the whole station mid push – observed in the wild after ~25
  min on station 7044 at record ~9030/13362. With
  `retry_on_failure = TRUE` the same record gets retried up to four
  times with the existing exponential backoff (2, 4, 8, 16 s), and
  because ThingsBoard de-duplicates by `(ts, key)` the retry never
  produces a duplicate row even when the first attempt actually reached
  the server before the connection dropped.

- Add
  [`tb_setup_devices()`](https://kwb-r.github.io/wasserportal/reference/tb_setup_devices.md),
  [`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md)
  and
  [`tb_push_station_attributes()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_attributes.md)
  for shipping Wasserportal time series and master data into a
  ThingsBoard tenant via the device-token telemetry API.
  [`tb_setup_devices()`](https://kwb-r.github.io/wasserportal/reference/tb_setup_devices.md)
  bootstraps a fresh tenant from an account-level API key, so the rest
  of the workflow runs from R alone

- Add `vignettes/thingsboard-demo.Rmd` walking through the ThingsBoard
  Cloud free-tier (Maker) demo on `eu.thingsboard.cloud`, including the
  switch to self-hosted Community Edition

- Add `inst/scripts/push_to_thingsboard.R` consuming the daily JSON
  artefacts on the `gh-pages` branch (no Wasserportal scrape of its
  own). The script picks the five groundwater stations with the longest
  combined gwl + gwq history and the most distinct gwq parameters,
  uploads merged master data as device attributes and pushes both the
  level and quality time series as telemetry

- Convert `Rechtswert_UTM_33_N` / `Hochwert_UTM_33_N` (ETRS89 / UTM zone
  33N, EPSG:25833) to WGS84 `latitude` / `longitude` attributes so
  ThingsBoard map widgets work out of the box

- Add `.github/workflows/thingsboard-push.yaml` running the script on
  push to `main` / `master` / `dev`, daily at 07:00 UTC and via
  `workflow_dispatch`. Credentials are read from the `TB_HOST` and
  `TB_API_KEY` repository secrets

- Authenticate
  [`tb_setup_devices()`](https://kwb-r.github.io/wasserportal/reference/tb_setup_devices.md)
  with the `X-Authorization: ApiKey <key>` request header that
  ThingsBoard expects for account-level API keys (the standard
  `Authorization: Bearer ...` and the JWT-style
  `X-Authorization: Bearer ...` variants both return HTTP 401)

- Drop pre-1970 timestamps inside `build_telemetry_payload()`. Some
  Wasserportal groundwater stations start in the 1950s, which yields
  negative epoch milliseconds (the Unix/POSIX epoch is defined as
  1970-01-01 UTC, see [IEEE Std 1003.1, “4.16 Seconds Since the
  Epoch”](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html#tag_04_16)).
  ThingsBoard transports `ts` as a Java `Long` of epoch milliseconds
  (see the [HTTP Device
  API](https://thingsboard.io/docs/reference/http-api/#publish-telemetry-data)
  reference); negative values are spec-legal but the Maker free tier
  observed in this branch responds with an opaque HTTP 500 to such
  posts. Filtering `ts_ms > 0` keeps the rest of the (post-1970) history
  flowing through. For station 3 this drops about 17 years of monthly
  groundwater level readings while preserving the remaining ~7800 values

- Wire a `tb_error_body()` helper into `httr2::req_error(body = ...)` on
  the telemetry and attributes calls so future ThingsBoard failures
  surface the JSON `message` field in the R error instead of the generic
  “HTTP 500 Internal Server Error” wrapper

- Add
  [`tb_push_latest_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_latest_telemetry.md)
  for the simplest `{"key": value}` form (server-stamped time). Used in
  `inst/scripts/push_to_thingsboard.R` as a smoke test before the bulk
  push: the bulk array-of-records form returns an opaque HTTP 500 on the
  ThingsBoard Cloud Maker free tier even though the same device accepts
  attribute writes and the simpler per-record format

- Add a `mode` parameter to
  [`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md)
  (`"single"` by default, `"bulk"` for self-hosted CE). Single mode
  POSTs each record as a standalone `{"ts": ms, "values": {...}}` object
  so historical telemetry actually goes through on Maker free; bulk mode
  keeps the previous fast array-per-chunk behaviour for self-hosted CE

- Add a `throttle_seconds` parameter to
  [`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md)
  so the inter-request sleep can be tuned per ThingsBoard plan instead
  of being hardcoded. `NULL` (default) keeps the previous values (50 ms
  in single mode, 100 ms in bulk mode); pass a non-zero number to slow
  down or `0` to push as fast as the server permits (e.g. self-hosted
  CE)

- Add
  [`tb_plan_defaults()`](https://kwb-r.github.io/wasserportal/reference/tb_plan_defaults.md)
  and a matching `TB_PLAN` env var so the GH-Actions push picks `mode`,
  `chunk_size` and `throttle_seconds` from the per-device transport rate
  limits documented at
  <https://thingsboard.io/docs/paas/eu/subscriptions/>. Presets: `free`
  -\> `single` mode (proven to work end-to-end on the Maker free tier);
  `free-bulk` -\> bulk preset for Free with `chunk_size = 10` /
  `throttle_seconds = 1.0`; **confirmed not to work** on the public
  Cloud Maker tier as of 2026-05 – the gateway returns the same
  empty-body HTTP 500 to a 10-record array as it did to the original
  100-record one, so the array form is rejected regardless of payload
  size. Kept as a reproducible baseline. `prototype` / `pilot` /
  `startup` / `business` -\> `bulk` with `chunk_size = 30` /
  `throttle_seconds = 1.0` (~30 dp/s, near the 2 000 dp/min per-device
  cap shared across all paid tiers); `ce` -\> unlimited bulk for
  self-hosted Community Edition. Add `TB_TELEMETRY_MODE`,
  `TB_CHUNK_SIZE` and `TB_THROTTLE_SECONDS` env vars on top of `TB_PLAN`
  so individual values can be overridden without switching plans

- Expose the plan and the per-run knobs as `workflow_dispatch` inputs in
  `thingsboard-push.yaml` (`plan`, `station_ids`, `history_days`,
  `telemetry_types`) and document the workflow_dispatch input -\>
  repository secret -\> hardcoded default fallback chain in a header
  comment of the env block. The default plan is `free` (single mode,
  proven to work); `free-bulk` is exposed as a workflow_dispatch option
  but stays out of the cron path until ThingsBoard lifts the Maker
  array-form rejection

- Drop the
  [`tb_push_latest_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_latest_telemetry.md)
  “smoke test” that `inst/scripts/push_to_thingsboard.R` ran per device
  before the bulk telemetry push. The smoke test posted one value per
  station with `{"key": value}` (no timestamp – server stamped with the
  current wall-clock time), originally as a fail-fast probe for the
  Maker free-tier auth/payload path. The visible side effect was a stale
  “GW-Stand = @ ” row that drowned out the real most-recent measurement
  in the device’s *Latest telemetry* view. The bulk historical push
  fails on its own first POST anyway, so the safety net was redundant.
  [`tb_push_latest_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_latest_telemetry.md)
  itself stays as an exported helper for ad-hoc connectivity probes

- Add
  [`tb_get_device_id()`](https://kwb-r.github.io/wasserportal/reference/tb_get_device_id.md),
  [`tb_list_device_telemetry_keys()`](https://kwb-r.github.io/wasserportal/reference/tb_list_device_telemetry_keys.md)
  and
  [`tb_delete_device_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_delete_device_telemetry.md)
  for read-only device discovery and selective telemetry cleanup against
  the ThingsBoard plugin API (`GET /api/tenant/devices`,
  `GET /api/plugins/telemetry/DEVICE/{id}/keys/timeseries`,
  `DELETE /api/plugins/telemetry/DEVICE/{id}/timeseries/delete`). All
  three accept `TB_HOST` / `TB_API_KEY` from the environment so they can
  be called from a fresh R session without explicit credentials. Pass
  `keys = NULL` to
  [`tb_delete_device_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_delete_device_telemetry.md)
  to wipe every key the device currently stores; server-side attributes
  (latitude, longitude, Bezirk, …) are left in place so the map widget
  keeps working after a wipe. Stale rows from the now-removed smoke test
  can also be cleared interactively in the ThingsBoard UI (Device \>
  Latest telemetry \> tick the row \> trash icon)

- Add `inst/extdata/thingsboard-dashboard.json`, an importable
  ThingsBoard dashboard for the demo: an OpenStreetMap of the five
  Berlin groundwater stations, a master-data entities table and two
  time-series charts (groundwater level, selected quality parameters).
  All four widgets discover the `wasserportal-gw-*` devices via an
  `entityName`-prefix alias so the import works without hardcoding
  device IDs. The dashboard-level timewindow runs from `1970-01-01 UTC`
  (POSIX epoch) to `2027-01-01 UTC` with `aggregation = NONE` and
  `limit = 50000` per series, so the charts return raw unaveraged
  measurements over the full Wasserportal archive rather than daily
  averages (the earlier `AVG` aggregation over the 130-year `1970..2100`
  window had made ThingsBoard show an indefinite loading spinner
  whenever the time-window selector was touched; switching to `NONE`
  keeps the wide range usable because the server only needs to return up
  to 50000 sorted raw points per (entity, key) pair which is comfortably
  above the ~16000 GW-Stand and ~8000 GWQ records per station that the
  Wasserportal archive contains). The map widget uses the modern
  `typeFullFqn = "system.map"` reference together with the
  `latKeyName = "latitude"` / `lngKeyName = "longitude"` settings
  binding that the `system.map` widget accepts as a stable
  backward-compatible attribute mapping, so markers render right after
  import (an earlier `markers` array variant with `xKey` / `yKey` left
  the map empty against the same lat/lon attributes)

- Speed up `mode = "single"` with
  [`httr2::req_perform_parallel()`](https://httr2.r-lib.org/reference/req_perform_parallel.html).
  The previous sequential one-POST-at-a-time loop was network-bound at
  ~1.2 records/s for the GWQ push (~5 h per station for the full
  history); concurrent posting with `max_active = 10` lifts that to ~10
  records/s.
  [`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md)
  gains a `max_active` parameter;
  [`tb_plan_defaults()`](https://kwb-r.github.io/wasserportal/reference/tb_plan_defaults.md)
  returns it per plan (default `10` for Free, `1` elsewhere); the script
  reads `TB_MAX_ACTIVE` from env / repo secrets through the same
  `env_or()` plan-fallback chain. Pace concurrent batches
  one-`max_active`-group at a time and retry on transient HTTP
  500/502/503/504 with exponential backoff, so the Free tier’s 600
  messages/minute sustained per-device limit doesn’t trip the gateway
  after ~35 s at 48 records/s (the symptom we hit with the initial
  implementation)

- Send one telemetry record per `(timestamp, key, value)` triple in
  `mode = "single"` instead of grouping every Parameter that shares a
  timestamp into a single record. Wasserportal groundwater quality data
  has ~30 analytes per sampling event; the resulting “fat” `values`
  dicts produced an opaque empty-body HTTP 500 on Cloud Maker even
  though the same keys went through one at a time (see
  [`tb_push_latest_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_latest_telemetry.md)
  smoke tests). `build_telemetry_payload()` gains a `group_by_ts`
  parameter (default `TRUE`); the push function flips it off in single
  mode and keeps grouping in bulk mode for compact array chunks

- Sanitise telemetry keys before serialising the values dict.
  Wasserportal groundwater quality parameters such as
  `Leitfaehigkeit 25 grd C vor Ort`, `Wasserst. (ROK) vor`,
  `pH-Wert (Feld)` or `Temperatur (Wasser)` triggered an opaque HTTP 500
  on the Maker free tier when used as raw JSON keys (after the level
  data already pushed cleanly). The new `sanitize_tb_key()` helper folds
  umlauts, drops parentheses and replaces spaces / dots / commas with
  underscores so quality data goes through too. Add a
  `TB_TELEMETRY_TYPES` env var (`"gwl,gwq"` by default) so a partial
  retry can skip the slow level re-push and only re-do the quality push

## [wasserportal 0.5.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.5.0) 2026-05-07

- Modernize GitHub Actions workflows: use
  `r-lib/actions/setup-r-dependencies@v2` and
  `r-lib/actions/check-r-package@v2` on `ubuntu-latest` instead of the
  deprecated v2/`ubuntu-20.04`/`r-hub/sysreqs` toolchain
- Bump JavaScript actions to Node-24-compatible versions
  (`actions/checkout@v5`, `actions/upload-artifact@v5`) and set
  `FORCE_JAVASCRIPT_ACTIONS_TO_NODE24=true` so transitive
  `r-lib/actions/*@v2` steps opt into Node 24 as well, ahead of the June
  2nd 2026 deprecation of Node 20 on GitHub Actions runners
- Add Claude Code review workflows (`claude.yaml`,
  `claude-code-review.yaml`)
- [`get_wasserportal_master_data()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_master_data.md):
  match the new HTML5 markup of the master-data table
  (`<caption>Pegel Berlin</caption>` instead of the legacy
  `summary="Pegel Berlin"` attribute)
- Decode wasserportal pages explicitly as `windows-1252`. The pages
  declare UTF-8 in `<meta charset>` but the server actually emits
  Latin-1 bytes (e.g. `0xE4` for `ä`); trusting the meta declaration
  left those bytes mis-marked as UTF-8 and broke
  `subst_special_chars()`’s `ä→ae` / `ü→ue` substitutions on Windows R
- Bypass
  [`rvest::html_table()`](https://rdrr.io/pkg/rvest/man/html_table.html)
  and `xml2::xml_text(trim = TRUE)` in
  [`get_wasserportal_master_data()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_master_data.md)
  and
  [`get_wasserportal_stations_table()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_stations_table.md):
  both delegate to a `sub("^[[:space:] ]+", ...)` pass that fails on
  Windows R when the cell text contains umlauts. Tables are now
  extracted directly via `xml2` and trimmed with a locale-safe
  `gsub(..., useBytes = TRUE)` helper (`trim_bytes()`)
- Make
  [`get_stations()`](https://kwb-r.github.io/wasserportal/reference/get_stations.md)
  and
  [`get_wasserportal_masters_data()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_masters_data.md)
  resilient when parallel workers cannot fetch a station overview: load
  the `wasserportal` namespace into the cluster and drop `try-error`
  results before
  [`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html)
  /
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
- Make live-HTTP tests skip gracefully when `wasserportal.berlin.de` is
  unreachable from the test host (CRAN, sandboxed CI)
- Update
  [`get_wasserportal_masters_data()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_masters_data.md)
  test expectations to include the new `Anmerkung` column that
  wasserportal added to surface-water master data

## [wasserportal 0.4.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.4.0) 2024-04-05

- New feature: add support for downloading all available surface water
  quality data for one or multiple monitoring stations. For details see
  [`get_surfacewater_qualities()`](https://kwb-r.github.io/wasserportal/reference/get_surfacewater_qualities.md)
- Bugfix for groundwater level and quality due to new Wasserportal API
- Add project
  [AD4GD](https://www.kompetenz-wasser.de/de/forschung/projekte/ad4gd)
  as funder

## [wasserportal 0.3.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.3.0) 2023-02-19

- Fix errors in GitHub actions: use actions from branches `v2`, `v3`,
  not from `master`
- Fix errors in tutorial.Rmd
- Fix errors in documentation
- Do not run examples that use parallel processing
- [`get_stations()`](https://kwb-r.github.io/wasserportal/reference/get_stations.md):
  add argument `n_cores`
- [`get_wasserportal_stations_table()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_stations_table.md):
  Use new (three letter) variable codes
- [`read_wasserportal_raw()`](https://kwb-r.github.io/wasserportal/reference/read_wasserportal_raw.md):
  adapt request to new API version, add argument `api_version`
- [`read_wasserportal_raw_gw()`](https://kwb-r.github.io/wasserportal/reference/read_wasserportal_raw_gw.md):
  adapt request to new API version
- Clean code, mainly to reduce duplicated duplication and to improve
  readability
  - check for more errors
  - use “safe” element selection
  - use more helper functions
  - use helper functions in vignettes
  - improve names

## [wasserportal 0.2.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.2.0) 2022-09-08

- Add functions for exporting time series data to `zip` files
  ([`wp_masters_data_to_list()`](https://kwb-r.github.io/wasserportal/reference/wp_masters_data_to_list.md))
  and master data to `csv` files
  ([`wp_timeseries_data_to_list()`](https://kwb-r.github.io/wasserportal/reference/wp_timeseries_data_to_list.md)),
  which will be uploaded to
  <https://kwb-r.github.io/wasserportal>/`<filename>`

- In addition `import` functions for downloading and importing the
  datasets above into R as lists were added
  ([`list_timeseries_data_to_zip()`](https://kwb-r.github.io/wasserportal/reference/list_timeseries_data_to_zip.md),
  [`list_masters_data_to_csv()`](https://kwb-r.github.io/wasserportal/reference/list_masters_data_to_csv.md))

- Code cleaning by `@hsonne` started

- Fix `master data` requests by using the `master_url` instead of
  `station_id`, as the latter was not unique. Now functions
  [`get_wasserportal_master_data()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_master_data.md)
  and it wrapper function
  [`get_wasserportal_masters_data()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_masters_data.md)
  require the `master_url` instead of `station_id` as input parameter.
  The function `get_stations` now adds the column `stammdaten_link` as
  additional column for each sublist element of the sublist
  `overview_list`.

- Fix to scrape `groundwater level` data from all available monitoring
  stations (instead of only 5!) and export to `.csv` file. In addition
  switch also to `.csv` export for `groundwater quality` instead of
  `.json` due to reduced storage space
  ([stations_gwq_data.json](https://github.com/KWB-R/wasserportal/blob/gh-pages/stations_gwq_data.json)
  file is already 98.4 MB large.

- Add functions
  ([`get_daily_surfacewater_data()`](https://kwb-r.github.io/wasserportal/reference/get_daily_surfacewater_data.md))
  and adapt article [Surface
  Water](https://kwb-r.github.io/wasserportal/articles/surface-water.md)
  for scraping all available daily surface water data and exporting to
  one `.csv` file for each parameter (containing all monitoring
  stations)

- Deactivate *PROMISCES* workflows (see [wasserportal
  v0.1.0](#wasserportal-010-2022-06-01)), due to failing Zenodo
  download. Will be moved into own R package, most properly
  [kwb.promisces](https://github.com/kwb-r/kwb.promisces).

## [wasserportal 0.1.1](https://github.com/KWB-R/wasserportal/releases/tag/v0.1.1) 2022-06-09

- Fix bug in
  [`get_wasserportal_stations_table()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_stations_table.md)
  now correctly naming parameter `temperature` (formerly incorrectly
  `level`)
- Fix [Surface
  Water](https://kwb-r.github.io/wasserportal/articles/surface-water.md)
  article
- Adapt Zenodo DOI badge to cite always latest release

## [wasserportal 0.1.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.1.0) 2022-06-01

R package for scraping `groundwater` data (`groundwater level` and
`quality`) from [Wasserportal Berlin](https://wasserportal.berlin.de).
Please note that the support for scraping `surface water` monitoring
stations is currently very limited!

**Functions:**

- [`get_stations()`](https://kwb-r.github.io/wasserportal/reference/get_stations.md):
  returns metadata for all available monitoring stations
- [`get_wasserportal_masters_data()`](https://kwb-r.github.io/wasserportal/reference/get_wasserportal_masters_data.md):
  get master data for selected `station_ids`
- [`read_wasserportal_raw_gw()`](https://kwb-r.github.io/wasserportal/reference/read_wasserportal_raw_gw.md):
  enables the download of `groundwater data`. Checkout the
  [Tutorial](https://kwb-r.github.io/wasserportal/articles/tutorial.md)
  article how to use it for downloading
  [one](https://kwb-r.github.io/wasserportal/articles/tutorial.html#download-and-plotting-one-station)
  or
  [multiple](https://kwb-r.github.io/wasserportal/articles/tutorial.html#download-and-plotting-multiple-stations)
  stations at once.
- [`read_wasserportal()`](https://kwb-r.github.io/wasserportal/reference/read_wasserportal.md):
  works for `surface water` monitoring stations, but is outdated, as it
  is based on an outdated static file with station/variable names
  (i.e. only for `11` instead of `82` stations currently provided!)
  instead of relying on new metadata provided online. This will be fixed
  within the next release. For progress on this issue checkout
  [\#21](https://github.com/KWB-R/wasserportal/issues/21)

**Workflows:**

- [Tutorial](https://kwb-r.github.io/wasserportal/articles/tutorial.md)
  article how to download groundwater level and quality data

- **Further Usage** by combining previously scraped (see
  [tutorial](https://kwb-r.github.io/wasserportal/articles/tutorial.md)
  above) data and performing some analysis:

  - [Groundwater](https://kwb-r.github.io/wasserportal/articles/groundwater.md),
    e.g. creating a map with GW level trends

  - Two workflows ([REACH
    UBA](https://kwb-r.github.io/wasserportal/articles/promisces_reach-uba.md),
    [Norman
    List](https://kwb-r.github.io/wasserportal/articles/promisces_norman-lists.md))
    created within the project
    [PROMISCES](https://www.kompetenz-wasser.de/en/forschung/projekte/promisces)
    for assessing prevalence and the spatial distribution of
    **p**ersistent, **m**obile and **t**oxic (PMT) substances in the
    Berlin groundwater, based on different PMT lists, i.e. [REACH
    UBA](https://kwb-r.github.io/wasserportal/articles/promisces_reach-uba.md)
    or [Norman
    List](https://kwb-r.github.io/wasserportal/articles/promisces_norman-lists.md).

## wasserportal 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.

- see <https://style.tidyverse.org/news.html> for writing a good
  `NEWS.md`
