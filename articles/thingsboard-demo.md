# ThingsBoard Demo (Free Tier)

This vignette walks through pushing Berlin **groundwater** stations from
[Wasserportal](https://wasserportal.berlin.de) into a
[ThingsBoard](https://thingsboard.io) tenant – typically the **free
Maker tier** on `https://eu.thingsboard.cloud` (5 devices, 1 M data
points / month). The same code works against self-hosted ThingsBoard
Community Edition (CE) by pointing `TB_HOST` at it and authenticating
with a username/password login instead of an account API key (CE has no
account API keys – see step 2 below).

Groundwater is the primary focus because that is what the daily
GitHub-Actions push (`.github/workflows/thingsboard-push.yaml`) and the
importable dashboard at `inst/extdata/thingsboard-dashboard.json` ship
out of the box: five Berlin groundwater stations with both a **level**
(`GW-Stand`) time series and a **quality** time series spanning dozens
of analytes (`Chlorid`, `Nitrat`, `Sulfat`, …). A short note at the end
shows how the same primitives apply to surface water gauges.

## 1. Prerequisites

1.  Sign up at <https://eu.thingsboard.cloud> (EU) or
    <https://thingsboard.cloud> (US).

2.  **Authenticate** – pick the option that matches your instance:

    - *ThingsBoard Cloud*: generate an **account API key** under
      *Account \> Security \> API keys \> Generate*. Give it a
      description like `wasserportal-demo` and copy the key once – it is
      only shown on creation. Store it in `TB_API_KEY`.
    - *Self-hosted Community Edition* (e.g.
      `https://dashboards.inowas.org`): CE has no account API keys, so
      use your ThingsBoard **username + password** (`TB_USERNAME` /
      `TB_PASSWORD`); the package logs in via
      [`tb_login()`](https://kwb-r.github.io/wasserportal/reference/tb_login.md)
      to obtain a JWT. The account needs tenant-administrator rights to
      create devices.

3.  Install the package and `httr2`:

    ``` r

    # install.packages("httr2")
    remotes::install_github("kwb-r/wasserportal")
    ```

4.  Store credentials as environment variables (e.g. in `~/.Renviron`):

    ``` r
    # ThingsBoard Cloud (account API key):
    TB_HOST=https://eu.thingsboard.cloud
    TB_API_KEY=<your-api-key>

    # ...or self-hosted Community Edition (username/password login):
    # TB_HOST=https://dashboards.inowas.org
    # TB_USERNAME=<your-thingsboard-user>
    # TB_PASSWORD=<your-thingsboard-password>
    # TB_PLAN=ce
    ```

    Restart the R session so the variables are loaded.

> **Free-tier limit**: 5 devices. The demo therefore pushes a small
> selection of stations. To push the full Wasserportal archive you need
> to self-host ThingsBoard CE or upgrade to a paid Cloud tier.

## 2. Inspect What Will Be Pushed

The `pkgdown` workflow publishes the day’s groundwater level and quality
ZIPs to the `gh-pages` branch every morning at 05:00 UTC.
[`inspect_gh_pages_zips()`](https://kwb-r.github.io/wasserportal/reference/inspect_gh_pages_zips.md)
downloads and summarises them locally so you can see record counts per
station before any data hits ThingsBoard:

``` r

library(wasserportal)

dat <- wasserportal::inspect_gh_pages_zips()
gwl <- dat$groundwater_level     # daily groundwater level
gwq <- dat$groundwater_quality   # irregular groundwater quality

# Five top stations by combined gwl+gwq history & analyte breadth:
demo_station_ids <- c("6038", "7044", "7045", "7206", "7207")

# Expected record counts (full archive):
table(gwl$Messstellennummer[gwl$Messstellennummer %in% demo_station_ids])
table(gwq$Messstellennummer[gwq$Messstellennummer %in% demo_station_ids])

# Drill into one station, see how many distinct GWQ parameters it has:
length(unique(gwq$Parameter[gwq$Messstellennummer == "6038"]))
```

The `inst/scripts/push_to_thingsboard.R` script picks these top-five
automatically via the score
`(n_gwl_rows + n_gwq_rows) * n_distinct_gwq_parameters`, so you can also
just let the production script choose for you.

## 3. Create Devices in ThingsBoard

[`tb_setup_devices()`](https://kwb-r.github.io/wasserportal/reference/tb_setup_devices.md)
uses the API key to create (or look up) one device per
`Messstellennummer` and reads back the device-specific access token used
for telemetry pushes. The device profile defaults to `wasserportal` and
is auto-created on first use.

``` r

device_tokens <- wasserportal::tb_setup_devices(
  station_ids = demo_station_ids,
  name_prefix = "wasserportal-gw-"
)

# Names are Messstellennummer, values are tokens. Treat as secret.
str(device_tokens)
```

Re-running is safe: existing devices are reused, their tokens are
re-read but not regenerated.

## 4. Push Station Master Data as Attributes

Static metadata (district, aquifer, terrain elevation, screen depth,
operator, …) is sent once per station as ThingsBoard *attributes*. They
show up under the device’s *Attributes \> Server* tab and are referenced
from the dashboard widgets (the master-data table and the map’s marker
tooltips).

The Wasserportal master files report coordinates in ETRS89 / UTM zone
33N (EPSG:25833); ThingsBoard map widgets read WGS84 from attributes
named exactly `latitude` and `longitude`. The push script does the
conversion via
[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)
before posting the attributes, so the OpenStreetMap markers drop in the
right place without any UI fiddling.

``` r

gwl_master <- jsonlite::fromJSON(
  "https://kwb-r.github.io/wasserportal/stations_gwl_master.json"
)
gwq_master <- jsonlite::fromJSON(
  "https://kwb-r.github.io/wasserportal/stations_gwq_master.json"
)

# (simplified: combine gwl + gwq master per station, then push)
for (station_id in demo_station_ids) {
  attrs <- as.list(
    gwl_master[gwl_master$Nummer == station_id, , drop = FALSE]
  )
  # ... merge gwq fields, drop NA / list columns, convert UTM33N -> WGS84
  # (see inst/scripts/push_to_thingsboard.R for the full helper)
  wasserportal::tb_push_station_attributes(
    attributes   = attrs,
    device_token = device_tokens[[station_id]]
  )
}
```

In practice run `Rscript inst/scripts/push_to_thingsboard.R` instead of
hand-rolling the master-data flatten step – the script ships the
production-grade version including UTM-to-WGS84 conversion, NA filtering
and shared key fallbacks between gwl and gwq master tables.

## 5. Push Time Series (Level + Quality)

[`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md)
pivots the long-format Wasserportal data on the fly so every distinct
`Parameter` value becomes a ThingsBoard telemetry key sharing the same
timestamp.

``` r

# Pushed as telemetry keys "GW-Stand", "Wassertemperatur", ...
for (station_id in demo_station_ids) {
  one <- gwl[gwl$Messstellennummer == station_id, , drop = FALSE]
  if (nrow(one) == 0L) next

  wasserportal::tb_push_station_telemetry(
    data         = one,
    device_token = device_tokens[[station_id]],
    ts_col       = "Datum",
    value_col    = "Messwert",
    key_col      = "Parameter",
    mode         = "single"        # see section 6
  )
}
```

For groundwater quality the call is identical – the wide value-key
matrix is built per-record so `Chlorid`, `Nitrat`, … all land as
separate telemetry keys:

``` r

for (station_id in demo_station_ids) {
  one <- gwq[gwq$Messstellennummer == station_id, , drop = FALSE]
  if (nrow(one) == 0L) next

  wasserportal::tb_push_station_telemetry(
    data         = one,
    device_token = device_tokens[[station_id]],
    ts_col       = "Datum",
    value_col    = "Messwert",
    key_col      = "Parameter",
    mode         = "single"
  )
}
```

Two transforms run silently inside the helper that are worth knowing
about for debugging:

- **Key sanitisation** (`sanitize_tb_key()`): Wasserportal GWQ parameter
  names like `Leitfaehigkeit 25 grd C vor Ort` or `pH-Wert (Feld)`
  trigger an opaque HTTP 500 on the Maker free tier when used as raw
  JSON keys. The helper folds umlauts, drops parentheses and replaces
  spaces / dots / commas with underscores (so e.g. `pH-Wert_Feld`,
  `Leitfaehigkeit_25_grd_C_vor_Ort`). The ThingsBoard dashboard widgets
  reference these sanitised names, not the raw German labels.
- **Pre-1970 filtering** (`build_telemetry_payload()`): some
  Wasserportal groundwater stations start in the 1950s, which yields
  negative epoch milliseconds. ThingsBoard transports `ts` as a Java
  `Long` of epoch milliseconds; negative values are spec-legal but the
  Maker free tier returns an opaque HTTP 500 to such posts. Records with
  `ts_ms <= 0` are silently dropped; for a typical Berlin station this
  trims ~17 years of pre-1970 monthly readings while preserving the
  post-1970 history.

## 6. Plan-Aware Tuning

ThingsBoard PaaS subscription tiers have very different per-device
transport rate limits, and the Maker free tier additionally rejects the
bulk array-of-records form on the device telemetry endpoint regardless
of payload size.
[`tb_plan_defaults()`](https://kwb-r.github.io/wasserportal/reference/tb_plan_defaults.md)
wraps the documented limits into the parameters this package’s push
functions take:

``` r

wasserportal::tb_plan_defaults("free")
#> $mode             "single"   # array form rejected on Maker
#> $chunk_size       1L
#> $throttle_seconds 1.0
#> $max_active       10L

wasserportal::tb_plan_defaults("ce")
#> $mode             "bulk"     # no rate limit on self-hosted
#> $chunk_size       1000L
#> $throttle_seconds 0
#> $max_active       1L
```

Pass the result straight through to the push function:

``` r

preset <- wasserportal::tb_plan_defaults("free")

wasserportal::tb_push_station_telemetry(
  data             = one,
  device_token     = device_tokens[[station_id]],
  mode             = preset$mode,
  chunk_size       = preset$chunk_size,
  throttle_seconds = preset$throttle_seconds,
  max_active       = preset$max_active
)
```

In `mode = "single"` the helper sends each `(ts, key, value)` record as
a standalone `{"ts": ms, "values": {<key>: <value>}}` POST. The loop
runs `max_active` requests concurrently via
[`httr2::req_perform_parallel()`](https://httr2.r-lib.org/reference/req_perform_parallel.html),
paced one max_active-group at a time by `throttle_seconds` so even
Free’s 600 messages-per-minute per-device sustained limit is respected.
Transient HTTP 408/429/500/502/503/504 are retried with exponential
backoff (2 s, 4 s, 8 s, 16 s).

## 7. Visualise – Import the Demo Dashboard

The package ships an auto-importable dashboard at
`inst/extdata/thingsboard-dashboard.json` so you do not have to wire up
widgets by hand. It contains four widgets, all driven by an
entity-name-prefix alias (`wasserportal-gw-*`) so no device IDs are
hardcoded:

- **Berlin groundwater stations** – OpenStreetMap with one marker per
  device. Binds via the legacy-style `latKeyName: "latitude"` /
  `lngKeyName: "longitude"` settings that the `system.map` widget
  accepts as a stable backward-compatible attribute mapping, with marker
  labels and tooltips driven by the `entityName`, `Bezirk` and
  `Auspraegung` attributes.
- **Master data per station** – table widget showing Bezirk, Ausprägung,
  aquifer, terrain elevation (GOK) and screen depth per station.
- **Groundwater level history** – time-series chart of the `GW-Stand`
  key across all five stations.
- **Quality parameters** – time-series chart for `Nitrat`, `Chlorid`,
  `Sulfat` (extend interactively with more keys via the legend /
  data-source editor).

The dashboard-level timewindow is `1970-01-01 .. 2027-01-01 UTC` with
`aggregation = NONE` (and `limit = 50000` points per series), so the
charts display raw unaveraged measurements over the full Wasserportal
archive. The server only needs to return up to 50000 sorted raw points
per (entity, key) pair, which is comfortably above the ~16000 GW-Stand
and ~8000 GWQ records per station in the archive.

To import:

1.  In ThingsBoard go to **Dashboards** → top-right **+** → **Import
    dashboard**.
2.  Drop in `inst/extdata/thingsboard-dashboard.json` or paste its
    contents.
3.  Open the dashboard. The map auto-fits to the five stations; the
    table fills from server attributes; the charts use the
    dashboard-level timewindow defined above.

If you want to add a chart for another GWQ parameter (Wasserportal
exposes ~190–220 distinct analytes per station), the quickest path is
the device-side discovery view: in **Devices \> wasserportal-gw-**\*
**\> Latest telemetry**, hover over the row of interest and click the
small chart icon to plot that key’s history without touching the
dashboard.

## 8. Operational Helpers

A handful of read / cleanup helpers complement the push primitives:

``` r

# Look up a device's internal UUID by name (returns NA if missing):
dev_id <- wasserportal::tb_get_device_id("wasserportal-gw-6038")

# Discover every telemetry key the device currently stores:
keys <- wasserportal::tb_list_device_telemetry_keys(dev_id)
length(keys)              # typically ~190-220 for the demo stations
head(keys, 10)

# Wipe stale rows. By default deletes every key the device knows;
# server-side attributes (latitude, longitude, Bezirk, ...) are NOT
# touched, so the map widget keeps working after a wipe.
wasserportal::tb_delete_device_telemetry(dev_id)

# Or wipe just one key (e.g. after a Parameter rename):
wasserportal::tb_delete_device_telemetry(dev_id, keys = "GW-Stand")

# Smoke-test the device-token auth path without a timestamp -- the
# server stamps it with the current wall-clock time:
wasserportal::tb_push_latest_telemetry(
  values       = list(`GW-Stand` = 35.6),
  device_token = device_tokens[["6038"]]
)
```

[`tb_push_latest_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_latest_telemetry.md)
is the simplest possible telemetry POST (`{"key": value}`); historically
it was wired into the demo push script as a fail-fast probe but the
visible side effect was a stale “GW-Stand = @ ” row that drowned out the
real most-recent measurement. The probe has been removed from
`inst/scripts/push_to_thingsboard.R`; the helper itself stays as a
one-off connectivity check.

## 9. Production Push via GitHub Actions

The `.github/workflows/thingsboard-push.yaml` workflow runs the
`inst/scripts/push_to_thingsboard.R` script on push to `main` / `master`
/ `dev`, daily at 07:00 UTC via cron, and on `workflow_dispatch`.
Required repository secrets:

- `TB_HOST` – e.g. `https://eu.thingsboard.cloud`, or
  `https://dashboards.inowas.org` for a self-hosted instance
- **Authentication** (set one): `TB_API_KEY` (account-level API key,
  ThingsBoard Cloud) *or* `TB_USERNAME` + `TB_PASSWORD` (login, required
  for self-hosted Community Edition; these win when both are set)

Optional `workflow_dispatch` inputs override the cron defaults for a
single run without editing the YAML:

| Input | Purpose | Example |
|----|----|----|
| `station_ids` | Comma-separated Messstellennummer values. Empty = auto-pick top 5 by (gwl+gwq) × q-params. | `6038,7044` |
| `history_days` | Push only the most recent N days per station (`0` = full history). | `1460` |
| `telemetry_types` | Subset of `gwl,gwq` to push. | `gwl` |
| `plan` | ThingsBoard plan: `free` / `free-bulk` / `prototype` / `pilot` / `startup` / `business` / `ce`. | `free` |
| `max_devices` | Max devices/stations to set up. `0` = no limit (every candidate); empty = default 5. | `0` |
| `station_scope` | Auto-pick pool: `both` (default) / `any` / `gwl` / `gwq` / `gwl-only` / `gwq-only`. | `any` |

These mirror the underlying env vars (`TB_STATION_IDS`,
`TB_MAX_DEVICES`, `TB_STATION_SCOPE`, `TB_HISTORY_DAYS`,
`TB_TELEMETRY_TYPES`, `TB_PLAN`) which the script also reads from the
environment if run standalone.

To push **every** groundwater station, set `max_devices = 0` and
`station_scope = any` (level-only and quality-only stations included).
`gwl` / `gwq` select stations that *have* that series (a both-station
matches too), while `gwl-only` / `gwq-only` are the exclusive sets –
stations that have *only* level resp. *only* quality data. Mind the
volume – several hundred devices over the full archive is millions of
data points; start with a bounded `history_days` to validate the run.

## 10. Re-Running and Idempotency

- [`tb_setup_devices()`](https://kwb-r.github.io/wasserportal/reference/tb_setup_devices.md)
  is idempotent: re-running it returns the existing device tokens
  unchanged.
- [`tb_push_station_attributes()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_attributes.md)
  overwrites the previous attribute values on every push (ThingsBoard
  attributes are last-write-wins).
- [`tb_push_station_telemetry()`](https://kwb-r.github.io/wasserportal/reference/tb_push_station_telemetry.md)
  does **not** track which records were already pushed. ThingsBoard
  de-duplicates on the server side by `(timestamp, key)` – identical
  pairs are overwritten in place, not duplicated – but the helper still
  POSTs every record again, so a full `TB_HISTORY_DAYS=0` re-run takes
  the same ~1.5 h on the Maker free tier as the first push.
- For incremental daily pushes set `TB_HISTORY_DAYS=30` (or so) in the
  workflow: only the recent rows are re-broadcast, older history is
  skipped client-side and stays untouched on ThingsBoard.

## 11. Surface Water as an Alternative

The same primitives push surface-water gauges – the call signature only
differs in the value column name (`Tagesmittelwert` instead of
`Messwert`) and a different `name_prefix` to keep the devices in
separate ThingsBoard namespaces. A minimal example:

``` r

sw_data_daily <- wasserportal::get_daily_surfacewater_data(
  stations = wasserportal::get_stations()
)
sw_water_level <- sw_data_daily$surface_water.water_level

sw_demo_ids <- c("5803900", "5805600", "5867000", "5826200", "5824300")
sw_tokens <- wasserportal::tb_setup_devices(
  station_ids = sw_demo_ids,
  name_prefix = "wasserportal-sw-"
)

for (station_id in sw_demo_ids) {
  one <- sw_water_level[sw_water_level$Messstellennummer == station_id, ]
  if (nrow(one) == 0L) next
  wasserportal::tb_push_station_telemetry(
    data         = one,
    device_token = sw_tokens[[station_id]],
    ts_col       = "Datum",
    value_col    = "Tagesmittelwert",
    key_col      = "Parameter"
  )
}
```

The imported demo dashboard is groundwater-specific (its entity alias
filters on the `wasserportal-gw-` name prefix); to visualise the
`wasserportal-sw-` devices either change the alias’s filter prefix or
clone the dashboard and bind the widgets to a second entity alias.

## 12. Switching to Self-Hosted CE

When the 5-device free-tier limit becomes the bottleneck, point
`TB_HOST` at your own deployment and switch the plan preset. Self-hosted
Community Edition has **no account API keys**, so authenticate with your
ThingsBoard username and password (the package logs in via
[`tb_login()`](https://kwb-r.github.io/wasserportal/reference/tb_login.md)
to obtain a JWT); the account needs tenant-administrator rights:

``` r

Sys.setenv(
  TB_HOST     = "https://dashboards.inowas.org",
  TB_USERNAME = "me@example.org",
  TB_PASSWORD = "secret"
)
ce_preset <- wasserportal::tb_plan_defaults("ce")
# bulk mode, chunk_size = 1000, throttle_seconds = 0

# quick credential check -- returns a JWT string on success:
# wasserportal::tb_login()
```

The same R code now talks to your ThingsBoard CE deployment; devices,
tokens and pushes all use the same endpoints. On CE the bulk
array-of-records format goes through (Maker rejects it), so the `ce`
preset is roughly 30 × faster than `free` on the same hardware.
