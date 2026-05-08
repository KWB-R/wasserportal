# Changelog

## wasserportal 0.5.0.9000 (development version)

- Add `tb_setup_devices()`, `tb_push_station_telemetry()` and
  `tb_push_station_attributes()` for shipping Wasserportal time series
  and master data into a ThingsBoard tenant via the device-token
  telemetry API. `tb_setup_devices()` bootstraps a fresh tenant from an
  account-level API key, so the rest of the workflow runs from R alone
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
- [`get_wasserportal_master_data()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_master_data.md):
  match the new HTML5 markup of the master-data table
  (`<caption>Pegel Berlin</caption>` instead of the legacy
  `summary="Pegel Berlin"` attribute)
- Decode wasserportal pages explicitly as `windows-1252`. The pages
  declare UTF-8 in `<meta charset>` but the server actually emits
  Latin-1 bytes (e.g. `0xE4` for `ä`); trusting the meta declaration
  left those bytes mis-marked as UTF-8 and broke
  `subst_special_chars()`’s `ä→ae` / `ü→ue` substitutions on Windows R
- Bypass
  [`rvest::html_table()`](https://rvest.tidyverse.org/reference/html_table.html)
  and `xml2::xml_text(trim = TRUE)` in
  [`get_wasserportal_master_data()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_master_data.md)
  and
  [`get_wasserportal_stations_table()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_stations_table.md):
  both delegate to a `sub("^[[:space:] ]+", ...)` pass that fails on
  Windows R when the cell text contains umlauts. Tables are now
  extracted directly via `xml2` and trimmed with a locale-safe
  `gsub(..., useBytes = TRUE)` helper (`trim_bytes()`)
- Make
  [`get_stations()`](https://kwb-r.github.io/wasserportal/dev/reference/get_stations.md)
  and
  [`get_wasserportal_masters_data()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_masters_data.md)
  resilient when parallel workers cannot fetch a station overview: load
  the `wasserportal` namespace into the cluster and drop `try-error`
  results before
  [`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html)
  /
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
- Make live-HTTP tests skip gracefully when `wasserportal.berlin.de` is
  unreachable from the test host (CRAN, sandboxed CI)
- Update
  [`get_wasserportal_masters_data()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_masters_data.md)
  test expectations to include the new `Anmerkung` column that
  wasserportal added to surface-water master data

## [wasserportal 0.4.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.4.0) 2024-04-05

- New feature: add support for downloading all available surface water
  quality data for one or multiple monitoring stations. For details see
  [`get_surfacewater_qualities()`](https://kwb-r.github.io/wasserportal/dev/reference/get_surfacewater_qualities.md)
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
- [`get_stations()`](https://kwb-r.github.io/wasserportal/dev/reference/get_stations.md):
  add argument `n_cores`
- [`get_wasserportal_stations_table()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_stations_table.md):
  Use new (three letter) variable codes
- [`read_wasserportal_raw()`](https://kwb-r.github.io/wasserportal/dev/reference/read_wasserportal_raw.md):
  adapt request to new API version, add argument `api_version`
- [`read_wasserportal_raw_gw()`](https://kwb-r.github.io/wasserportal/dev/reference/read_wasserportal_raw_gw.md):
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
  ([`wp_masters_data_to_list()`](https://kwb-r.github.io/wasserportal/dev/reference/wp_masters_data_to_list.md))
  and master data to `csv` files
  ([`wp_timeseries_data_to_list()`](https://kwb-r.github.io/wasserportal/dev/reference/wp_timeseries_data_to_list.md)),
  which will be uploaded to
  <https://kwb-r.github.io/wasserportal>/`<filename>`

- In addition `import` functions for downloading and importing the
  datasets above into R as lists were added
  ([`list_timeseries_data_to_zip()`](https://kwb-r.github.io/wasserportal/dev/reference/list_timeseries_data_to_zip.md),
  [`list_masters_data_to_csv()`](https://kwb-r.github.io/wasserportal/dev/reference/list_masters_data_to_csv.md))

- Code cleaning by `@hsonne` started

- Fix `master data` requests by using the `master_url` instead of
  `station_id`, as the latter was not unique. Now functions
  [`get_wasserportal_master_data()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_master_data.md)
  and it wrapper function
  [`get_wasserportal_masters_data()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_masters_data.md)
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
  ([`get_daily_surfacewater_data()`](https://kwb-r.github.io/wasserportal/dev/reference/get_daily_surfacewater_data.md))
  and adapt article [Surface
  Water](https://kwb-r.github.io/wasserportal/dev/articles/surface-water.md)
  for scraping all available daily surface water data and exporting to
  one `.csv` file for each parameter (containing all monitoring
  stations)

- Deactivate *PROMISCES* workflows (see [wasserportal
  v0.1.0](#wasserportal-010-2022-06-01)), due to failing Zenodo
  download. Will be moved into own R package, most properly
  [kwb.promisces](https://github.com/kwb-r/kwb.promisces).

## [wasserportal 0.1.1](https://github.com/KWB-R/wasserportal/releases/tag/v0.1.1) 2022-06-09

- Fix bug in
  [`get_wasserportal_stations_table()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_stations_table.md)
  now correctly naming parameter `temperature` (formerly incorrectly
  `level`)
- Fix [Surface
  Water](https://kwb-r.github.io/wasserportal/dev/articles/surface-water.md)
  article
- Adapt Zenodo DOI badge to cite always latest release

## [wasserportal 0.1.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.1.0) 2022-06-01

R package for scraping `groundwater` data (`groundwater level` and
`quality`) from [Wasserportal Berlin](https://wasserportal.berlin.de).
Please note that the support for scraping `surface water` monitoring
stations is currently very limited!

**Functions:**

- [`get_stations()`](https://kwb-r.github.io/wasserportal/dev/reference/get_stations.md):
  returns metadata for all available monitoring stations
- [`get_wasserportal_masters_data()`](https://kwb-r.github.io/wasserportal/dev/reference/get_wasserportal_masters_data.md):
  get master data for selected `station_ids`
- [`read_wasserportal_raw_gw()`](https://kwb-r.github.io/wasserportal/dev/reference/read_wasserportal_raw_gw.md):
  enables the download of `groundwater data`. Checkout the
  [Tutorial](https://kwb-r.github.io/wasserportal/dev/articles/tutorial.md)
  article how to use it for downloading
  [one](https://kwb-r.github.io/wasserportal/dev/articles/tutorial.html#download-and-plotting-one-station)
  or
  [multiple](https://kwb-r.github.io/wasserportal/dev/articles/tutorial.html#download-and-plotting-multiple-stations)
  stations at once.
- [`read_wasserportal()`](https://kwb-r.github.io/wasserportal/dev/reference/read_wasserportal.md):
  works for `surface water` monitoring stations, but is outdated, as it
  is based on an outdated static file with station/variable names
  (i.e. only for `11` instead of `82` stations currently provided!)
  instead of relying on new metadata provided online. This will be fixed
  within the next release. For progress on this issue checkout
  [\#21](https://github.com/KWB-R/wasserportal/issues/21)

**Workflows:**

- [Tutorial](https://kwb-r.github.io/wasserportal/dev/articles/tutorial.md)
  article how to download groundwater level and quality data

- **Further Usage** by combining previously scraped (see
  [tutorial](https://kwb-r.github.io/wasserportal/dev/articles/tutorial.md)
  above) data and performing some analysis:

  - [Groundwater](https://kwb-r.github.io/wasserportal/dev/articles/groundwater.md),
    e.g. creating a map with GW level trends

  - Two workflows ([REACH
    UBA](https://kwb-r.github.io/wasserportal/dev/articles/promisces_reach-uba.md),
    [Norman
    List](https://kwb-r.github.io/wasserportal/dev/articles/promisces_norman-lists.md))
    created within the project
    [PROMISCES](https://www.kompetenz-wasser.de/en/forschung/projekte/promisces)
    for assessing prevalence and the spatial distribution of
    **p**ersistent, **m**obile and **t**oxic (PMT) substances in the
    Berlin groundwater, based on different PMT lists, i.e. [REACH
    UBA](https://kwb-r.github.io/wasserportal/dev/articles/promisces_reach-uba.md)
    or [Norman
    List](https://kwb-r.github.io/wasserportal/dev/articles/promisces_norman-lists.md).

## wasserportal 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.

- see <https://style.tidyverse.org/news.html> for writing a good
  `NEWS.md`
