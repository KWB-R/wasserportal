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


