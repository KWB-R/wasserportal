# [wasserportal 0.2.0](https://github.com/KWB-R/wasserportal/releases/tag/v0.2.0) <small>2022-09-06</small>

* Add functions (`get_daily_surfacewater_data()`) and adapt article 
[Surface Water](../articles/surface-water.html) for scraping all available daily 
surface water data and exporting to one `.csv` file for each parameter (containing
all monitoring stations)


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


