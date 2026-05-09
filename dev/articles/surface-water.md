# Surface Water

## Define Helper Functions

``` r

cat_file_enumeration <- function(files) {
  cat(paste0(collapse = "\n\n", sprintf(
    "- [%s](%s/%s)", 
    files, 
    "https://kwb-r.github.io/wasserportal",
    files
  )))
}
```

## Master Data

``` r

library(wasserportal)

stations <- wasserportal::get_stations()
#> Importing 10 station overviews from Wasserportal Berlin ... ok. (7.76 secs)
stations_list <- kwb.utils::selectElements(stations, "overview_list")

is_sw <- stringr::str_detect(names(stations_list), "surface")

files <- wasserportal::list_masters_data_to_csv(stations_list[is_sw])
#> Writing 'stations_surface-water_water-level.csv' ... ok. (0.08 secs) 
#> Writing 'stations_surface-water_flow.csv' ... ok. (0.00 secs) 
#> Writing 'stations_surface-water_temperature.csv' ... ok. (0.00 secs) 
#> Writing 'stations_surface-water_conductivity.csv' ... ok. (0.00 secs) 
#> Writing 'stations_surface-water_ph.csv' ... ok. (0.00 secs) 
#> Writing 'stations_surface-water_oxygen-concentration.csv' ... ok. (0.00 secs) 
#> Writing 'stations_surface-water_oxygen-saturation.csv' ... ok. (0.00 secs) 
#> Writing 'stations_surface-water_quality.csv' ... ok. (0.00 secs)
```

The following surface water master data `.csv` files are available for
download:

- [stations_surface-water_water-level.csv](https://kwb-r.github.io/wasserportal/stations_surface-water_water-level.csv)

- [stations_surface-water_flow.csv](https://kwb-r.github.io/wasserportal/stations_surface-water_flow.csv)

- [stations_surface-water_temperature.csv](https://kwb-r.github.io/wasserportal/stations_surface-water_temperature.csv)

- [stations_surface-water_conductivity.csv](https://kwb-r.github.io/wasserportal/stations_surface-water_conductivity.csv)

- [stations_surface-water_ph.csv](https://kwb-r.github.io/wasserportal/stations_surface-water_ph.csv)

- [stations_surface-water_oxygen-concentration.csv](https://kwb-r.github.io/wasserportal/stations_surface-water_oxygen-concentration.csv)

- [stations_surface-water_oxygen-saturation.csv](https://kwb-r.github.io/wasserportal/stations_surface-water_oxygen-saturation.csv)

- [stations_surface-water_quality.csv](https://kwb-r.github.io/wasserportal/stations_surface-water_quality.csv)

## Daily Surface Water Data & Overall Surface Water Quality

By running the code below all available `daily surface water` data of
monitoring stations from Wasserportal Berlin will be downloaded and
exported into one `.json` file for each parameter and all available
monitoring stations.

``` r

variables <- wasserportal::get_surfacewater_variables()

variables
#>          surface_water.water_level                 surface_water.flow 
#>                              "ows"                              "odf" 
#>          surface_water.temperature         surface_water.conductivity 
#>                              "owt"                              "olf" 
#>                   surface_water.ph surface_water.oxygen_concentration 
#>                              "oph"                              "oog" 
#>    surface_water.oxygen_saturation 
#>                              "oos"

sw_data_daily_list <- wasserportal::get_daily_surfacewater_data(
  stations,
  variables
)
#> Importing 'surface_water.water_level' ... Station id: 5865900 (1/66)
#> Reading 'variable_ows' for station 5865900 (station_5865900) ... ok. (1.28 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5827103 (2/66)
#> Reading 'variable_ows' for station 5827103 (station_5827103) ... ok. (0.77 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5870400 (3/66)
#> Reading 'variable_ows' for station 5870400 (station_5870400) ... ok. (0.89 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5865300 (4/66)
#> Reading 'variable_ows' for station 5865300 (station_5865300) ... ok. (0.82 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5819900 (5/66)
#> Reading 'variable_ows' for station 5819900 (station_5819900) ... ok. (1.72 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5864801 (6/66)
#> Reading 'variable_ows' for station 5864801 (station_5864801) ... ok. (0.47 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5861101 (7/66)
#> Reading 'variable_ows' for station 5861101 (station_5861101) ... ok. (0.68 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800107 (8/66)
#> Reading 'variable_ows' for station 5800107 (station_5800107) ... ok. (1.57 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800317 (9/66)
#> Reading 'variable_ows' for station 5800317 (station_5800317) ... ok. (0.87 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867003 (10/66)
#> Reading 'variable_ows' for station 5867003 (station_5867003) ... ok. (1.70 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867401 (11/66)
#> Reading 'variable_ows' for station 5867401 (station_5867401) ... ok. (0.90 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800301 (12/66)
#> Reading 'variable_ows' for station 5800301 (station_5800301) ... ok. (0.72 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800302 (13/66)
#> Reading 'variable_ows' for station 5800302 (station_5800302) ... ok. (1.68 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5863000 (14/66)
#> Reading 'variable_ows' for station 5863000 (station_5863000) ... ok. (0.53 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867900 (15/66)
#> Reading 'variable_ows' for station 5867900 (station_5867900) ... ok. (1.91 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867601 (16/66)
#> Reading 'variable_ows' for station 5867601 (station_5867601) ... ok. (0.51 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5827101 (17/66)
#> Reading 'variable_ows' for station 5827101 (station_5827101) ... ok. (0.95 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800320 (18/66)
#> Reading 'variable_ows' for station 5800320 (station_5800320) ... ok. (0.50 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800313 (19/66)
#> Reading 'variable_ows' for station 5800313 (station_5800313) ... ok. (0.70 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5860900 (20/66)
#> Reading 'variable_ows' for station 5860900 (station_5860900) ... ok. (0.49 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867101 (21/66)
#> Reading 'variable_ows' for station 5867101 (station_5867101) ... ok. (0.57 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800303 (22/66)
#> Reading 'variable_ows' for station 5800303 (station_5800303) ... ok. (0.57 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5826702 (23/66)
#> Reading 'variable_ows' for station 5826702 (station_5826702) ... ok. (0.88 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800305 (24/66)
#> Reading 'variable_ows' for station 5800305 (station_5800305) ... ok. (1.53 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800304 (25/66)
#> Reading 'variable_ows' for station 5800304 (station_5800304) ... ok. (0.57 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800306 (26/66)
#> Reading 'variable_ows' for station 5800306 (station_5800306) ... ok. (0.80 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5861000 (27/66)
#> Reading 'variable_ows' for station 5861000 (station_5861000) ... ok. (1.02 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867300 (28/66)
#> Reading 'variable_ows' for station 5867300 (station_5867300) ... ok. (0.97 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5864700 (29/66)
#> Reading 'variable_ows' for station 5864700 (station_5864700) ... ok. (0.43 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5865000 (30/66)
#> Reading 'variable_ows' for station 5865000 (station_5865000) ... ok. (0.58 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5865200 (31/66)
#> Reading 'variable_ows' for station 5865200 (station_5865200) ... ok. (0.64 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800308 (32/66)
#> Reading 'variable_ows' for station 5800308 (station_5800308) ... ok. (0.78 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800321 (33/66)
#> Reading 'variable_ows' for station 5800321 (station_5800321) ... ok. (1.45 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867100 (34/66)
#> Reading 'variable_ows' for station 5867100 (station_5867100) ... ok. (0.89 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5869700 (35/66)
#> Reading 'variable_ows' for station 5869700 (station_5869700) ... ok. (0.55 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867500 (36/66)
#> Reading 'variable_ows' for station 5867500 (station_5867500) ... ok. (1.41 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5870100 (37/66)
#> Reading 'variable_ows' for station 5870100 (station_5870100) ... ok. (1.51 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800309 (38/66)
#> Reading 'variable_ows' for station 5800309 (station_5800309) ... ok. (1.38 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5819901 (39/66)
#> Reading 'variable_ows' for station 5819901 (station_5819901) ... ok. (0.72 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800318 (40/66)
#> Reading 'variable_ows' for station 5800318 (station_5800318) ... ok. (0.60 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5826701 (41/66)
#> Reading 'variable_ows' for station 5826701 (station_5826701) ... ok. (1.10 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5866403 (42/66)
#> Reading 'variable_ows' for station 5866403 (station_5866403) ... ok. (1.37 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800315 (43/66)
#> Reading 'variable_ows' for station 5800315 (station_5800315) ... ok. (0.62 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800316 (44/66)
#> Reading 'variable_ows' for station 5800316 (station_5800316) ... ok. (0.57 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867700 (45/66)
#> Reading 'variable_ows' for station 5867700 (station_5867700) ... ok. (0.84 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5803500 (46/66)
#> Reading 'variable_ows' for station 5803500 (station_5803500) ... ok. (1.72 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800312 (47/66)
#> Reading 'variable_ows' for station 5800312 (station_5800312) ... ok. (0.56 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867000 (48/66)
#> Reading 'variable_ows' for station 5867000 (station_5867000) ... ok. (1.10 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800106 (49/66)
#> Reading 'variable_ows' for station 5800106 (station_5800106) ... ok. (0.57 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5866700 (50/66)
#> Reading 'variable_ows' for station 5866700 (station_5866700) ... ok. (0.76 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5866800 (51/66)
#> Reading 'variable_ows' for station 5866800 (station_5866800) ... ok. (1.64 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5862811 (52/66)
#> Reading 'variable_ows' for station 5862811 (station_5862811) ... ok. (0.88 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867600 (53/66)
#> Reading 'variable_ows' for station 5867600 (station_5867600) ... ok. (0.43 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5827700 (54/66)
#> Reading 'variable_ows' for station 5827700 (station_5827700) ... ok. (1.62 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5820000 (55/66)
#> Reading 'variable_ows' for station 5820000 (station_5820000) ... ok. (1.15 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5815911 (56/66)
#> Reading 'variable_ows' for station 5815911 (station_5815911) ... ok. (0.78 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5803200 (57/66)
#> Reading 'variable_ows' for station 5803200 (station_5803200) ... ok. (1.77 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867301 (58/66)
#> Reading 'variable_ows' for station 5867301 (station_5867301) ... ok. (0.71 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867201 (59/66)
#> Reading 'variable_ows' for station 5867201 (station_5867201) ... ok. (1.01 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867202 (60/66)
#> Reading 'variable_ows' for station 5867202 (station_5867202) ... ok. (0.90 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800310 (61/66)
#> Reading 'variable_ows' for station 5800310 (station_5800310) ... ok. (0.61 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800314 (62/66)
#> Reading 'variable_ows' for station 5800314 (station_5800314) ... ok. (1.10 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5861200 (63/66)
#> Reading 'variable_ows' for station 5861200 (station_5861200) ... ok. (2.11 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867001 (64/66)
#> Reading 'variable_ows' for station 5867001 (station_5867001) ... ok. (1.48 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5864800 (65/66)
#> Reading 'variable_ows' for station 5864800 (station_5864800) ... ok. (0.80 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5866301 (66/66)
#> Reading 'variable_ows' for station 5866301 (station_5866301) ... ok. (0.89 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> ok. (1.15 mins) 
#> Importing 'surface_water.flow' ... Station id: 5865300 (1/15)
#> Reading 'variable_odf' for station 5865300 (station_5865300) ... ok. (0.81 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5864801 (2/15)
#> Reading 'variable_odf' for station 5864801 (station_5864801) ... ok. (0.68 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867401 (3/15)
#> Reading 'variable_odf' for station 5867401 (station_5867401) ... ok. (0.80 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867900 (4/15)
#> Reading 'variable_odf' for station 5867900 (station_5867900) ... ok. (0.87 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867601 (5/15)
#> Reading 'variable_odf' for station 5867601 (station_5867601) ... ok. (0.35 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5827101 (6/15)
#> Reading 'variable_odf' for station 5827101 (station_5827101) ... ok. (0.49 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5865200 (7/15)
#> Reading 'variable_odf' for station 5865200 (station_5865200) ... ok. (1.44 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5870100 (8/15)
#> Reading 'variable_odf' for station 5870100 (station_5870100) ... ok. (1.04 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5826701 (9/15)
#> Reading 'variable_odf' for station 5826701 (station_5826701) ... ok. (0.79 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5862811 (10/15)
#> Reading 'variable_odf' for station 5862811 (station_5862811) ... ok. (0.67 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5827700 (11/15)
#> Reading 'variable_odf' for station 5827700 (station_5827700) ... ok. (1.62 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5803200 (12/15)
#> Reading 'variable_odf' for station 5803200 (station_5803200) ... ok. (1.25 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5861200 (13/15)
#> Reading 'variable_odf' for station 5861200 (station_5861200) ... ok. (0.80 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5864800 (14/15)
#> Reading 'variable_odf' for station 5864800 (station_5864800) ... ok. (0.69 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5866301 (15/15)
#> Reading 'variable_odf' for station 5866301 (station_5866301) ... ok. (0.76 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> ok. (13.98 secs) 
#> Importing 'surface_water.temperature' ... Station id: 601 (1/65)
#> Reading 'variable_owt' for station 601 (station_601) ... ok. (1.04 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 151 (2/65)
#> Reading 'variable_owt' for station 151 (station_151) ... ok. (0.96 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 153 (3/65)
#> Reading 'variable_owt' for station 153 (station_153) ... ok. (0.79 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 139 (4/65)
#> Reading 'variable_owt' for station 139 (station_139) ... ok. (0.28 secs) 
#> Removing 1 elements that are empty or failed (variables: 'owt') ... ok. (0.00 secs)
#> No remaining data frames. Returning NULL.
#> Station id: 509 (5/65)
#> Reading 'variable_owt' for station 509 (station_509) ... ok. (1.25 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 504 (6/65)
#> Reading 'variable_owt' for station 504 (station_504) ... ok. (1.11 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 414 (7/65)
#> Reading 'variable_owt' for station 414 (station_414) ... ok. (1.15 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 141 (8/65)
#> Reading 'variable_owt' for station 141 (station_141) ... ok. (1.48 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 111 (9/65)
#> Reading 'variable_owt' for station 111 (station_111) ... ok. (1.47 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 211 (10/65)
#> Reading 'variable_owt' for station 211 (station_211) ... ok. (1.41 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 161 (11/65)
#> Reading 'variable_owt' for station 161 (station_161) ... ok. (1.89 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 421 (12/65)
#> Reading 'variable_owt' for station 421 (station_421) ... ok. (1.86 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5865900 (13/65)
#> Reading 'variable_owt' for station 5865900 (station_5865900) ... ok. (0.80 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5870400 (14/65)
#> Reading 'variable_owt' for station 5870400 (station_5870400) ... ok. (0.62 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5865300 (15/65)
#> Reading 'variable_owt' for station 5865300 (station_5865300) ... ok. (0.63 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5861101 (16/65)
#> Reading 'variable_owt' for station 5861101 (station_5861101) ... ok. (0.69 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800107 (17/65)
#> Reading 'variable_owt' for station 5800107 (station_5800107) ... ok. (0.79 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800317 (18/65)
#> Reading 'variable_owt' for station 5800317 (station_5800317) ... ok. (0.78 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867003 (19/65)
#> Reading 'variable_owt' for station 5867003 (station_5867003) ... ok. (0.87 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867401 (20/65)
#> Reading 'variable_owt' for station 5867401 (station_5867401) ... ok. (0.90 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800301 (21/65)
#> Reading 'variable_owt' for station 5800301 (station_5800301) ... ok. (0.60 secs) 
#> Removing 1 elements that are empty or failed (variables: 'owt') ... ok. (0.00 secs)
#> No remaining data frames. Returning NULL.
#> Station id: 5800302 (22/65)
#> Reading 'variable_owt' for station 5800302 (station_5800302) ... ok. (0.72 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867900 (23/65)
#> Reading 'variable_owt' for station 5867900 (station_5867900) ... ok. (0.70 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867601 (24/65)
#> Reading 'variable_owt' for station 5867601 (station_5867601) ... ok. (0.56 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5827101 (25/65)
#> Reading 'variable_owt' for station 5827101 (station_5827101) ... ok. (0.78 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800320 (26/65)
#> Reading 'variable_owt' for station 5800320 (station_5800320) ... ok. (0.66 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867101 (27/65)
#> Reading 'variable_owt' for station 5867101 (station_5867101) ... ok. (0.72 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800303 (28/65)
#> Reading 'variable_owt' for station 5800303 (station_5800303) ... ok. (0.86 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800305 (29/65)
#> Reading 'variable_owt' for station 5800305 (station_5800305) ... ok. (0.79 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800304 (30/65)
#> Reading 'variable_owt' for station 5800304 (station_5800304) ... ok. (0.94 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800306 (31/65)
#> Reading 'variable_owt' for station 5800306 (station_5800306) ... ok. (0.85 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867300 (32/65)
#> Reading 'variable_owt' for station 5867300 (station_5867300) ... ok. (0.85 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5864700 (33/65)
#> Reading 'variable_owt' for station 5864700 (station_5864700) ... ok. (0.64 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5865000 (34/65)
#> Reading 'variable_owt' for station 5865000 (station_5865000) ... ok. (0.67 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5865200 (35/65)
#> Reading 'variable_owt' for station 5865200 (station_5865200) ... ok. (0.72 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800308 (36/65)
#> Reading 'variable_owt' for station 5800308 (station_5800308) ... ok. (0.45 secs) 
#> Removing 1 elements that are empty or failed (variables: 'owt') ... ok. (0.00 secs)
#> No remaining data frames. Returning NULL.
#> Station id: 5800321 (37/65)
#> Reading 'variable_owt' for station 5800321 (station_5800321) ... ok. (0.48 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867100 (38/65)
#> Reading 'variable_owt' for station 5867100 (station_5867100) ... ok. (0.81 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5869700 (39/65)
#> Reading 'variable_owt' for station 5869700 (station_5869700) ... ok. (0.87 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867500 (40/65)
#> Reading 'variable_owt' for station 5867500 (station_5867500) ... ok. (1.68 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5870100 (41/65)
#> Reading 'variable_owt' for station 5870100 (station_5870100) ... ok. (0.81 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800309 (42/65)
#> Reading 'variable_owt' for station 5800309 (station_5800309) ... ok. (0.80 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5819901 (43/65)
#> Reading 'variable_owt' for station 5819901 (station_5819901) ... ok. (0.67 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800318 (44/65)
#> Reading 'variable_owt' for station 5800318 (station_5800318) ... ok. (0.84 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5826701 (45/65)
#> Reading 'variable_owt' for station 5826701 (station_5826701) ... ok. (0.83 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5866403 (46/65)
#> Reading 'variable_owt' for station 5866403 (station_5866403) ... ok. (0.50 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800315 (47/65)
#> Reading 'variable_owt' for station 5800315 (station_5800315) ... ok. (0.83 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800316 (48/65)
#> Reading 'variable_owt' for station 5800316 (station_5800316) ... ok. (0.73 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867700 (49/65)
#> Reading 'variable_owt' for station 5867700 (station_5867700) ... ok. (0.70 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5803500 (50/65)
#> Reading 'variable_owt' for station 5803500 (station_5803500) ... ok. (0.90 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800312 (51/65)
#> Reading 'variable_owt' for station 5800312 (station_5800312) ... ok. (0.71 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867000 (52/65)
#> Reading 'variable_owt' for station 5867000 (station_5867000) ... ok. (0.81 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800106 (53/65)
#> Reading 'variable_owt' for station 5800106 (station_5800106) ... ok. (0.74 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5866700 (54/65)
#> Reading 'variable_owt' for station 5866700 (station_5866700) ... ok. (0.83 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5866800 (55/65)
#> Reading 'variable_owt' for station 5866800 (station_5866800) ... ok. (0.68 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867600 (56/65)
#> Reading 'variable_owt' for station 5867600 (station_5867600) ... ok. (0.63 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5820000 (57/65)
#> Reading 'variable_owt' for station 5820000 (station_5820000) ... ok. (0.81 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5803200 (58/65)
#> Reading 'variable_owt' for station 5803200 (station_5803200) ... ok. (1.87 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867301 (59/65)
#> Reading 'variable_owt' for station 5867301 (station_5867301) ... ok. (0.84 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867201 (60/65)
#> Reading 'variable_owt' for station 5867201 (station_5867201) ... ok. (0.74 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5867202 (61/65)
#> Reading 'variable_owt' for station 5867202 (station_5867202) ... ok. (0.87 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5800310 (62/65)
#> Reading 'variable_owt' for station 5800310 (station_5800310) ... ok. (0.71 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5861200 (63/65)
#> Reading 'variable_owt' for station 5861200 (station_5861200) ... ok. (1.72 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5864800 (64/65)
#> Reading 'variable_owt' for station 5864800 (station_5864800) ... ok. (0.80 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 5866301 (65/65)
#> Reading 'variable_owt' for station 5866301 (station_5866301) ... ok. (0.67 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Empty data frame when looping through 'sw_data_list' in sw_data_list_to_df()
#> Warning in min(which(stringr::str_detect(metadata, ":"))): no non-missing
#> arguments to min; returning Inf
#> Empty data frame when looping through 'sw_data_list' in sw_data_list_to_df()
#> Warning in min(which(stringr::str_detect(metadata, ":"))): no non-missing
#> arguments to min; returning Inf
#> Empty data frame when looping through 'sw_data_list' in sw_data_list_to_df()
#> Warning in min(which(stringr::str_detect(metadata, ":"))): no non-missing
#> arguments to min; returning Inf
#> ok. (1.01 mins) 
#> Importing 'surface_water.conductivity' ... Station id: 601 (1/12)
#> Reading 'variable_olf' for station 601 (station_601) ... ok. (1.21 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 151 (2/12)
#> Reading 'variable_olf' for station 151 (station_151) ... ok. (2.01 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 153 (3/12)
#> Reading 'variable_olf' for station 153 (station_153) ... ok. (1.05 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 139 (4/12)
#> Reading 'variable_olf' for station 139 (station_139) ... ok. (0.44 secs) 
#> Removing 1 elements that are empty or failed (variables: 'olf') ... ok. (0.00 secs)
#> No remaining data frames. Returning NULL.
#> Station id: 509 (5/12)
#> Reading 'variable_olf' for station 509 (station_509) ... ok. (1.23 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 504 (6/12)
#> Reading 'variable_olf' for station 504 (station_504) ... ok. (1.22 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 414 (7/12)
#> Reading 'variable_olf' for station 414 (station_414) ... ok. (1.18 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 141 (8/12)
#> Reading 'variable_olf' for station 141 (station_141) ... ok. (1.53 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 111 (9/12)
#> Reading 'variable_olf' for station 111 (station_111) ... ok. (1.53 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 211 (10/12)
#> Reading 'variable_olf' for station 211 (station_211) ... ok. (1.49 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 161 (11/12)
#> Reading 'variable_olf' for station 161 (station_161) ... ok. (1.81 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 421 (12/12)
#> Reading 'variable_olf' for station 421 (station_421) ... ok. (1.90 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Empty data frame when looping through 'sw_data_list' in sw_data_list_to_df()
#> Warning in min(which(stringr::str_detect(metadata, ":"))): no non-missing
#> arguments to min; returning Inf
#> ok. (17.55 secs) 
#> Importing 'surface_water.ph' ... Station id: 601 (1/12)
#> Reading 'variable_oph' for station 601 (station_601) ... ok. (1.09 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 151 (2/12)
#> Reading 'variable_oph' for station 151 (station_151) ... ok. (1.15 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 153 (3/12)
#> Reading 'variable_oph' for station 153 (station_153) ... ok. (0.91 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 139 (4/12)
#> Reading 'variable_oph' for station 139 (station_139) ... ok. (0.45 secs) 
#> Removing 1 elements that are empty or failed (variables: 'oph') ... ok. (0.00 secs)
#> No remaining data frames. Returning NULL.
#> Station id: 509 (5/12)
#> Reading 'variable_oph' for station 509 (station_509) ... ok. (1.13 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 504 (6/12)
#> Reading 'variable_oph' for station 504 (station_504) ... ok. (1.20 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 414 (7/12)
#> Reading 'variable_oph' for station 414 (station_414) ... ok. (1.18 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 141 (8/12)
#> Reading 'variable_oph' for station 141 (station_141) ... ok. (1.47 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 111 (9/12)
#> Reading 'variable_oph' for station 111 (station_111) ... ok. (1.51 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 211 (10/12)
#> Reading 'variable_oph' for station 211 (station_211) ... ok. (1.49 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 161 (11/12)
#> Reading 'variable_oph' for station 161 (station_161) ... ok. (1.90 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 421 (12/12)
#> Reading 'variable_oph' for station 421 (station_421) ... ok. (1.94 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Empty data frame when looping through 'sw_data_list' in sw_data_list_to_df()
#> Warning in min(which(stringr::str_detect(metadata, ":"))): no non-missing
#> arguments to min; returning Inf
#> ok. (16.66 secs) 
#> Importing 'surface_water.oxygen_concentration' ... Station id: 601 (1/12)
#> Reading 'variable_oog' for station 601 (station_601) ... ok. (1.19 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 151 (2/12)
#> Reading 'variable_oog' for station 151 (station_151) ... ok. (1.13 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 153 (3/12)
#> Reading 'variable_oog' for station 153 (station_153) ... ok. (0.87 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 139 (4/12)
#> Reading 'variable_oog' for station 139 (station_139) ... ok. (1.46 secs) 
#> Removing 1 elements that are empty or failed (variables: 'oog') ... ok. (0.00 secs)
#> No remaining data frames. Returning NULL.
#> Station id: 509 (5/12)
#> Reading 'variable_oog' for station 509 (station_509) ... ok. (1.24 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 504 (6/12)
#> Reading 'variable_oog' for station 504 (station_504) ... ok. (1.22 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 414 (7/12)
#> Reading 'variable_oog' for station 414 (station_414) ... ok. (1.19 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 141 (8/12)
#> Reading 'variable_oog' for station 141 (station_141) ... ok. (1.41 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 111 (9/12)
#> Reading 'variable_oog' for station 111 (station_111) ... ok. (1.49 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 211 (10/12)
#> Reading 'variable_oog' for station 211 (station_211) ... ok. (1.53 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 161 (11/12)
#> Reading 'variable_oog' for station 161 (station_161) ... ok. (1.92 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 421 (12/12)
#> Reading 'variable_oog' for station 421 (station_421) ... ok. (1.80 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Empty data frame when looping through 'sw_data_list' in sw_data_list_to_df()
#> Warning in min(which(stringr::str_detect(metadata, ":"))): no non-missing
#> arguments to min; returning Inf
#> ok. (17.56 secs) 
#> Importing 'surface_water.oxygen_saturation' ... Station id: 601 (1/12)
#> Reading 'variable_oos' for station 601 (station_601) ... ok. (1.02 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 151 (2/12)
#> Reading 'variable_oos' for station 151 (station_151) ... ok. (1.11 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 153 (3/12)
#> Reading 'variable_oos' for station 153 (station_153) ... ok. (0.90 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 139 (4/12)
#> Reading 'variable_oos' for station 139 (station_139) ... ok. (0.33 secs) 
#> Removing 1 elements that are empty or failed (variables: 'oos') ... ok. (0.00 secs)
#> No remaining data frames. Returning NULL.
#> Station id: 509 (5/12)
#> Reading 'variable_oos' for station 509 (station_509) ... ok. (1.19 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 504 (6/12)
#> Reading 'variable_oos' for station 504 (station_504) ... ok. (1.19 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 414 (7/12)
#> Reading 'variable_oos' for station 414 (station_414) ... ok. (1.16 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 141 (8/12)
#> Reading 'variable_oos' for station 141 (station_141) ... ok. (1.34 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 111 (9/12)
#> Reading 'variable_oos' for station 111 (station_111) ... ok. (1.27 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 211 (10/12)
#> Reading 'variable_oos' for station 211 (station_211) ... ok. (1.39 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 161 (11/12)
#> Reading 'variable_oos' for station 161 (station_161) ... ok. (1.70 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Station id: 421 (12/12)
#> Reading 'variable_oos' for station 421 (station_421) ... ok. (1.85 secs)
#> Warning in warning_not_implemented("merge_raw_results_daily()"):
#> merge_raw_results_daily() is not yet implemented. Returning raw data
#> Empty data frame when looping through 'sw_data_list' in sw_data_list_to_df()
#> Warning in min(which(stringr::str_detect(metadata, ":"))): no non-missing
#> arguments to min; returning Inf
#> ok. (15.53 secs)

files <- wasserportal::list_timeseries_data_to_zip(sw_data_daily_list)
#> Writing 'daily_surface-water_water-level.zip' ... ok. (0.91 secs) 
#> Writing 'daily_surface-water_flow.zip' ... ok. (0.17 secs) 
#> Writing 'daily_surface-water_temperature.zip' ... ok. (0.87 secs) 
#> Writing 'daily_surface-water_conductivity.zip' ... ok. (0.26 secs) 
#> Writing 'daily_surface-water_ph.zip' ... ok. (0.32 secs) 
#> Writing 'daily_surface-water_oxygen-concentration.zip' ... ok. (0.34 secs) 
#> Writing 'daily_surface-water_oxygen-saturation.zip' ... ok. (0.28 secs)
files
#> [1] "daily_surface-water_water-level.zip"         
#> [2] "daily_surface-water_flow.zip"                
#> [3] "daily_surface-water_temperature.zip"         
#> [4] "daily_surface-water_conductivity.zip"        
#> [5] "daily_surface-water_ph.zip"                  
#> [6] "daily_surface-water_oxygen-concentration.zip"
#> [7] "daily_surface-water_oxygen-saturation.zip"

# Data availability per parameter
sw_data_daily_list %>%
  dplyr::bind_rows() %>% 
  dplyr::count(Parameter, Einheit)
#>             Parameter Einheit      n
#> 1          Durchfluss    m³/s 120477
#> 2       Leitfähigkeit   µS/cm  89304
#> 3    Sauerstoffgehalt    mg/l  88111
#> 4 Sauerstoffsättigung       %  88106
#> 5         Wasserstand      cm 578444
#> 6   Wasssertemperatur      °C 251858
#> 7             pH-Wert          88866


station_ids <- stations$overview_list$surface_water.quality$Messstellennummer

swq_data <- wasserportal::get_surfacewater_qualities(station_ids)
#> Downloading surface water quality data ... 
#> 01/56: station_id = '102' ... ok. (0.52 secs) 
#> 02/56: station_id = '105' ... ok. (1.71 secs) 
#> 03/56: station_id = '110' ... ok. (3.76 secs) 
#> 04/56: station_id = '115' ... ok. (2.74 secs) 
#> 05/56: station_id = '120' ... ok. (2.27 secs) 
#> 06/56: station_id = '125' ... ok. (2.67 secs) 
#> 07/56: station_id = '130' ... ok. (3.84 secs) 
#> 08/56: station_id = '135' ... ok. (2.38 secs) 
#> 09/56: station_id = '140' ... ok. (4.04 secs) 
#> 10/56: station_id = '150' ... ok. (2.87 secs) 
#> 11/56: station_id = '160' ... ok. (6.37 secs) 
#> 12/56: station_id = '215' ... ok. (5.30 secs) 
#> 13/56: station_id = '220' ... ok. (1.89 secs) 
#> 14/56: station_id = '225' ... ok. (1.57 secs) 
#> 15/56: station_id = '228' ... ok. (0.38 secs) 
#> 16/56: station_id = '230' ... ok. (2.86 secs) 
#> 17/56: station_id = '305' ... ok. (2.64 secs) 
#> 18/56: station_id = '311' ... ok. (1.16 secs) 
#> 19/56: station_id = '315' ... ok. (1.81 secs) 
#> 20/56: station_id = '316' ... ok. (0.51 secs) 
#> 21/56: station_id = '320' ... ok. (4.81 secs) 
#> 22/56: station_id = '325' ... ok. (2.71 secs) 
#> 23/56: station_id = '330' ... ok. (2.08 secs) 
#> 24/56: station_id = '340' ... ok. (2.04 secs) 
#> 25/56: station_id = '345' ... ok. (4.50 secs) 
#> 26/56: station_id = '350' ... ok. (2.22 secs) 
#> 27/56: station_id = '355' ... ok. (2.10 secs) 
#> 28/56: station_id = '407' ... ok. (0.93 secs) 
#> 29/56: station_id = '410' ... ok. (3.36 secs) 
#> 30/56: station_id = '415' ... ok. (1.63 secs) 
#> 31/56: station_id = '420' ... ok. (3.78 secs) 
#> 32/56: station_id = '430' ... ok. (8.17 secs) 
#> 33/56: station_id = '505' ... ok. (2.44 secs) 
#> 34/56: station_id = '510' ... ok. (2.13 secs) 
#> 35/56: station_id = '515' ... ok. (3.44 secs) 
#> 36/56: station_id = '602' ... ok. (0.54 secs) 
#> 37/56: station_id = '605' ... ok. (2.34 secs) 
#> 38/56: station_id = '606' ... ok. (0.53 secs) 
#> 39/56: station_id = '610' ... ok. (1.89 secs) 
#> 40/56: station_id = '710' ... ok. (3.03 secs) 
#> 41/56: station_id = '714' ... ok. (0.51 secs) 
#> 42/56: station_id = '715' ... ok. (1.66 secs) 
#> 43/56: station_id = '718' ... ok. (0.93 secs) 
#> 44/56: station_id = '719' ... ok. (1.27 secs) 
#> 45/56: station_id = '723' ... ok. (0.76 secs) 
#> 46/56: station_id = '725' ... ok. (1.65 secs) 
#> 47/56: station_id = '728' ... ok. (2.56 secs) 
#> 48/56: station_id = '729' ... ok. (0.91 secs) 
#> 49/56: station_id = '730' ... ok. (2.30 secs) 
#> 50/56: station_id = '740' ... ok. (1.35 secs) 
#> 51/56: station_id = '741' ... ok. (1.06 secs) 
#> 52/56: station_id = '742' ... ok. (1.18 secs) 
#> 53/56: station_id = '803' ... ok. (0.79 secs) 
#> 54/56: station_id = '805' ... ok. (1.36 secs) 
#> 55/56: station_id = '807' ... ok. (0.94 secs) 
#> 56/56: station_id = '815' ... ok. (1.69 secs) 
#> ok. (2.12 mins)

files1 <- wasserportal::list_timeseries_data_to_zip(
  list("surface-water_quality" = swq_data)
  )
#> Writing 'daily_surface-water-quality.zip' ... ok. (1.36 secs)

files2 <- "surface-water_quality.zip"

file.rename(files1, files2)
#> [1] TRUE
```

The following `.zip` files are available for download:

- [daily_surface-water_water-level.zip](https://kwb-r.github.io/wasserportal/daily_surface-water_water-level.zip)

- [daily_surface-water_flow.zip](https://kwb-r.github.io/wasserportal/daily_surface-water_flow.zip)

- [daily_surface-water_temperature.zip](https://kwb-r.github.io/wasserportal/daily_surface-water_temperature.zip)

- [daily_surface-water_conductivity.zip](https://kwb-r.github.io/wasserportal/daily_surface-water_conductivity.zip)

- [daily_surface-water_ph.zip](https://kwb-r.github.io/wasserportal/daily_surface-water_ph.zip)

- [daily_surface-water_oxygen-concentration.zip](https://kwb-r.github.io/wasserportal/daily_surface-water_oxygen-concentration.zip)

- [daily_surface-water_oxygen-saturation.zip](https://kwb-r.github.io/wasserportal/daily_surface-water_oxygen-saturation.zip)

- [surface-water_quality.zip](https://kwb-r.github.io/wasserportal/surface-water_quality.zip)

## Daily Surface Water Levels

``` r

swl_master <- wasserportal::get_wasserportal_masters_data(
  master_urls = stations_list$surface_water.water_level %>%
    dplyr::filter(.data$Betreiber == "Land Berlin") %>%
    dplyr::pull(.data$stammdaten_link)
)
#> Importing master data for 66 stations from Wasserportal Berlin ... ok. (30.83 secs)

column_level_zero <- "Pegelnullpunkt_m_NHN"
  
swl_data <- sw_data_daily_list$surface_water.water_level %>% 
  dplyr::select(where(~!all(is.na(.x)))) %>%
  dplyr::left_join(
    kwb.utils::selectColumns(swl_master, c("Nummer", column_level_zero)),
    by = c(Messstellennummer = "Nummer")
  ) %>%
  dplyr::mutate(
    Tagesmittelwert_Pegelstand_mNN = as.numeric(.data[[column_level_zero]]) + 
      .data$Tagesmittelwert / 100
  ) %>%
  ### remove -777 for messstellennummer 5867000 (few values in 2000) resulted by
  ### step above
  dplyr::filter(.data[["Tagesmittelwert_Pegelstand_mNN"]] != -777) %>%
  kwb.utils::removeColumns(column_level_zero)

str(swl_data)
#> 'data.frame':    577362 obs. of  6 variables:
#>  $ Messstellennummer             : chr  "5865900" "5865900" "5865900" "5865900" ...
#>  $ Datum                         : Date, format: "1999-11-01" "1999-11-02" ...
#>  $ Tagesmittelwert               : int  16 23 19 17 17 15 14 14 19 19 ...
#>  $ Parameter                     : chr  "Wasserstand" "Wasserstand" "Wasserstand" "Wasserstand" ...
#>  $ Einheit                       : chr  "cm" "cm" "cm" "cm" ...
#>  $ Tagesmittelwert_Pegelstand_mNN: num  45.8 45.8 45.8 45.8 45.8 ...
#>  - attr(*, "metadata")= chr [1:5] "Stationsnummer: 5865900" "Stationsname: Allee der Kosmonauten" "Gewässer: M.-H.-Grenzgr." "Wasserstand in cm" ...
```
