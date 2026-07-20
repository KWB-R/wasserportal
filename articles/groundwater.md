# Groundwater

## Define URLs and Helper Functions

``` r

`%>%` <- magrittr::`%>%`

urls <- kwb.utils::resolve(list(
  gh_wasserportal = "https://kwb-r.github.io/wasserportal",
  stations_gwl_meta = "<gh_wasserportal>/stations_gwl_master.json",
  stations_gwl_data = "<gh_wasserportal>/stations_gwl_data.json",
  stations_gwq_meta = "<gh_wasserportal>/stations_gwq_master.json",
  stations_gwq_data = "<gh_wasserportal>/stations_gwq_data.json",
  stations_crosstable = "<gh_wasserportal>/stations_crosstable.json"
))

top_filter_data_table <- function(data) {
  DT::datatable(data, filter = "top")
}

cat_file_enumeration <- function(base_url, files) {
  cat(paste0(
    sprintf("- [%s](%s/%s)", files, base_url, files),
    collapse = "\n\n"
  ))
}
```

## Master Data

``` r

stations_list <- wasserportal::get_stations(type = "list")
#> Importing 10 station overviews from Wasserportal Berlin ... ok. (9.84 secs)

is_gw <- stringr::str_detect(names(stations_list), "groundwater")

files <- wasserportal::list_masters_data_to_csv(stations_list[is_gw])
#> Writing 'stations_groundwater_level.csv' ... ok. (0.14 secs) 
#> Writing 'stations_groundwater_quality.csv' ... ok. (0.01 secs)
```

The following groundwater master data `.csv` files are available for
download:

- [stations_groundwater_level.csv](https://kwb-r.github.io/wasserportal/stations_groundwater_level.csv)

- [stations_groundwater_quality.csv](https://kwb-r.github.io/wasserportal/stations_groundwater_quality.csv)

## Get Groundwater Data

``` r

if (use_random_subset_of_stations) {
  stations_list_bak <- stations_list
  x <- stations_list$groundwater.level[sample(876, 10), ]
  stations_list$groundwater.level <- x
  x <- stations_list$groundwater.quality[sample(208, 10), ]
  stations_list$groundwater.quality <- x
}

gw_data_list <- wasserportal::get_groundwater_data(
  stations_list = stations_list, 
  debug = TRUE
)
#> Importing 'groundwater.level' data (1/2) ... ok. (9.72 mins) 
#> Importing 'groundwater.quality' data (2/2) ... ok. (1.88 mins)

files <- wasserportal::list_timeseries_data_to_zip(gw_data_list)
#> Writing 'groundwater_level.zip' ... ok. (14.82 secs) 
#> Writing 'groundwater_quality.zip' ... ok. (1.27 secs)

files
#> [1] "groundwater_level.zip"   "groundwater_quality.zip"

# Data availability per parameter
gw_data_list %>%
  dplyr::bind_rows() %>% 
  dplyr::count(Parameter, Einheit) %>% 
  dplyr::arrange(dplyr::desc(.data$n))
#>                 Parameter   Einheit       n
#>                    <char>    <char>   <int>
#>   1:             GW-Stand m ü. NHN) 8646423
#>   2:              Chlorid     mg/l]    9608
#>   3:              Kalzium     mg/l]    9607
#>   4:               Sulfat     mg/l]    9603
#>   5:            Magnesium     mg/l]    9595
#>  ---                                       
#> 250:        Phenobarbital     µg/l]       2
#> 251:    Tetrachlorethylen     µg/l]       2
#> 252:      Trichlorethylen     µg/l]       2
#> 253: Gesamtmineralisation     mg/l]       1
#> 254:         gelöstes CO2     mg/l]       1
```

The following groundwater data `.zip` files are available for download:

- [groundwater_level.zip](https://kwb-r.github.io/wasserportal/groundwater_level.zip)

- [groundwater_quality.zip](https://kwb-r.github.io/wasserportal/groundwater_quality.zip)

## Do Your Own Analysis!

Download CSV/JSON/ZIP files scraped and prepared each day at 5 a.m. UTC
for re-use in R. The following data are available:

- **Data availability**

- [stations_crosstable.json](https://kwb-r.github.io/wasserportal/stations_crosstable.json):
  available parameters per station (see
  [`wasserportal::get_overview_options()`](https://kwb-r.github.io/wasserportal/reference/get_overview_options.md)
  for available options). Note: includes also surface monitoring
  stations!

``` r

library(wasserportal)

stations_crosstable <- jsonlite::fromJSON(urls$stations_crosstable)

str(stations_crosstable)
#> 'data.frame':    1116 obs. of  12 variables:
#>  $ Messstellennummer: chr  "5865900" "5827103" "5870400" "5865300" ...
#>  $ Messstellenname  : chr  "Allee der Kosmonauten" "Allendestraße" "Alsenbrücke" "Am Bahndamm" ...
#>  $ ows              : chr  "x" "x" "x" "x" ...
#>  $ owt              : chr  "x" NA "x" "x" ...
#>  $ odf              : chr  NA NA NA "x" ...
#>  $ olf              : chr  NA NA NA NA ...
#>  $ oph              : chr  NA NA NA NA ...
#>  $ oog              : chr  NA NA NA NA ...
#>  $ oos              : chr  NA NA NA NA ...
#>  $ opq              : chr  NA NA NA NA ...
#>  $ gws              : chr  NA NA NA NA ...
#>  $ gwq              : chr  NA NA NA NA ...
```

- **Master Data**

- [stations_gwl_master.json](https://kwb-r.github.io/wasserportal/stations_gwl_master.json):
  for GW level stations

- [stations_gwq_master.json](https://kwb-r.github.io/wasserportal/stations_gwq_master.json):
  for GW quality stations

- **Measurements**

- [stations_gwl_data.json](https://kwb-r.github.io/wasserportal/stations_gwl_data.json):
  GW level measurements for stations

- [stations_gwq_data.json](https://kwb-r.github.io/wasserportal/stations_gwq_data.json):
  GW quality measurements for all available parameters and stations

Please find an example below for merging all this information into a
single data frame:

``` r

library(wasserportal)

site_number_to_character <- function(data) {
  data %>%
    dplyr::mutate(
      Messstellennummer = as.character(.data$Messstellennummer)
    )
}

left_join_by_site <- function(data, master_data) {
  data %>%
    dplyr::left_join(master_data, by = c("Messstellennummer" = "Nummer"))
}

### GW levels
gwl_master <- jsonlite::fromJSON(urls$stations_gwl_meta)
gwl_data <- jsonlite::fromJSON(urls$stations_gwl_data) %>%  
  site_number_to_character() %>% 
  left_join_by_site(gwl_master)

str(gwl_data)
#> 'data.frame':    48806 obs. of  15 variables:
#>  $ Messstellennummer             : chr  "1" "1" "1" "1" ...
#>  $ Datum                         : chr  "1970-01-02" "1970-01-16" "1970-02-02" "1970-02-16" ...
#>  $ Parameter                     : chr  "GW-Stand" "GW-Stand" "GW-Stand" "GW-Stand" ...
#>  $ Einheit                       : chr  "m ü. NHN" "m ü. NHN" "m ü. NHN" "m ü. NHN" ...
#>  $ Messwert                      : num  35.2 35.2 35.2 35.2 35.2 ...
#>  $ Bezirk                        : chr  "Reinickendorf" "Reinickendorf" "Reinickendorf" "Reinickendorf" ...
#>  $ Betreiber                     : chr  "Senat" "Senat" "Senat" "Senat" ...
#>  $ Auspraegung                   : chr  "GW-Stand" "GW-Stand" "GW-Stand" "GW-Stand" ...
#>  $ Grundwasserleiter             : chr  "Hauptgrundwasserleiter (GWL 1.3 + 2)" "Hauptgrundwasserleiter (GWL 1.3 + 2)" "Hauptgrundwasserleiter (GWL 1.3 + 2)" "Hauptgrundwasserleiter (GWL 1.3 + 2)" ...
#>  $ Gelaendeoberkante_GOK_m_ue_NHN: chr  "49.20" "49.20" "49.20" "49.20" ...
#>  $ Rohroberkante_m_ue_NHN        : chr  "49.49" "49.49" "49.49" "49.49" ...
#>  $ Filteroberkante_m_u_GOK       : chr  "37.34" "37.34" "37.34" "37.34" ...
#>  $ Filterunterkante_m_u_GOK      : chr  "39.34" "39.34" "39.34" "39.34" ...
#>  $ Rechtswert_UTM_33_N           : chr  "384416" "384416" "384416" "384416" ...
#>  $ Hochwert_UTM_33_N             : chr  "5831712" "5831712" "5831712" "5831712" ...

### GW quality (all available parameters!)
gwq_master <- jsonlite::fromJSON(urls$stations_gwq_meta)
gwq_data <- jsonlite::fromJSON(urls$stations_gwq_data) %>%  
  site_number_to_character() %>% 
  left_join_by_site(gwq_master)

str(gwq_data)
#> 'data.frame':    713955 obs. of  15 variables:
#>  $ Messstellennummer             : chr  "3" "3" "3" "3" ...
#>  $ Datum                         : chr  "2020-07-01" "2020-07-01" "2020-07-01" "2020-07-01" ...
#>  $ Parameter                     : chr  "Temperatur (Luft)" "pH-Wert (Feld)" "Temperatur (Wasser)" "Leitfähigkeit 25°C vor Ort" ...
#>  $ Einheit                       : chr  "grd Celsius" "ohne Einheit" "grd C" "µS/cm" ...
#>  $ Messwert                      : num  19 7.1 12.2 939 4.91 5 6 0.25 453 0 ...
#>  $ Bezirk                        : chr  "Reinickendorf" "Reinickendorf" "Reinickendorf" "Reinickendorf" ...
#>  $ Betreiber                     : chr  "Senat" "Senat" "Senat" "Senat" ...
#>  $ Auspraegung                   : chr  "GW-Stand + GW-Güte" "GW-Stand + GW-Güte" "GW-Stand + GW-Güte" "GW-Stand + GW-Güte" ...
#>  $ Grundwasserleiter             : chr  "Hauptgrundwasserleiter (GWL 1.3 + 2)" "Hauptgrundwasserleiter (GWL 1.3 + 2)" "Hauptgrundwasserleiter (GWL 1.3 + 2)" "Hauptgrundwasserleiter (GWL 1.3 + 2)" ...
#>  $ Gelaendeoberkante_GOK_m_ue_NHN: chr  "37.87" "37.87" "37.87" "37.87" ...
#>  $ Rohroberkante_m_ue_NHN        : chr  "38.65" "38.65" "38.65" "38.65" ...
#>  $ Filteroberkante_m_u_GOK       : chr  "12.27" "12.27" "12.27" "12.27" ...
#>  $ Filterunterkante_m_u_GOK      : chr  "13.27" "13.27" "13.27" "13.27" ...
#>  $ Rechtswert_UTM_33_N           : chr  "385789" "385789" "385789" "385789" ...
#>  $ Hochwert_UTM_33_N             : chr  "5830677" "5830677" "5830677" "5830677" ...

### Merge GW level and quality into one data frame
gw_data <- dplyr::bind_rows(gwl_data, gwq_data)

str(gw_data)
#> 'data.frame':    762761 obs. of  15 variables:
#>  $ Messstellennummer             : chr  "1" "1" "1" "1" ...
#>  $ Datum                         : chr  "1970-01-02" "1970-01-16" "1970-02-02" "1970-02-16" ...
#>  $ Parameter                     : chr  "GW-Stand" "GW-Stand" "GW-Stand" "GW-Stand" ...
#>  $ Einheit                       : chr  "m ü. NHN" "m ü. NHN" "m ü. NHN" "m ü. NHN" ...
#>  $ Messwert                      : num  35.2 35.2 35.2 35.2 35.2 ...
#>  $ Bezirk                        : chr  "Reinickendorf" "Reinickendorf" "Reinickendorf" "Reinickendorf" ...
#>  $ Betreiber                     : chr  "Senat" "Senat" "Senat" "Senat" ...
#>  $ Auspraegung                   : chr  "GW-Stand" "GW-Stand" "GW-Stand" "GW-Stand" ...
#>  $ Grundwasserleiter             : chr  "Hauptgrundwasserleiter (GWL 1.3 + 2)" "Hauptgrundwasserleiter (GWL 1.3 + 2)" "Hauptgrundwasserleiter (GWL 1.3 + 2)" "Hauptgrundwasserleiter (GWL 1.3 + 2)" ...
#>  $ Gelaendeoberkante_GOK_m_ue_NHN: chr  "49.20" "49.20" "49.20" "49.20" ...
#>  $ Rohroberkante_m_ue_NHN        : chr  "49.49" "49.49" "49.49" "49.49" ...
#>  $ Filteroberkante_m_u_GOK       : chr  "37.34" "37.34" "37.34" "37.34" ...
#>  $ Filterunterkante_m_u_GOK      : chr  "39.34" "39.34" "39.34" "39.34" ...
#>  $ Rechtswert_UTM_33_N           : chr  "384416" "384416" "384416" "384416" ...
#>  $ Hochwert_UTM_33_N             : chr  "5831712" "5831712" "5831712" "5831712" ...
```

## Data Availability

### GW Quality

``` r

# Helper functions to be reused in different data summaries
select_main_columns <- function(data) {
  data %>%
    dplyr::select(dplyr::all_of(c(
      "Messstellennummer",
      "Parameter",
      "Datum",
      "Messwert"
    )))
}

summarise_min_max_n_arrange <- function(data) {
  data %>%
    dplyr::summarise(
      date_min = min(.data$Datum),
      date_max = max(.data$Datum),
      n = dplyr::n(), 
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n))
}
```

``` r

gwq_data_by_parameter <- gwq_data %>%
    select_main_columns() %>%
    dplyr::group_by(.data$Parameter) %>%
    summarise_min_max_n_arrange()

top_filter_data_table(gwq_data_by_parameter)
```

``` r

gwq_data_by_parameter_and_station <- gwq_data %>%
  select_main_columns() %>%
  dplyr::group_by(.data$Parameter, .data$Messstellennummer) %>%
  summarise_min_max_n_arrange()

top_filter_data_table(gwq_data_by_parameter_and_station)
#> Warning in instance$preRenderHook(instance): It seems your data is too big for
#> client-side DataTables. You may consider server-side processing:
#> https://rstudio.github.io/DT/server.html
```

## Export

### GW Quality

``` r

openxlsx::write.xlsx(
  x = list(
  gwq_by_parameter = gwq_data_by_parameter,
  gwq_by_parameter_and_station = gwq_data_by_parameter_and_station,
  gwq_data = gwq_data,
  gwq_master = gwq_master
),
  file = "wasserportal_gwq_data.xlsx",
  overwrite = TRUE
)
```
