# Tutorial

## Install the Package

Use the “remotes” package to install the package “wasserportal” directly
from KWB’s GitHub site:

``` r

# install.packages("remotes")
remotes::install_github("kwb-r/wasserportal", upgrade = "never", force = TRUE)
```

## Overview on Monitoring Stations and Parameters

Get information on monitoring stations and parameters that are available
on the Wasserportal:

``` r

stations <- wasserportal::get_stations(type = c("list", "crosstable"))
#> Importing 10 station overviews from Wasserportal Berlin ... ok. (8.02 secs)
str(stations, 2)
#> List of 2
#>  $ overview_list:List of 10
#>   ..$ surface_water.water_level         : tibble [113 × 10] (S3: tbl_df/tbl/data.frame)
#>   ..$ surface_water.flow                : tibble [36 × 10] (S3: tbl_df/tbl/data.frame)
#>   ..$ surface_water.temperature         : tibble [71 × 10] (S3: tbl_df/tbl/data.frame)
#>   ..$ surface_water.conductivity        : tibble [18 × 10] (S3: tbl_df/tbl/data.frame)
#>   ..$ surface_water.ph                  : tibble [18 × 9] (S3: tbl_df/tbl/data.frame)
#>   ..$ surface_water.oxygen_concentration: tibble [18 × 10] (S3: tbl_df/tbl/data.frame)
#>   ..$ surface_water.oxygen_saturation   : tibble [18 × 10] (S3: tbl_df/tbl/data.frame)
#>   ..$ surface_water.quality             : tibble [56 × 10] (S3: tbl_df/tbl/data.frame)
#>   ..$ groundwater.level                 : tibble [891 × 11] (S3: tbl_df/tbl/data.frame)
#>   ..$ groundwater.quality               : tibble [208 × 10] (S3: tbl_df/tbl/data.frame)
#>  $ crosstable   : tibble [1,114 × 12] (S3: tbl_df/tbl/data.frame)
```

The data frame `stations$crosstable` informs about the parameters that
are measured at the different monitoring stations:

The parameter abbreviations that appear as column names in the above
table have the following meanings:

``` r

parameters <- wasserportal::get_overview_options()
str(parameters)
#> List of 2
#>  $ surface_water:List of 8
#>   ..$ water_level         : chr "ows"
#>   ..$ flow                : chr "odf"
#>   ..$ temperature         : chr "owt"
#>   ..$ conductivity        : chr "olf"
#>   ..$ ph                  : chr "oph"
#>   ..$ oxygen_concentration: chr "oog"
#>   ..$ oxygen_saturation   : chr "oos"
#>   ..$ quality             : chr "opq"
#>  $ groundwater  :List of 2
#>   ..$ level  : chr "gws"
#>   ..$ quality: chr "gwq"
```

The table of data availabilty for each monitoring station is also
available in JSON format here:
<https://kwb-r.github.io/wasserportal/stations_crosstable.json>

## Provide Pipe Operator and Helper Functions

The code provided in the following requires the pipe operator `%>%` of
the “magrittr” package and some helper functions to be defined:

``` r

`%>%` <- magrittr::`%>%`

comma_separated <- function(x) {
  paste(x, collapse = ", ")
}
  
to_plotly_title <- function(x) {
  key_values <- paste(names(x), unname(unlist(x)), sep = ": ")
  list(text = sprintf(
    "%s<br><sup>%s</sup>", 
    key_values[1L], 
    comma_separated(key_values[-1L])
  ))
}

ggplot2_date_value <- function(data, col) {
  ggplot2::ggplot(data, mapping = ggplot2::aes(
    x = Datum, 
    y = Messwert, 
    col = col
  ))
}
```

## Groundwater Level Data

The data frame `stations$overview_list$groundwater.level` gives general
information on the groundwater monitoring stations:

### Master data

More information on the groundwater level stations (master data), such
as the coordinates of the wells, can be found if you follow the web link
(URL) that is given in column `stammdaten_link` of the above table. The
“wasserportal” package provides a function to retrieve information from
these links:

``` r

urls <- stations$overview_list$groundwater.level$stammdaten_link
stations_gwl_master <- wasserportal::get_wasserportal_masters_data(urls)
#> Importing master data for 891 stations from Wasserportal Berlin ... ok. (8.12 mins)
```

This is how the resulting table `stations_gwl_master` looks like:

The master data of groundwater level stations is also available in JSON
format here:
<https://kwb-r.github.io/wasserportal/stations_gwl_master.json>

### Trend Classification

Groundwater level trend classification (provided by SenWeb) is
visualized below.

#### 1. Trend Classification Histogram

``` r

gwl <- stations$overview_list$groundwater.level %>% 
  dplyr::mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"))

text_low_levels <- c("extrem niedrig", "sehr niedrig", "niedrig")
text_high_levels <- c("hoch", "sehr hoch", "extrem hoch")
levels_ordered <- c(text_low_levels, "normal", text_high_levels, "keine")

gwl$Klassifikation <- forcats::fct_relevel(gwl$Klassifikation, levels_ordered)

gwl_classified_only <- gwl %>% 
  dplyr::filter(Klassifikation != "keine")

percental_share_low_levels <- rounded_percentage(
  sum(gwl_classified_only$Klassifikation %in% text_low_levels), 
  basis = nrow(gwl_classified_only)
)

percental_share_high_levels <- rounded_percentage(
  sum(gwl_classified_only$Klassifikation %in% text_high_levels), 
  basis = nrow(gwl_classified_only)
)

title_text <- sprintf(
  "GW level classification (n = %d out of %d have 'classification' data)",
  nrow(gwl_classified_only), 
  nrow(gwl)
)

g1 <- gwl_classified_only %>% 
  dplyr::count(Klassifikation, Grundwasserspannung) %>% 
  dplyr::mutate(percental_share = kwb.utils::percentage(n, nrow(gwl))) %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = Klassifikation,
    y = percental_share,
    fill = Grundwasserspannung
  )) +
  ggplot2::geom_bar(stat = "identity") + 
  ggplot2::labs(
    title = title_text,
    x = "Classification",
    y = "Percental share (%)"
  ) +
  ggplot2::theme_bw()

plotly::ggplotly(g1)
```

65.47 percent of all considered 805 groundwater level monitoring
stations containing `classification` data (out of 891 provided by
SenWeb) indicate `below normal` (extrem niedrig, sehr niedrig, niedrig)
groundwater levels. However, only 65.47 percent are indicate
`above normal` (hoch, sehr hoch, extrem hoch) groundwater levels.

#### 2. Trend Classification Map

``` r

level_colors <- data.frame(
  Klassifikation = levels_ordered, 
  classi_color = c(
    "darkred", 
    "red", 
    "orange", 
    "green", 
    "lightblue", 
    "blue", 
    "darkblue", 
    "grey"
  )
)

rechtswert <- "Rechtswert_UTM_33_N"
hochwert <- "Hochwert_UTM_33_N"

gwl_classified_only_with_coords <- gwl_classified_only %>% 
  dplyr::mutate(
    Messstellennummer = as.character(Messstellennummer),
  ) %>% 
  dplyr::inner_join(
    stations_gwl_master %>%
      tibble::as_tibble() %>% 
      dplyr::select(dplyr::all_of(c("Nummer", rechtswert, hochwert))) %>% 
      dplyr::rename(Messstellennummer = "Nummer"),
    by = "Messstellennummer"
  ) %>% 
  dplyr::left_join(
    level_colors, 
    by = "Klassifikation"
  ) %>% 
  sf::st_as_sf(
    coords = c(rechtswert, hochwert), 
    crs = 25833
  ) %>% 
  sf::st_transform(crs = 4326)

if(nrow(gwl_classified_only_with_coords) > 0) {

# Create a vector of labels for each row in gwl_classified_only_with_coords
labs <- wasserportal::columns_to_labels(
  data = gwl_classified_only_with_coords, 
  columns = c(
    "Messstellennummer", 
    "Grundwasserspannung", 
    "Klassifikation", 
    "Datum"
  ),
  fmt = "<p>%s: %s</p>",
  sep = ""
)

# Print Map
gwlmap <- gwl_classified_only_with_coords %>% 
  leaflet::leaflet() %>%
  leaflet::addTiles() %>% 
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addCircles(
    color = ~classi_color,
    label = lapply(labs, htmltools::HTML)
  ) %>% 
  leaflet::addLegend(
    position = "topright",
    colors = level_colors$classi_color,
    labels = level_colors$Klassifikation,
    title = sprintf(
      "Classification (latest data: %s)",
      max(gwl_classified_only_with_coords$Datum)
    )
  )

htmlwidgets::saveWidget(
  gwlmap, 
  "./map_gwl-trend.html", 
  title = "GW level trend"
)

gwlmap
}
```

GW level trend plot is also available on a full html page here:
[https://kwb-r.github.io/wasserportal/map_gwl-trend.html](https://kwb-r.github.io/wasserportal/map_gwl-trend.md)

### Groundwater Levels: One Station

The following code downloads and plots groundwater level data for one
monitoring station:

``` r

station_gwl <- stations$overview_list$groundwater.level[1L, ]

gw_level <- wasserportal::read_wasserportal_raw_gw(
  station = station_gwl$Messstellennummer, 
  stype = "gws"
  #, as_text = TRUE, dbg = TRUE
) %>% 
  dplyr::mutate(Label = sprintf("%s (%s)", Parameter, Einheit))

head(gw_level)

g <- gw_level %>% 
  ggplot2_date_value(col = "Label") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly::ggplotly(g) %>%
  plotly::layout(title = to_plotly_title(station_gwl))
```

### Groundwater Levels: Multiple Stations

The following code downloads and plots groundwater level data for
multiple monitoring stations:

``` r

gw_level_multi <- data.table::rbindlist(lapply(
  stations$overview_list$groundwater.level$Messstellennummer, 
  function(id) { 
    kwb.utils::catAndRun(
      sprintf("Downloading Messstellennummer == '%s'", id), 
      wasserportal::read_wasserportal_raw_gw(station = id, stype = "gws"), 
      dbg = FALSE
    )
  }
))

readr::write_csv(gw_level_multi, file = "groundwater_level.csv")

# Plot 10 GW level
selected_stations <- stations$overview_list$groundwater.level$Messstellennummer[1:10]

g <- gw_level_multi %>% 
  dplyr::filter(Messstellennummer %in% selected_stations) %>% 
  dplyr::mutate(Messstellennummer = as.character(Messstellennummer)) %>% 
  ggplot2_date_value(col = "Messstellennummer") +
  ggplot2::labs(title = "GW level (m above NN)") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly::ggplotly(g)
```

The data of all GW level stations is also available in CSV format here:
<https://kwb-r.github.io/wasserportal/groundwater_level.csv>

## Groundwater Quality Data

Overview data of GW level stations can be requested as shown below:

``` r

stations_gwq <- wasserportal::get_wasserportal_stations_table(
  type = parameters$groundwater$quality
)
```

Master data of groundwater quality stations can be requested as shown
below:

``` r

stations_gwq_master <- wasserportal::get_wasserportal_masters_data(
  master_urls = stations_gwq$stammdaten_link
)
#> Importing master data for 208 stations from Wasserportal Berlin ... ok. (1.99 mins)
```

The master data of groundwater quality stations is also available in
JSON format here:
<https://kwb-r.github.io/wasserportal/stations_gwq_master.json>

### Groundwater Quality: One Station

The following code downloads and plots groundwater quality data for one
monitoring station:

``` r

station_gwq <- stations$overview_list$groundwater.quality[1L, ]

gw_quality <- wasserportal::read_wasserportal_raw_gw(
  station = station_gwq$Messstellennummer, 
  stype = "gwq"
)

head(gw_quality)

unique(gw_quality$Parameter)

g <- gw_quality %>%  
  dplyr::filter(Parameter == "Sulfat") %>% 
  ggplot2_date_value(col = "Parameter") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly::ggplotly(g) %>%
  plotly::layout(title = to_plotly_title(station_gwq))
```

### Groundwater Quality: Multiple Stations

The following code downloads and plots groundwater quality data for
multiple monitoring stations:

``` r

gw_quality_multi <- data.table::rbindlist(lapply(
  stations$overview_list$groundwater.quality$Messstellennummer, 
  function(id) kwb.utils::catAndRun(
    sprintf("Downloading Messstellennummer == '%s'", id), 
    wasserportal::read_wasserportal_raw_gw(station = id, stype = "gwq"), 
    dbg = FALSE
  )
))

readr::write_csv(gw_quality_multi, "groundwater_quality.csv")

# Plot 10 GW quality 
selected_stations <- stations$overview_list$groundwater.quality$Messstellennummer[1:10]

g <- gw_quality_multi %>% 
  dplyr::filter(Messstellennummer %in% selected_stations) %>% 
  dplyr::mutate(Messstellennummer = as.character(Messstellennummer)) %>% 
  dplyr::filter(Parameter == "Sulfat") %>% 
  ggplot2_date_value(col = "Messstellennummer") +
  ggplot2::labs(title = "GW quality (Sulfat)") +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly::ggplotly(g)
```

The data of all GW quality stations is also available in CSV format
here: <https://kwb-r.github.io/wasserportal/groundwater_quality.csv>
