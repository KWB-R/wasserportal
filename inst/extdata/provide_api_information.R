#
# - Download the API documentation file (pdf) and convert it to text
# - Extract information from the text, e.g. how are the URLs composed
# - Source the whole script first, then manually go through the MAIN section
#

# MAIN: Download pdf, convert to text, show pages as text ----------------------
if (FALSE)
{
  # Define URL to API description file
  api_pdf <- paste0(
    "https://wasserportal.berlin.de/download/",
    "wasserportal_berlin_getting_data.pdf"
  )

  # Convert pdf to text
  api_text <- pdftools::pdf_text(api_pdf)

  # Print one page of the pdf file to the screen (with empty lines removed)
  show_page <- function(i) {
    text <- api_text[[i]]
    stopifnot(length(text) == 1L)
    strsplit(text, "\n")[[1L]] %>%
      kwb.utils::removeEmpty() %>%
      writeLines()
  }

  # Show page 2 (table of contents)
  show_page(2)
}

# MAIN: Show tables that have been extracted from the documentation ------------
if (FALSE)
{
  # The tables already have been added to the wasserportal package
  api_tables <- wasserportal::get_api_tables()

  # Show these tables after renaming
  api_tables$global_surface_water
  api_tables$global_groundwater

  api_tables$surface_soil_water
  api_tables$groundwater

  head(api_tables$substances_groundwater)
  head(api_tables$substances_sampling)
}

# MAIN: Compose URLs -----------------------------------------------------------
if (FALSE)
{
  # 1. Wasserportal Berlin (WPB)
  # The WPB serves raw data from surface waters, groundwater stations and soil
  # moisture stations of Berlin.

  station_types <- c("surface_water", "groundwater", "soil_water")

  # 1.1. Base URL
  url_1_1 <- create_url_overview()

  identical(
    url_1_1,
    "https://wasserportal.berlin.de/start.php"
  )

  # 2. Differences between Data Download from Surface Waters, Soil Waters and
  # Groundwater

  # There are tiny differences in offering data for water level, or quality
  # parameter topics. The main download format for data are .csv files. For the
  # surface water topic it is also possible in a XML notation as WaterML.

  # 2.1 Overview Surface Waters – All Stations
  url_2_1 <- create_parameters_overview(
    anzeige = "tabelle_ow",
    messanzeige = "ms_ow_berlin"
  ) %>%
    create_url_overview()

  identical(
    url_2_1,
    "https://wasserportal.berlin.de/start.php?anzeige=tabelle_ow&messanzeige=ms_ow_berlin"
  )

  # 2.2 Overview Soil Waters – All Stations
  url_2_2 <- create_parameters_overview(
    anzeige = "tabelle_bw",
    messanzeige = "ms_bw_berlin"
  ) %>%
    create_url_overview()

  identical(
    url_2_2,
    "https://wasserportal.berlin.de/start.php?anzeige=tabelle_bw&messanzeige=ms_bw_berlin"
  )

  # 2.3 Overview Groundwater – All Stations
  url_2_3 <- create_parameters_overview(
    anzeige = "tabelle_gw",
    messanzeige = "ms_gw_berlin"
  ) %>%
    create_url_overview()

  identical(
    url_2_3,
    "https://wasserportal.berlin.de/start.php?anzeige=tabelle_gw&messanzeige=ms_gw_berlin"
  )

  # 3. Structure of Queries – Surface waters and soil parameters

  # HTTPS method: GET
  # anzeige = d             Output (g = graphic, d= download)
  # thema=ows               Type of measurement (Temperature etc.)
  # station=5865900         Station ID
  # sreihe=ew               type of time values
  # smode=c                 Output format (only when anzeige = d)
  # sdatum=20.12.2020       Date and time (from when)

  url_3 <- create_parameters_surface_soil_water(
    anzeige = "d",
    station = "5865900",
    thema = "ows",
    sreihe = "ew",
    smode = "c",
    sdatum = "20.12.2022"
  ) %>%
    create_url_station()

  identical(
    url_3,
    "https://wasserportal.berlin.de/station.php?anzeige=d&station=5865900&thema=ows&sreihe=ew&smode=c&sdatum=20.12.2022"
  )

  # 3.1 Examples

  # a. Water levels in cm as daily values for station 5865900 (Allee der
  # Kosmonauten) starting from 21. Dec. 2020 as .csv file

  url_3_1_a <- create_parameters_surface_soil_water(
    anzeige = "d",
    station = "5865900",
    thema = "ows",
    sreihe = "tw",
    smode = "c",
    sdatum = "21.12.2020"
  ) %>%
    create_url_station()

  identical(
    url_3_1_a,
    "https://wasserportal.berlin.de/station.php?anzeige=d&station=5865900&thema=ows&sreihe=tw&smode=c&sdatum=21.12.2020"
  )

  # b. Monthly Mean levels starting from 21.12.2013 (including monthly minimum
  # and maximum values)

  url_3_1_b <- create_parameters_surface_soil_water(
    anzeige = "d",
    station = "5865900",
    thema = "ows",
    sreihe = "mw",
    smode = "c",
    sdatum = "21.12.2013"
  ) %>%
    create_url_station()

  identical(
    url_3_1_b,
    "https://wasserportal.berlin.de/station.php?anzeige=d&station=5865900&thema=ows&sreihe=mw&smode=c&sdatum=21.12.2013"
  )

  # 6 Structure of Queries – Groundwater and Probenahme

  # Groundwater stations are much more stations than surface water stations. The
  # data comes only as .csv files. However, users must always select a time
  # period here.

  # anzeige=d                         d=Download, g=graphische Ausgabe
  # station=5149                      station number
  # sreihe=ew                         Type of time values
  # smode=c                           data format (only when anzeige = d)
  # thema=gws                         topic -> here groundwater (gws)
  # exportthema                       gw = Grundwasser or pq=Probenahme (only when anzeige = d)
  # sdatum=09.01.2014                 start date
  # senddatum=09.01.2020              end date

  url_6 <- create_parameters_groundwater(
    anzeige = "d",
    station = "5149",
    sreihe = "ew",
    smode = "c",
    thema = "gws",
    exportthema = "gw",
    sdatum = "09.01.2014",
    senddatum = "09.01.2020"
  ) %>%
    create_url_station()

  identical(
    url_6,
    "https://wasserportal.berlin.de/station.php?anzeige=d&station=5149&sreihe=ew&smode=c&thema=gws&exportthema=gw&sdatum=09.01.2014&senddatum=09.01.2020"
  )

  # 6.1 Examples

  # a. Groundwater levels for station no. 5149 as csv file for the time range
  # from 09.01.2014 until 09.01.2020.

  url_6_1_a <- create_parameters_groundwater(
    anzeige = "d",
    station = "5149",
    smode = "c",
    thema = "gws",
    exportthema = "gw",
    sreihe = "ew",
    sdatum = "09.01.2014",
    senddatum = "09.01.2020"
  ) %>%
    create_url_station()

  identical(
    order_parameters(url_6_1_a),
    order_parameters("https://wasserportal.berlin.de/station.php?anzeige=d&station=5149&smode=c&thema=gws&exportthema=gw&sreihe=ew&sdatum=09.01.2014&senddatum=09.01.2020")
  )

  # b. Groundwater temperature for station no. 15156 as single values from
  # 01.2014 until 01.2020 as csv.
  url_6_1_b <- create_parameters_groundwater(
    anzeige = "d",
    station = "15156",
    smode = "c",
    thema = "gwq",
    exportthema = "gw",
    sreihe = "ew",
    nstoffid = c(10L, 0L),
    sdatum = "09.01.2014",
    senddatum = "09.01.2020"
  ) %>%
    create_url_station()

  identical(
    order_parameters(url_6_1_b),
    order_parameters("https://wasserportal.berlin.de/station.php?anzeige=d&station=15156&smode=c&thema=gwq&exportthema=gw&nstoffid=10&nstoffid2=0&sreihe=ew&sdatum=09.01.2014&senddatum=09.01.2020")
  )

  # c. Combination of Temperatures -> groundwater and air - for station no.
  # 15156 as single values, same time range, as csv.

  url_6_1_c <- create_parameters_groundwater(
    anzeige = "d",
    station = "15156",
    smode = "c",
    thema = "gwq",
    exportthema = "gw",
    nstoffid = c(10L, 2L),
    sreihe = "ew",
    sdatum = "09.01.2014",
    senddatum = "09.01.2020"
  ) %>%
    create_url_station()

  identical(
    order_parameters(url_6_1_c),
    order_parameters("https://wasserportal.berlin.de/station.php?anzeige=d&station=15156&smode=c&thema=gwq&exportthema=gw&nstoffid=10&nstoffid2=2&sreihe=ew&sdatum=09.01.2014&senddatum=09.01.2020")
  )
}

# MAIN: Download text from URLs ------------------------------------------------
if (FALSE)
{
  example_urls <- lapply(stats::setNames(nm = ls(pattern = "^url_")), get)

  text_contents <- lapply(example_urls, function(url) try(download(url)))

  text_contents[!sapply(text_contents, kwb.utils::isTryError)]
}

`%>%` <- magrittr::`%>%`

# set_class --------------------------------------------------------------------
set_class <- function(x, class)
{
  structure(x, class = class)
}

# create_parameters_overview ---------------------------------------------------
create_parameters_overview <- function(
    anzeige = "(tabelle_ow|tabelle_bw|tabelle_gw)",
    messanzeige = "(ms_ow_berlin|ms_bw_berlin|ms_gw_berlin)"
)
{
  parameters <- list(
    anzeige = anzeige,
    messanzeige = messanzeige
  )

  set_class(parameters, c(
    "parameters_overview",
    "parameters"
  ))
}

# create_parameters_surface_soil_water -----------------------------------------
create_parameters_surface_soil_water <- function(
    anzeige = "(d|g)",
    station = "<station>",
    thema = "(ows)",
    sreihe = "(ew)",
    smode = "(c)",
    sdatum = "<dd.mm.yyyy>"
)
{
  parameters <- list(
    anzeige = anzeige,
    station = station,
    thema = thema,
    sreihe = sreihe,
    smode = smode,
    sdatum = sdatum
  )

  set_class(parameters, c(
    "parameters_surface_soil_water",
    "parameters"
  ))
}

# create_parameters_groundwater ------------------------------------------------
create_parameters_groundwater <- function(
    anzeige = "(d|g)",
    station = "<station>",
    sreihe = "ew",
    smode = "c", # data format (only when anzeige = d)
    thema = "gws", # topic -> here groundwater (gws)
    exportthema = "(gw|pq)", # (only when anzeige = d)
    nstoffid = integer(),
    sdatum = "09.01.2014", # start date
    senddatum = "09.01.2020" # end date
)
{
  if (!identical(anzeige, "d")) {
    stop_formatted("Only anzeige = 'd' (display) supported!")
  }

  parameters <- list(
    anzeige = anzeige,
    station = station,
    sreihe = sreihe,
    smode = smode,
    thema = thema,
    exportthema = exportthema,
    sdatum = sdatum,
    senddatum = senddatum
  )

  # Expand the nstoffid argument and add it to the list of parameters
  n_subst <- length(nstoffid)

  if (n_subst > 0L) {
    nm <- paste0("nstoffid", c("", as.character(seq_len(n_subst - 1L) + 1L)))
    parameters <- c(parameters, as.list(stats::setNames(nstoffid, nm)))
  }

  set_class(parameters, c(
    "parameters_groundwater",
    "parameters"
  ))
}

# create_url_creator -----------------------------------------------------------
create_url_creator <- function(endpoint)
{
  function(parameters = list()) {

    has_parameters <- length(parameters) > 0L
    stopifnot(!has_parameters || inherits(parameters, "parameters"))

    url <- sprintf(
      "%s/%s%s%s",
      wasserportal::wasserportal_base_url(),
      endpoint,
      ifelse(has_parameters, "?", ""),
      do.call(wasserportal:::url_parameter_string, parameters)
    )

    structure(url, class = gsub("parameters", "url", class(parameters)[1L]))
  }
}

# create_url_overview ----------------------------------------------------------
create_url_overview <- create_url_creator(endpoint = "start.php")

# create_url_station -----------------------------------------------------------
create_url_station <- create_url_creator(endpoint = "station.php")

# order_parameters -------------------------------------------------------------
order_parameters <- function(x)
{
  parts <- strsplit(x, "[?]")[[1L]]
  key_values <- sort(strsplit(parts[2L], "&")[[1L]])
  paste0(parts[1L], "?", paste(key_values, collapse = "&"))
}

# http_get_as_textlines --------------------------------------------------------
http_get_as_textlines <- function(url)
{
  wasserportal:::get_text_response_of_httr_request(url, "GET", dbg = TRUE) %>%
    wasserportal:::split_into_lines()
}

# download ---------------------------------------------------------------------
download <- function(url, ...)
{
  UseMethod("download")
}

# download.url_overview --------------------------------------------------------
download.url_overview <- function(url, ...)
{
  message("download of overview...")

  http_get_as_textlines(url) %>%
    head(10L)
}

# download.url_groundwater -----------------------------------------------------
download.url_groundwater <- function(url, ...)
{
  message("download of groundwater data...")

  http_get_as_textlines(url) %>%
    head(10L)
}

# download.url_surface_soil_water ----------------------------------------------
download.url_surface_soil_water <- function(url, ...)
{
  message("download of surface water or soil water data...")

  http_get_as_textlines(url) %>%
    head(10L)
}

# download.default -------------------------------------------------------------
download.default <- function(url, ...)
{
  stop("No download function implemented for this type of url: ", url)
}
