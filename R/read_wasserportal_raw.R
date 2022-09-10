#' Helper function: get available station variables
#'
#' @param station_df station_df
#'
#' @return returns names of available variables for station
#' @export
#'
#' @importFrom dplyr select_if
#'
get_station_variables <- function(station_df)
{
  station_df %>%
    dplyr::select_if(function(x){!all(is.na(x))}) %>%
    names() %>%
    setdiff(c("Messstellennummer", "Messstellenname"))
}

# read_wasserportal_raw --------------------------------------------------------

#' Read Wasserportal Raw
#'
#' @param variable variable
#' @param station station id
#' @param from_date start date
#' @param type one of "single", "daily", "monthly" (default: "single")
#' @param include_raw_time TRUE or FALSE (default: FALSE)
#' @param handle handle (default: NULL)
#' @param stations_crosstable sublist `crosstable` as retrieved from \code{\link{get_stations}}
#' i.e. `get_stations()$crosstable`
#' @return ????
#' @export
#' @importFrom kwb.utils catAndRun selectColumns selectElements
#' @importFrom kwb.datetime textToEuropeBerlinPosix
#' @importFrom httr content POST
read_wasserportal_raw <- function(
  variable,
  station,
  from_date,
  type = "single",
  include_raw_time = FALSE,
  handle = NULL,
  stations_crosstable
)
{
  #variable <- variables[1]

  stopifnot(length(station) == 1L)
  stopifnot(length(variable) == 1L)

  from_date <- assert_date(from_date)

  station_ids <- kwb.utils::selectColumns(
    stations_crosstable,
    "Messstellennummer"
  )

  stopifnot(station %in% station_ids)

  station_df <- stations_crosstable[station_ids == station, , drop = FALSE] %>%
    dplyr::select_if(function(x){!all(is.na(x))})

  variable_ids <- get_station_variables(station_df)

  stopifnot(variable %in% variable_ids)

  sreihe <- kwb.utils::selectElements(elements = type, list(
    single = "w",
    single_all = "wa",
    daily = "m",
    monthly = "j"
  ))

  variable_mapping <- list(
    ws = "w",
    df = "d",
    wt = "t",
    lf = "l",
    ph = "p",
    og = "o",
    os = "s"
  )

  variable <- kwb.utils::selectElements(variable_mapping, variable)
  variable_ids <- unlist(variable_mapping)

  # Compose the body of the request
  body <- list(
    sreihe = sreihe,
    smode = "c",
    sdatum = date_string_de(from_date) # start date
  )

  # Post the request to the web server
  response <- kwb.utils::catAndRun(
    get_wasserportal_text(station, variable, station_ids, variable_ids),
    httr::POST(
      url = get_wasserportal_url(station, variable),
      body = body,
      handle = handle
    )
  )

  if (httr::http_error(response)) {
    message("POST request failed. Returning the response object.")
    return(response)
  }

  # Read the response of the web server as text
  text <- httr::content(response, as = "text", encoding = "Latin1")

  # Split the text into separate lines
  textlines <- strsplit(text, "\n")[[1L]]

  # Split the header row into fields
  header_fields <- as.character(read(textlines[1L]))

  # Return empty list with metadata if no data rows are available
  if (length(textlines) == 1L) {
    return(add_wasserportal_metadata(list(), header_fields))
  }

  # Read the data rows
  data <- read(text, header = FALSE, skip = 1L)

  # Get the numbers of the data columns
  if (!type %in% c("daily", "monthly")) {
    stopifnot(ncol(data) == 2L)
  }

  # Name the data columns as given in the first columns of the header row
  names(data) <- header_fields[seq_len(ncol(data))]

  if (type == "single") {
    data <- clean_timestamp_columns(data, include_raw_time)
  }

  # Return the data frame with the additional fields of the header row as
  # meta information in attribute "metadata"
  add_wasserportal_metadata(data, header_fields)
}

# clean_timestamp_columns ------------------------------------------------------

clean_timestamp_columns <- function(data, include_raw_time)
{
  raw_timestamps <- kwb.utils::selectColumns(data, "Datum")

  data <- kwb.utils::renameColumns(data, list(Datum = "timestamp_raw"))

  data$timestamp_corr <- repair_wasserportal_timestamps(raw_timestamps)

  data <- remove_remaining_duplicates(data)

  data$LocalDateTime <- kwb.datetime::textToEuropeBerlinPosix(
    data$timestamp_corr,
    format = "%d.%m.%Y %H:%M",
    switches = FALSE,
    dbg = FALSE
  )

  stopifnot(! any(duplicated(data$LocalDateTime)))

  keys <- c("timestamp_raw", "timestamp_corr", "LocalDateTime")

  data <- kwb.utils::moveColumnsToFront(data, keys)

  if (! include_raw_time) {
    data <- kwb.utils::removeColumns(data, keys[1:2])
  }

  remove_timestep_outliers(data, data$LocalDateTime, 60 * 15)
}

# add_wasserportal_metadata ----------------------------------------------------
add_wasserportal_metadata <- function(x, header_fields)
{
  structure(x, metadata = header_fields[-(1:2)])
}

# get_wasserportal_text --------------------------------------------------------
get_wasserportal_text <- function(station, variable, station_ids, variable_ids)
{
  default_names <- function(ids, prefix) {
    kwb.utils::defaultIfNULL(names(ids), paste0(prefix, ids))
  }

  variable_names <- default_names(variable_ids, "variable_")
  station_names <- default_names(station_ids, "station_")

  sprintf(
    "Reading '%s' for station %s (%s)",
    variable_names[match(variable, unlist(variable_ids))],
    station,
    station_names[match(station, unlist(station_ids))]
  )
}

# get_wasserportal_url ---------------------------------------------------------
get_wasserportal_url <- function(station, variable)
{
  url_base <- sprintf("%s/station.php", wasserportal_base_url())

  sprintf("%s?sstation=%s&anzeige=%sd", url_base, station, variable)
}

# repair_wasserportal_timestamps -----------------------------------------------
repair_wasserportal_timestamps <- function(timestamps, dbg = FALSE)
{
  duplicates <- timestamps[duplicated(timestamps)]

  index_pairs <- lapply(duplicates, function(x) which(timestamps == x))

  if (length(index_pairs) == 0L) {
    return(timestamps)
  }

  stopifnot(all(lengths(index_pairs) == 2L))

  first_indices <- sapply(index_pairs, kwb.utils::firstElement)

  if (dbg && ! all(is_expected <- grepl(" 03", timestamps[first_indices]))) {

    message(
      "There are unexpected duplicated timestamps: ",
      kwb.utils::stringList(timestamps[first_indices][! is_expected])
    )
  }

  timestamps_old <- timestamps

  timestamps[first_indices] <- gsub(" 03", " 02", timestamps[first_indices])

  indices <- sort(unlist(index_pairs))

  kwb.utils::printIf(dbg, caption = "After timestamp repair", data.frame(
    row = indices,
    old = timestamps_old[indices],
    new = timestamps[indices]
  ))

  timestamps
}

# remove_remaining_duplicates --------------------------------------------------
remove_remaining_duplicates <- function(data)
{
  timestamps <- kwb.utils::selectColumns(data, "timestamp_corr")

  is_duplicate <- duplicated(timestamps)

  if (any(is_duplicate)) {

    message("Removing rows with unexpected duplicated timestamps:")
    print(data[is_duplicate, ])
  }

  data[! is_duplicate, ]
}

# remove_timestep_outliers -----------------------------------------------------
remove_timestep_outliers <- function(data, timestamps, timestep = 15L * 60L)
{
  stopifnot(inherits(timestamps, "POSIXct"))
  stopifnot(nrow(data) == length(timestamps))

  is_outlier <- as.numeric(timestamps) %% timestep != 0L

  if (! any(is_outlier)) {
    return(data)
  }

  message("Removing rows with 'non-timestep-multiple' timestamps:")
  print(data[is_outlier, ])

  data[! is_outlier, ]
}
