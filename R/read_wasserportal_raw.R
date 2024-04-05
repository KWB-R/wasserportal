# read_wasserportal_raw --------------------------------------------------------

#' Read Wasserportal Raw
#'
#' @param variable variable
#' @param station station id
#' @param from_date start date
#' @param type one of "single", "daily", "monthly" (default: "single")
#' @param include_raw_time TRUE or FALSE (default: FALSE)
#' @param handle handle (default: NULL)
#' @param stations_crosstable data frame as returned by
#'   \code{\link{get_stations}(type = "crosstable")}
#' @param api_version 1 integer number representing the version of
#'   wasserportal's API. 1L: before 2023, 2L: since 2023. Default: 2L
#' @return ????
#' @export
#' @importFrom kwb.datetime textToEuropeBerlinPosix
read_wasserportal_raw <- function(
  variable,
  station,
  from_date,
  type = "single",
  include_raw_time = FALSE,
  handle = NULL,
  stations_crosstable,
  api_version = 2L
)
{
  #variable <- variables[1]
  #`%>%` <- magrittr::`%>%`

  stopifnot(length(station) == 1L)
  stopifnot(length(variable) == 1L)
  stopifnot(identical(api_version, 1L) || identical(api_version, 2L))

  from_date <- assert_date(from_date)

  station_ids <- select_columns(stations_crosstable, "Messstellennummer")

  stop_if_not_all_in(station, station_ids)

  variable_ids <- get_station_variables(
    stations_crosstable[station_ids == station, , drop = FALSE]
  )

  stop_if_not_all_in(variable, variable_ids)

  sreihe_options <- if (api_version == 1L) {

    list(
      single = "w",
      single_all = "wa",
      daily = "m",
      monthly = "j"
    )

  } else {

    list(
      single = "ew", # ew = Einzelwerte
      daily = "tw",  # tw = Tageswerte
      monthly = "mw" # mw = Monatswerte
    )
  }

  sreihe <- select_elements(sreihe_options, type)

  # Compose the URL and the body for the request
  if (api_version == 1L) {

    variable_mapping <- list(
      ows = "w",
      odf = "d",
      owt = "t",
      olf = "l",
      oph = "p",
      oog = "o",
      oos = "s"
    )

    variable <- select_elements(variable_mapping, variable)
    variable_ids <- unlist(variable_mapping)

    url <- get_wasserportal_url(station, variable)

    # Compose the body of the request
    body <- list(
      sreihe = sreihe,
      smode = "c",
      sdatum = date_string_de(from_date) # start date
    )

  } else {

    url <- paste0(
      wasserportal_base_url(),
      "/station.php?",
      url_parameter_string(
        anzeige = "d", # = download
        station = station,
        thema = variable, # type of measurement
        sreihe = sreihe, # type of time value
        smode = "c", # output format: csv (?)
        sdatum = date_string_de(from_date) # start date
      )
    )

    body <- list()
  }

  text <- cat_and_run(
    get_wasserportal_text(
      station,
      variable,
      station_ids,
      variable_ids = variable
    ),
    expr = get_text_response_of_httr_post_request(
      url,
      body = body,
      handle = handle
    )
  )

  if (text == "") {
    message("Wasserportal returned an empty string. Returning NULL.")
    return(NULL)
  }

  # Split the text into separate lines
  textlines <- split_into_lines(text)

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

# get_wasserportal_url ---------------------------------------------------------
get_wasserportal_url <- function(station, variable)
{
  url_base <- sprintf("%s/station.php", wasserportal_base_url())

  sprintf("%s?sstation=%s&anzeige=%sd", url_base, station, variable)
}

# get_wasserportal_text --------------------------------------------------------
get_wasserportal_text <- function(station, variable, station_ids, variable_ids)
{
  default_names <- function(ids, prefix) {
    default_if_null(names(ids), paste0(prefix, ids))
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

# add_wasserportal_metadata ----------------------------------------------------
add_wasserportal_metadata <- function(x, header_fields)
{
  structure(x, metadata = header_fields[-(1:2)])
}

# clean_timestamp_columns ------------------------------------------------------
clean_timestamp_columns <- function(data, include_raw_time)
{
  raw_timestamps <- select_columns(data, "Datum")

  data <- rename_columns(data, list(Datum = "timestamp_raw"))

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

  data <- move_columns_to_front(data, keys)

  if (! include_raw_time) {
    data <- remove_columns(data, keys[1:2])
  }

  remove_timestep_outliers(data, data$LocalDateTime, 60 * 15)
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

  first_indices <- sapply(index_pairs, first_element)

  if (dbg && ! all(is_expected <- grepl(" 03", timestamps[first_indices]))) {

    message(
      "There are unexpected duplicated timestamps: ",
      string_list(timestamps[first_indices][! is_expected])
    )
  }

  timestamps_old <- timestamps

  timestamps[first_indices] <- gsub(" 03", " 02", timestamps[first_indices])

  indices <- sort(unlist(index_pairs))

  print_if(dbg, caption = "After timestamp repair", data.frame(
    row = indices,
    old = timestamps_old[indices],
    new = timestamps[indices]
  ))

  timestamps
}

# remove_remaining_duplicates --------------------------------------------------
remove_remaining_duplicates <- function(data)
{
  timestamps <- select_columns(data, "timestamp_corr")

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
