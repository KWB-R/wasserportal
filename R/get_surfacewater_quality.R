#' Get Surface Water Quality for One Monitoring Station
#'
#' @param station_id id of surface water measurement station
#'
#' @return data frame with water quality data for one monitoring station
#' @export
#' @importFrom stringr str_detect str_remove
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#' station_id <- stations$overview_list$surface_water.quality$Messstellennummer[1]
#' swq <- wasserportal::get_surfacewater_quality(station_id)
#' str(swq)
#' }
#'
get_surfacewater_quality <- function(station_id) {

  sreihe <- "wa"
  stype <- "opq"
  exportthema <- "pq"
  sdatum <- "01.01.1900"
  senddatum <- date_string_de(Sys.Date())

  url <- paste0(
    wasserportal_base_url(),
    "/station.php?",
    url_parameter_string(
      anzeige = "d", # download
      station = station_id,
      sreihe = sreihe,
      smode = "c", # data format (= csv?)
      thema = stype,
      exportthema = exportthema,
      sdatum = sdatum,
      senddatum = senddatum
    )
  )

  text <- get_text_response_of_httr_request(url, method = "POST")

  # Split the text into separate lines
  textlines <- split_into_lines(text)

  date_pattern <- "Datum"
  start_line <- which(stringr::str_detect(textlines, date_pattern))

  if (length(start_line) == 0L) {
    stop_formatted(
      "Could not find the header row (starting with '%s')",
      date_pattern
    )
  }

  textlines <- textlines[start_line:length(textlines)]

  # Split the header row into fields
  header_fields <- as.character(read(textlines[1L])) %>%
    stringr::str_remove("/Parameter:$")

  # Return empty list with metadata if no data rows are available
  if (length(textlines) == 1L) {
    return(add_wasserportal_metadata(list(), header_fields))
  }

  # Read the data rows
  data <- read(text, header = FALSE, skip = start_line)

  # Get the numbers of the data columns
  if (stype == "opq") {
    stopifnot(ncol(data) == 10L)
  }

  # Name the data columns as given in the first columns of the header row
  names(data) <- header_fields[seq_len(ncol(data))]

  data
}
