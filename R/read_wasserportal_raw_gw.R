
#' read_wasserportal_raw_gw
#'
#' @param station station id
#' @param stype "gwl" or "gwq"
#' @param type "single" or "single_all" (if stype = "gwq")
#' @param from_date (default: "")
#' @param include_raw_time default: FALSE
#' @param handle default: NULL
#'
#' @return data.frame with values (currently only if stype == "gwl")
#' @export
#' @importFrom stringr str_remove str_extract
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select filter mutate
#' @examples
#' \dontrun{
#' read_wasserportal_raw_gw(station = 149, stype = "gwl")
#' read_wasserportal_raw_gw(station = 149, stype = "gwq")
#' }
read_wasserportal_raw_gw <- function(
    station = 149,
    stype = "gwl",
    type = "single_all",
    from_date = "",
    include_raw_time = FALSE,
    handle = NULL
)
{
  if (stype == "gwq") {
    type <- "single_all"
  }

  stype_options <- list(
    "gwl" = list(
      download_shortcut = "g",
      par_remove_pattern = "\\s+\\(.*\\)",
      unit_extract_pattern = "\\(.*\\)",
      unit_remove_pattern = "\\(|\\)"
    ),
    "gwq" = list(
      download_shortcut = "q",
      par_remove_pattern = "\\s+\\[.*\\]",
      unit_extract_pattern = "\\[.*\\]",
      unit_remove_pattern = "\\[|\\]"
    )
  )

  download_shortcut <- if (stype %in% names(stype_options)) {
    stype_options[[stype]]$download_shortcut
  } else {
    "s"
  }

  url <- sprintf(
    "%s/station.php?anzeige=%sd&sstation=%s",
    wasserportal_base_url(),
    download_shortcut,
    station
  )

  sreihe <- kwb.utils::selectElements(elements = type, list(
    single = "w",
    single_all = "wa",
    daily = "m",
    monthly = "j"
  ))

  # Format the start date
  if (from_date != "") {
    sdatum <- date_string_de(from_date)
  }

  if (sreihe == "wa") {
    sdatum <- "01.01.1900"
  }

  # Compose the body of the request
  body <- list(
    sreihe = sreihe,
    smode = "c",
    sdatum = sdatum,
    senddatum = date_string_de(Sys.Date()),
    sthema = "gw"
  )

  # Post the request to the web server
  response <- httr::POST(url, body = body, handle = handle)

  if (httr::http_error(response)) {
    message("POST request failed. Returning the response object.")
    return(response)
  }

  # Read the response of the web server as text
  text <- httr::content(response, as = "text", encoding = "Latin1")

  # Split the text into separate lines
  textlines <- strsplit(text, "\n")[[1L]]

  start_line <- which(startsWith(textlines, "Datum"))
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
  if (type != "monthly" && stype == "gwl") {
    stopifnot(ncol(data) == 2L)
  }

  # Name the data columns as given in the first columns of the header row
  names(data) <- header_fields[seq_len(ncol(data))]

  if (stype %in% names(stype_options)) {

    opts <- stype_options[[stype]]

    data <- data %>%
      tidyr::pivot_longer(
        cols = setdiff(names(data), "Datum"),
        names_to = c("parameter_unit"),
        values_to = "Messwert"
      ) %>% dplyr::mutate(
        Messstellennummer = station,
        Parameter = stringr::str_remove(
          .data$parameter_unit,
          pattern = opts$par_remove_pattern
        ),
        Einheit = stringr::str_extract(
          .data$parameter_unit,
          pattern = opts$unit_extract_pattern
        ) %>%
          stringr::str_remove(
            pattern = opts$unit_remove_pattern
          )
      ) %>%
      dplyr::filter(!is.na(.data$Messwert)) %>%
      kwb.utils::selectColumns(c(
        "Messstellennummer", "Datum", "Parameter", "Einheit", "Messwert"
      ))
  }

  data <- data %>%
    dplyr::mutate(Datum = as_date_de(.data$Datum))

  # Return the data frame with the additional fields of the header row as
  # meta information in attribute "metadata"
  add_wasserportal_metadata(data, header_fields)
}
