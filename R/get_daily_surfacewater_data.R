#' Helper function: convert surface water data list to data frame
#'
#' @param sw_data_list sw_data_list
#'
#' @return data frame
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom stats setNames
#' @importFrom stringr str_detect str_split_fixed
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom kwb.utils getAttribute
sw_data_list_to_df <- function (sw_data_list)
{
  # Helper function to split parameter string into parameter and unit
  parameter_string_to_tibble <- function(x) {

    parts <- stringr::str_split_fixed(x, pattern = " in | im ", n = 2L)

    tibble::tibble(
      Parameter = parts[1L],
      Einheit = parts[2L]
    )
  }

  data_frames <- lapply(sw_data_list, function(x) {

    # Select the first data frame
    data <- x[[1L]]

    # Get its metadata
    metadata <- kwb.utils::getAttribute(data, "metadata")

    # Index in metadata where we expect the parameter name and unit
    index <- min(which(stringr::str_detect(metadata, ":"))) + 3L

    # tibble with columns <parameter name> and <unit>
    parameter <- parameter_string_to_tibble(metadata[index])

    # Add parameter columns
    dplyr::bind_cols(data, parameter)
  })

  data_frames %>%
    dplyr::bind_rows(.id = "Messstellennummer") %>%
    dplyr::mutate(Datum = as.Date(.data$Datum, format = "%d.%m.%Y"))
}

#' Helper function: get surface water variables
#'
#' @return vector with surface water variables
#' @export
#'
#' @importFrom stringr str_detect
get_surfacewater_variables <- function()
{
  variables <- unlist(get_overview_options())

  variables[startsWith(names(variables), "surface")]
}

#' Get Daily Surfacewater Data: wrapper to scrape daily surface water data
#'
#' @param stations stations as retrieved by by \code{\link{get_stations}}
#' @param variables variables as retrieved by by \code{\link{get_surfacewater_variables}}
#' @param list2df convert result list to data frame (default: FALSE)
#' @return list or data frame with all available data from Wasserportal
#' @export
#'
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#' variables <- get_surfacewater_variables()
#' variables
#' sw_data_daily <- get_daily_surfacewater_data(stations, variables)
#' }
#' @importFrom kwb.utils catAndRun
#' @importFrom dplyr bind_rows filter pull
#' @importFrom stats setNames
get_daily_surfacewater_data <- function(
    stations,
    variables = get_surfacewater_variables(),
    list2df = FALSE
)
{
  overviews <- kwb.utils::selectElements(stations, "overview_list")
  crosstable <- kwb.utils::selectElements(stations, "crosstable")

  data_frames <- lapply(names(variables), function(variable_name) {

    #variable_name <- names(variables)[1L]

    kwb.utils::catAndRun(sprintf("Importing '%s'", variable_name), expr = {

      # data frame with stations at which <variable_name> is measured
      station_data <- kwb.utils::selectElements(overviews, variable_name)

      masterdata_urls <- station_data %>%
        dplyr::pull(.data$stammdaten_link)

      all_station_numbers <- masterdata_urls %>%
      get_wasserportal_masters_data() %>%
        kwb.utils::selectElements("Nummer")

      #length(all_station_numbers)
      #nrow(station_data)
      #all_station_numbers == as.character(station_data$Messstellennummer)

      belongs_to_berlin <- station_data$Betreiber == "Land Berlin"
      station_numbers <- all_station_numbers[belongs_to_berlin]

      #head(station_data$Messstellennummer)
      #head(station_numbers)

      lapply(
        X = station_numbers,
        FUN = read_wasserportal,
        from_date = "1900-01-01",
        variables = variables[[variable_name]],
        type = "daily",
        stations_crosstable = crosstable
      ) %>%
        stats::setNames(sw_numbers) %>%
        sw_data_list_to_df() %>%
        dplyr::filter(.data$Tagesmittelwert != -777)
    })
  })

  names(data_frames) <- names(variables)

  if (!list2df) {
    return(data_frames)
  }

  dplyr::bind_rows(data_frames)
}
