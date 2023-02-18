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
#' variables <- wasserportal::get_surfacewater_variables()
#' variables
#' sw_data_daily <- wasserportal::get_daily_surfacewater_data(stations, variables)
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
  #kwb.utils::assignPackageObjects("wasserportal")
  overviews <- kwb.utils::selectElements(stations, "overview_list")
  crosstable <- kwb.utils::selectElements(stations, "crosstable")

  data_frames <- lapply(names(variables), function(variable_name) {

    #variable_name <- names(variables)[1L]

    kwb.utils::catAndRun(sprintf("Importing '%s'", variable_name), expr = {

      # data frame with stations at which <variable_name> is measured
      station_data <- kwb.utils::selectElements(overviews, variable_name)

      # Identifiers of non-external monitoring stations to loop through
      station_ids <- get_non_external_station_ids(station_data)

      results_per_station <- lapply(
        X = station_ids,
        FUN = function(station_id) {

          #station_id <- station_ids[1L]

          cat(sprintf(
            "Station id: %s (%d/%d)\n",
            station_id,
            which(station_id == station_ids),
            length(station_ids)
          ))

          read_wasserportal(
            station_id,
            from_date = "1900-01-01",
            variables = variables[[variable_name]],
            type = "daily",
            stations_crosstable = crosstable
          )
        }
      )

      names(results_per_station) <- station_ids

      results_per_station %>%
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

# get_non_external_station_ids -------------------------------------------------
get_non_external_station_ids <- function(station_data)
{
  # Function to safely select columns from station_data
  pull <- kwb.utils::createAccessor(station_data)

  is_external <- is_external_link(pull("stammdaten_link"))
  is_berlin <- pull("Betreiber") == "Land Berlin"

  # Identifiers of monitoring stations to loop through
  as.character(pull("Messstellennummer")[is_berlin & !is_external])
}

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
    metadata <- if (!is.null(data)) {
      kwb.utils::getAttribute(data, "metadata")
    } else {
      message(sprintf(
        "Empty data frame when looping through '%s' in %s",
        "sw_data_list", "sw_data_list_to_df()"
      ))
      NULL
    }

    # Index in metadata where we expect the parameter name and unit
    index <- min(which(stringr::str_detect(metadata, ":"))) + 3L

    # tibble with columns <parameter name> and <unit>
    parameter <- parameter_string_to_tibble(metadata[index])

    # Add parameter columns
    dplyr::bind_cols(data, parameter)
  })

  data_frames %>%
    dplyr::bind_rows(.id = "Messstellennummer") %>%
    dplyr::mutate(Datum = as_date_de(.data$Datum))
}
