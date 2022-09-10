#' Helper function: convert surface water data list to data frame
#'
#' @param sw_data_list sw_data_list
#'
#' @return data frame
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom  stats setNames
#' @importFrom stringr str_detect str_split_fixed
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols bind_rows
sw_data_list_to_df <- function (sw_data_list) {
  stats::setNames(lapply(names(sw_data_list), function(name) {
    tmp <- sw_data_list[[name]][[1]]

    start_idx <- min(which(stringr::str_detect(attr(tmp, "metadata"), ":"))) + 3

    parameter_unit <- stringr::str_split_fixed(attr(tmp, "metadata")[start_idx],
                                               pattern = " in | im ",
                                               n = 2)

    meta <- tibble::tibble(Parameter = parameter_unit[1],
                           Einheit = parameter_unit[2])

    dplyr::bind_cols(tmp, meta)}),
    names(sw_data_list)) %>%
    dplyr::bind_rows(.id = "Messstellennummer") %>%
    dplyr::mutate(Datum = as_date_de(.data$Datum))
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
  sw_data_list <- stats::setNames(nm = names(variables), lapply(
    X = seq_along(variables),
    FUN = function(i) {

      fname <- names(variables[i])
      fvalue <- as.vector(variables[i])

      kwb.utils::catAndRun(sprintf("Importing '%s'", fname), expr = {

        master_urls <- stations$overview_list[[fname]] %>%
          dplyr::filter(.data$Betreiber == "Land Berlin") %>%
          dplyr::pull(.data$stammdaten_link)

        sw_numbers <- master_urls %>%
          get_wasserportal_masters_data() %>%
          kwb.utils::selectElements("Nummer")

        sw_data_list <- lapply(
          X = sw_numbers,
          FUN = read_wasserportal,
          from_date = "1900-01-01",
          variables = fvalue,
          type = "daily",
          stations_crosstable = stations$crosstable
        )

        sw_data_list %>%
          stats::setNames(sw_numbers) %>%
          sw_data_list_to_df() %>%
          dplyr::filter(.data$Tagesmittelwert != -777)
      })
    })
  )

  if (!list2df) {
    return(sw_data_list)
  }

  dplyr::bind_rows(sw_data_list)
}
