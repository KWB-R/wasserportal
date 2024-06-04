#' Helper function: get available station variables
#'
#' @param station_df data frame with one row per station and columns
#'   "Messstellennummer", "Messstellenname" and additional columns each of which
#'   represents a variable that is measured at that station. If the variable
#'   columns contain the value "x" it means that the corresponding variable is
#'   measured and the name of the column is contained in the returned vector of
#'   variable names.
#'
#' @return returns names of available variables for station
#' @export
#'
#' @importFrom dplyr select_if
#'
get_station_variables <- function(station_df)
{
  stopifnot(is.data.frame(station_df))

  variables <- station_df %>%
    remove_columns(c("Messstellennummer", "Messstellenname")) %>%
    remove_empty_columns(dbg = FALSE) %>%
    names()

  all_variables <- unlist(get_overview_options())

  stop_if_not_all_in(variables, all_variables, type = "variable code")

  all_variables[match(variables, all_variables)]
}
