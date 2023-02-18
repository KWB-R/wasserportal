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
