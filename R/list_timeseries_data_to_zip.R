#' Helper function: list timeseries data to zip
#'
#' @param timeseries_data_list time series data in list form as retrieved by
#' \code{\link{get_groundwater_data}}
#' @return loops through list of data frames and uses list names as filenames
#' @export
#' @importFrom readr write_csv
#' @importFrom stringr str_replace
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#'
#' # Groundwater Time Series
#' gw_tsdata_list <- wasserportal::get_groundwater_data(stations)
#' gw_tsdata_files <- wasserportal::list_timeseries_data_to_zip(gw_tsdata_list)
#'
#' # Surface Water Time Series
#' sw_tsdata_list <- wasserportal::get_daily_surfacewater_data(stations)
#' sw_tsdata_files <- wasserportal::list_timeseries_data_to_zip(sw_tsdata_list)
#' }
list_timeseries_data_to_zip <- function(timeseries_data_list)
{
  list_data_to_csv_or_zip(
    timeseries_data_list,
    file_prefix = "",
    to_zip = TRUE
  )
}
