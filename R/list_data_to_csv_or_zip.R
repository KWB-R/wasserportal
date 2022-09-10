#' Helper function: list data to csv or zip
#'
#' @param data_list data in list form
#' @param file_prefix file prefix
#' @param to_zip whether or not to convert to zip file
#' @return loops through list of data frames and uses list names as filenames
#' @export
#' @importFrom archive archive_write
#' @importFrom readr write_csv
#' @importFrom stringr str_replace str_replace_all
list_data_to_csv_or_zip <- function(data_list, file_prefix, to_zip)
{
  tmp <- lapply(names(data_list), function(name) {

    filename_base <- paste0(file_prefix, to_base_filename(name))

    if (startsWith(filename_base, "surface")) {
      filename_base <- paste0("daily_", filename_base)
    }

    filename_csv <- paste0(filename_base, ".csv")
    filename_zip <- paste0(filename_base, ".zip")

    filename <- ifelse(to_zip, filename_zip, filename_csv)

    kwb.utils::catAndRun(
      messageText = sprintf("Writting '%s'", filename),
      expr = {

        file <- if (to_zip) {
          archive::archive_write(archive = filename_zip, file = filename_csv)
        } else {
          filename_csv
        }

        readr::write_csv(data_list[[name]], file)
      }
    )

    filename
  })

  unlist(tmp)
}


#' Helper function: list timeseries data to zip
#'
#' @param timeseries_data_list time series data in list form as retrieved by
#' \code{\link{get_groundwater_data}} or \code{\link{get_dailygroundwater_data}}
#' @return loops through list of data frames and uses list names as filenames
#' @export
#' @importFrom readr write_csv
#' @importFrom stringr str_replace
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#' # Groundwater Time Series
#' gw_tsdata_list <- wasserportal::get_groundwater_data(stations)
#' gw_tsdata_files <- wasserportal::list_timeseries_data_to_zip(gw_tsdata_list)
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

#' Helper function: list masters data to csv
#'
#' @param masters_data_list masters data in list form as retrieved by
#' \code{\link{get_stations}} sublist element "overview_list"
#' @return loops through list of data frames and uses list names as filenames
#' @export
#' @importFrom readr write_csv
#' @importFrom stringr str_replace
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#' masters_data_csv_files <- wasserportal:list_masters_data_to_csv(stations$overview_list)
#' masters_data_csv_files
#' }
list_masters_data_to_csv <- function(masters_data_list)
{
  list_data_to_csv_or_zip(
    masters_data_list,
    file_prefix = "stations_",
    to_zip = FALSE
  )
}
