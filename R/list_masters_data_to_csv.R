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
