#' Wasserportal Master Data: download and Import in R List
#'
#' @param overview_list_names names of "overview_list" as retrieved by
#' \code{\link{get_stations}}
#' @param target_dir target directory for downloading data (default:
#' tempdir())
#' @param file_prefix prefix given to file names
#' @param is_zipped are the data to be downloaded zipped (default:
#' FALSE)
#'
#' @return downloads csv master data from Wasserportal
#' @export
#' @importFrom archive archive_extract
#' @importFrom fs dir_create
#' @importFrom utils download.file
#' @importFrom readr read_csv
#' @importFrom stringr str_replace str_replace_all
#' @importFrom withr with_dir
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#' overview_list_names <- names(stations$overview_list)
#' wp_masters_data_list <- wp_masters_data_to_list(overview_list_names)
#' }
wp_masters_data_to_list <- function(
    overview_list_names,
    target_dir = tempdir(),
    file_prefix = "stations_",
    is_zipped = FALSE
)
{
  wp_data_to_list(
    overview_list_names,
    target_dir,
    is_zipped,
    modify_filenames = function(x) paste0(file_prefix, x)
  )
}
