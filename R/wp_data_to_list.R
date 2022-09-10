# wp_data_to_list --------------------------------------------------------------
wp_data_to_list <- function(
    overview_list_names,
    target_dir,
    is_zipped,
    modify_filenames
)
{
  fs::dir_create(target_dir, recurse = TRUE)

  filenames_base <- overview_list_names %>%
    to_base_filename() %>%
    modify_filenames()

  filenames_csv <- paste0(filenames_base, ".csv")
  filenames_zip <- paste0(filenames_base, ".zip")

  filenames <- if (is_zipped) filenames_zip else filenames_csv

  results <- lapply(seq_along(filenames_base), function(i) {

    url <- file.path(base_url_download(), filenames[i])

    if (is_zipped) {

      withr::with_dir(new = target_dir, code = {
        archive::archive_extract(url) %>%
          readr::read_csv()
      })

    } else {

      target_path <- file.path(target_dir, filenames_csv[i])

      try(utils::download.file(url = url, destfile = target_path))

      readr::read_csv(file = target_path)
    }
  })

  stats::setNames(results, filenames_base)
}

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

#' Wasserportal Time Series Data: download and Import in R List
#'
#' @param overview_list_names names of "overview_list" as retrieved by
#' \code{\link{get_stations}}
#' @param target_dir target directory for downloading data (default:
#' tempdir())
#' @param is_zipped are the data to be downloaded zipped (default:
#' TRUE)
#'
#' @return downloads (zipped) data from wasserportal
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
#' wp_timeseries_data_list <- wp_timeseries_data_to_list(overview_list_names)
#' }
wp_timeseries_data_to_list <- function(
    overview_list_names,
    target_dir = tempdir(),
    is_zipped = TRUE
)
{
  wp_data_to_list(
    overview_list_names,
    target_dir,
    is_zipped,
    modify_filenames = function(x) {
      stringr::str_replace_all(x, "^surface", "daily_surface")
    }
  )
}
