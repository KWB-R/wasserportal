#' Helper function: base url for download
#' @keywords internal
#' @noMd
#' @noRd
#' @return base url for download of csv/zip files prepared by R package
#' @export
#'
base_url_download <- function() {
"https://kwb-r.github.io/wasserportal"
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
wp_timeseries_data_to_list <- function(overview_list_names,
                            target_dir = tempdir(),
                            is_zipped = TRUE) {

  fs::dir_create(target_dir, recurse = TRUE)


  filenames_base <- overview_list_names %>%
    stringr::str_replace_all("_", "-") %>%
    stringr::str_replace("\\.", "_")

  filenames_base <- stringr::str_replace_all(filenames_base,
                                             "^surface",
                                             "daily_surface")

  filenames_csv <- paste0(filenames_base, ".csv")
  filenames_zip <- paste0(filenames_base, ".zip")


  stats::setNames(lapply(seq_len(length(filenames_base)), function(i) {

    url <- sprintf("%s/%s",
                   base_url_download(),
                   ifelse(is_zipped,
                          filenames_zip[i],
                          filenames_csv[i]))

    if(is_zipped) {

      withr::with_dir(new = target_dir,
                      code = {
                        archive::archive_extract(url) %>%
                          readr::read_csv()
                      })
    } else {
      target_path <- file.path(target_dir, filenames_csv[i])
      try(utils::download.file(url = url, destfile = target_path))
      readr::read_csv(file = target_path)
    }
  }), nm = filenames_base
  )
}
