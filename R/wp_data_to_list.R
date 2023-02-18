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

#' Helper function: base url for download
#'
#' @return base url for download of csv/zip files prepared by R package
#' @export
#'
base_url_download <- function()
{
  "https://kwb-r.github.io/wasserportal"
}
