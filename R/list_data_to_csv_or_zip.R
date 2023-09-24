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
      messageText = sprintf("Writing '%s'", filename),
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
