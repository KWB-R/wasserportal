#' Helper function: list data to csv
#'
#' @param data_list data in list form
#' @param file_prefix file prefix (default: "")
#' @param to_zip convert to zip file (default: FALSE)
#' @return loops through list of data frames and uses list names as filenames
#' @export
#' @importFrom readr write_csv
#' @importFrom stringr str_replace
list_data_to_csv_or_zip <- function(
    data_list,
    file_prefix = "",
    to_zip = FALSE
)
{
  tmp <- lapply(names(data_list), function(name) {

    filename_base <- paste0(
      file_prefix,
      name %>%
        stringr::str_replace("_", "-") %>%
        stringr::str_replace("\\.", "_"),
      collapse = "_"
    )

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

        readr::write_csv(data_list[[i]], file)
      }
    )

    filename
  })

  unlist(tmp)
}
