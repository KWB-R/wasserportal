#' Helper function: list data to csv
#'
#' @param data_list data in list form
#' @param file_prefix file prefix (default: "")
#' @param to_zip convert to zip file (default: FALSE)
#' @return loops through list of data frames and uses list names as filenames
#' @export
#' @importFrom readr write_csv
#' @importFrom stringr str_replace
#' @importFrom archive archive_write
#' @importFrom fs file_delete
list_data_to_csv_or_zip <- function(data_list,
                                    file_prefix = "",
                                    to_zip = FALSE) {
  tmp <- lapply(seq_len(length(data_list)), function(i) {


    filename <- sprintf("%s.csv",
    paste0(file_prefix, names(data_list[i]), collapse = "_"))
    msg <- sprintf("Writting '%s'", filename)

    kwb.utils::catAndRun(messageText = msg,
                         expr = {
                           readr::write_csv(data_list[[i]],
                                            file = filename)

                           if(to_zip) {
                             filename_zip <- stringr::str_replace(filename, ".csv$", ".zip")
                             archive::archive_write(
                             archive = filename_zip,
                             file = filename)
                             fs::file_delete(path = filename)
                             filename <- filename_zip
                         }})
    filename
  })
  unlist(tmp)
}
