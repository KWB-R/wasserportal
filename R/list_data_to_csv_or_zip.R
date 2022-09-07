#' Helper function: list data to csv
#'
#' @param data_list data in list form
#' @param file_prefix file prefix (default: "")
#' @param to_zip convert to zip file (default: FALSE)
#' @return loops through list of data frames and uses list names as filenames
#' @export
#' @importFrom readr write_csv
#' @importFrom stringr str_replace
list_data_to_csv_or_zip <- function(data_list,
                                    file_prefix = "",
                                    to_zip = FALSE) {
  tmp <- lapply(seq_len(length(data_list)), function(i) {


    filename <- sprintf("%s.csv",
    paste0(file_prefix,
           stringr::str_replace(names(data_list[i]), "_", "-") %>%
             stringr::str_replace("_", "-") %>%
             stringr::str_replace("\\.", "_"),
           collapse = "_"))

    filename_zip <- stringr::str_replace(filename, ".csv$", ".zip")

    msg <- sprintf("Writting '%s'",
                   dplyr::if_else(to_zip,
                                  filename_zip,
                                  filename))

    kwb.utils::catAndRun(messageText = msg,
                         expr = {

                           if(to_zip) {
                             readr::write_csv(data_list[[i]],
                                              archive::archive_write(
                                                archive = filename_zip,
                                                file = filename))
                             filename <- filename_zip
                           } else {
                             readr::write_csv(data_list[[i]],
                                              file = filename)
                             }
                           })
    filename
  })
  unlist(tmp)
}
