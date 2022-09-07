#' Helper function: list data to csv
#'
#' @param data_list data in list form
#' @param file_prefix file prefix (default: "")
#'
#' @return loops through list of data frames and uses list names as filenames
#' @export
#' @importFrom readr write_csv
#'
list_data_to_csv <- function(data_list,
                             file_prefix = "") {
  tmp <- lapply(seq_len(length(data_list)), function(i) {
    filename <- sprintf("%s.csv",
    paste0(file_prefix, names(data_list[i]), collapse = "_"))
    msg <- sprintf("Writting '%s'", filename)

    kwb.utils::catAndRun(messageText = msg,
                         expr = {
                           readr::write_csv(data_list[[i]],
                                            file = filename)
                         })
    filename
  })
  unlist(tmp)
}
