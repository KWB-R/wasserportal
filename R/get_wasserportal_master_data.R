#' Helper function: Base Url of Berlin Wassersportal
#'
#' @return string with base url of Berlin Wasserportal
#' @export

wasserportal_base_url <- function() {
  "https://wasserportal.berlin.de"
}

#' Wasserportal Berlin: get master data for a single station
#'
#' @param station_id station_id
#' @param url_wasserportal base url to Wasserportal berlin (default:
#' wasserportal_base_url())
#' @return data frame with metadata for
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate rename
#' @importFrom rlang .data
#' @export
#' @examples
#' ## GW Station
#' get_wasserportal_master_data(station_id = 149)
#'
#' ## SW Station
#' get_wasserportal_master_data(station_id = 5865900)

get_wasserportal_master_data <- function (
  station_id,
  url_wasserportal = wasserportal_base_url()
) {


  master_url <- sprintf("%s/station.php?anzeige=i&sstation=%s",
                          url_wasserportal,
                          as.character(station_id))

  html_overview <- xml2::read_html(master_url)

   master_table <-  html_overview %>%
    rvest::html_node(xpath = '//*[@summary="Pegel Berlin"]') %>%
    rvest::html_table()

   if(nrow(master_table) == 0) {
    msg <- sprintf("No master table for station '%s' available at '%s'",
                   station_id,
                   master_url)
     stop(msg)
   } else {
   master_table <- master_table %>%
    dplyr::rename("key" = "X1", "value" = "X2") %>%
    dplyr::mutate(key = stringr::str_remove_all(.data$key, "-")) %>%
    dplyr::mutate(key = kwb.utils::substSpecialChars(.data$key)) %>%
    tidyr::pivot_wider(names_from = "key", values_from = "value")
   }

  master_table

}
