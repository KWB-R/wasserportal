#' Wasserportal Berlin: get master data for a single station
#'
#' @param master_url url with master data for single station as retrieved by
#' \code{\link{get_wasserportal_stations_table}}
#' @return data frame with metadata for selected station
#' @importFrom dplyr mutate rename
#' @importFrom kwb.utils stopFormatted
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @export
#' @examples
#' stations <- wasserportal::get_stations()
#'
#'## GW Station
#' master_url <- stations$overview_list$groundwater.level$stammdaten_link[1]
#' get_wasserportal_master_data(master_url)
#'
#'## SW Station
#' ### Reduce  to monitoring stations maintained by Berlin
#' master_urls <- stations$overview_list$surface_water.water_level %>%
#' dplyr::filter(.data$Betreiber == "Land Berlin") %>%
#' dplyr::pull(.data$stammdaten_link)
#' get_wasserportal_master_data(master_urls[1])
#'
get_wasserportal_master_data <- function(master_url)
{
  stop_on_external_data_provider(master_url)

  master_table <- master_url %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//*[@summary="Pegel Berlin"]') %>%
    rvest::html_table()

  if (nrow(master_table) == 0L) {
    kwb.utils::stopFormatted("No master table available at '%s'", master_url)
  }

  master_table %>%
    dplyr::rename("key" = "X1", "value" = "X2") %>%
    dplyr::mutate(key = stringr::str_remove_all(.data$key, "-")) %>%
    dplyr::mutate(key = kwb.utils::substSpecialChars(.data$key)) %>%
    tidyr::pivot_wider(names_from = "key", values_from = "value")
}
