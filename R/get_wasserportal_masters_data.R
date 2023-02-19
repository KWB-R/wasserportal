#' Wasserportal Berlin: get master data for a multiple stations
#'
#' @param master_urls urls with master data as retrieved by
#'   \code{\link{get_stations}} and one of  "overview_list" sublist elements
#'   column name "stammdaten_link"
#' @param run_parallel default: TRUE
#'
#' @return data frame with metadata for selected master urls
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom data.table rbindlist
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#' ### Reduce  to monitoring stations maintained by Berlin
#' master_urls <- stations$overview_list$surface_water.water_level %>%
#' dplyr::filter(.data$Betreiber == "Land Berlin") %>%
#' dplyr::pull(.data$stammdaten_link)
#'
#' system.time(master_parallel <- get_wasserportal_masters_data(
#'   master_urls
#' ))
#'
#' system.time(master_sequential <- get_wasserportal_masters_data(
#'   master_urls,
#'   run_parallel = FALSE
#' ))
#' }
#'
get_wasserportal_masters_data <- function(
    master_urls,
    run_parallel = TRUE
)
{
  # If applicable, prepare clusters for parallel processing
  if (run_parallel) {
    cl <- parallel::makeCluster(parallel::detectCores() - 1L)
    on.exit(parallel::stopCluster(cl))
  }

  # Define function to be called within the loop
  FUN <- function(master_url) {
    try(get_wasserportal_master_data(master_url))
  }

  master_list <- kwb.utils::catAndRun(
    messageText = sprintf(
      "Importing %d station metadata from Wasserportal Berlin",
      length(master_urls)
    ),
    expr = if (run_parallel) {
      parallel::parLapply(cl, master_urls, FUN)
    } else {
      lapply(master_urls, FUN)
    }
  )

  failed <- sapply(master_list, kwb.utils::isTryError)

  if (any(failed)) {
    message("Failed fetching data from the following URLs:")
    print(master_urls[failed])
  }

  data.table::rbindlist(master_list[!failed], fill = TRUE)
}

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
#' \dontrun{
#' stations <- wasserportal::get_stations()
#'
#' ## GW Station
#' master_url <- stations$overview_list$groundwater.level$stammdaten_link[1]
#' get_wasserportal_master_data(master_url)
#'
#' ## SW Station
#' ### Reduce  to monitoring stations maintained by Berlin
#' master_urls <- stations$overview_list$surface_water.water_level %>%
#' dplyr::filter(.data$Betreiber == "Land Berlin") %>%
#' dplyr::pull(.data$stammdaten_link)
#' get_wasserportal_master_data(master_urls[1])
#' }
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

# stop_on_external_data_provider -----------------------------------------------
stop_on_external_data_provider <- function(url)
{
  if (is_external_link(url)) {

    kwb.utils::stopFormatted(
      paste0(
        "The master_url '%s' you provided refers to an external ",
        "data provider. Currently only master data within '%s' can be ",
        "requested by using the R package 'wasserportal'"
      ),
      url,
      wasserportal_base_url()
    )
  }
}
