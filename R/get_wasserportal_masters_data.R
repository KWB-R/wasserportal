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
#' stations <- wasserportal::get_stations()
#' parallel::detectCores()
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
    try(wasserportal::get_wasserportal_master_data(master_url))
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
