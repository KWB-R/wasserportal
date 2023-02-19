#' Get Stations
#'
#' @param run_parallel default: TRUE
#' @param n_cores number of cores to use if \code{run_parallel = TRUE}.
#'   Default: one less than the detected number of cores.
#' @return list with general station "overview" (either as list "overview_list"
#' or as data.frame "overview_df") and a crosstable with information which
#' parameters is available per station ("x" if available, NA if not)
#' @export
#' @importFrom data.table rbindlist
#' @importFrom dplyr left_join mutate select
#' @importFrom kwb.utils catAndRun
#' @importFrom parallel makeCluster parLapply stopCluster
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider separate
#' @examples
#' stations <- wasserportal::get_stations(n_cores = 2L)
#' str(stations)
#'
get_stations <- function(
    run_parallel = TRUE, n_cores = parallel::detectCores() - 1L
)
{
  overview_options <- unlist(get_overview_options())

  # Prepare message text for console output
  messageText <- sprintf(
    "Importing %d station overviews from Wasserportal Berlin",
    length(overview_options)
  )

  # Prepare parallel processing if desired
  if (run_parallel) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))
  }

  # Function to be called within a loop
  FUN <- function(type) {
    try(get_wasserportal_stations_table(type = type))
  }

  # Loop through overview_options, either in parallel or sequentially
  overview_list <- kwb.utils::catAndRun(messageText, expr = {
    if (run_parallel) {
      parallel::parLapply(cl, overview_options, FUN)
    } else {
      lapply(overview_options, FUN)
    }
  })

  overview_df <- data.table::rbindlist(
    overview_list,
    fill = TRUE,
    idcol = "key"
  )

  metadata <- tidyr::separate(
    data.frame(
      key = names(overview_options),
      station_type = as.vector(overview_options)
    ),
    .data$key,
    into = c("water_body", "variable"),
    sep = "\\.",
    remove = FALSE
  )

  overview_df <- dplyr::left_join(overview_df, metadata, by = "key")

  crosstable <- overview_df %>%
    dplyr::select("Messstellennummer", "Messstellenname", "station_type") %>%
    dplyr::mutate(value = "x") %>%
    tidyr::pivot_wider(
      names_from = "station_type",
      values_from = "value"
    )

  list(
    overview_list = overview_list,
    overview_df = overview_df,
    crosstable = crosstable
  )
}
