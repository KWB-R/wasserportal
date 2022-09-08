
#' Get Stations
#'
#' @param run_parallel default: TRUE
#' @return list with general station "overview" (either as list "overview_list"
#' or as data.frame "overview_df") and a crosstable with information which
#' parameters is available per station ("x" if available, NA if not)
#' @export
#' @importFrom dplyr select mutate left_join
#' @importFrom tidyr pivot_wider
#' @importFrom data.table rbindlist
#' @importFrom rlang .data
#' @examples
#' stations <- wasserportal::get_stations()
#' str(stations)
#'
get_stations <- function(run_parallel = TRUE)
{
  overview_options <- unlist(get_overview_options())

  msg <- sprintf(
    "Importing %d station overviews from Wasserportal Berlin",
    length(overview_options)
  )

  if (run_parallel) {

    ncores <- parallel::detectCores() - 1L

    cl <- parallel::makeCluster(ncores)

    overview_list <- kwb.utils::catAndRun(
      messageText = msg,
      expr = parallel::parLapply(cl, overview_options, function(type) {
        try(wasserportal::get_wasserportal_stations_table(type = type))
      })
    )

    parallel::stopCluster(cl)

  } else {

    overview_list <- lapply(overview_options, function(type) {
      try(get_wasserportal_stations_table(type = type))
    })
  }

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
