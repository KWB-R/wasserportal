#' Wasserportal Berlin: get master data for a multiple stations
#'
#' @param station_ids station_ids
#' @param run_parallel default: TRUE
#'
#' @return data frame with metadata for selected station ids
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom data.table rbindlist
#' @examples
#' station_ids <- 1:4
#' parallel::detectCores()
#' system.time(get_wasserportal_masters_data(station_ids))
#' system.time(get_wasserportal_masters_data(station_ids,
#'                                           run_parallel = FALSE))
#'
get_wasserportal_masters_data <- function(
  station_ids,
  run_parallel = TRUE
)
{
  msg <- sprintf("Importing %d station metadata from Wasserportal Berlin",
                 length(station_ids))


  if (run_parallel) {

    ncores <- parallel::detectCores() - 1

    cl <- parallel::makeCluster(ncores)

    master_list <- kwb.utils::catAndRun(
      messageText = msg,
      expr = parallel::parLapply(
        cl, station_ids, function(id) {
          try(wasserportal::get_wasserportal_master_data(station_id = id))
        })
    )

    parallel::stopCluster(cl)

  } else {

    master_list <- lapply(station_ids, function(id) {
      wasserportal::get_wasserportal_master_data(id)
      })

  }

  data.table::rbindlist(master_list)

}
