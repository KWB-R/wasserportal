#' Get Surface Water Quality for Multiple Monitoring Stations
#'
#' @param station_ids vector with ids of multiple (or one) monitoring stations
#' @param dbg print debug messages (default: TRUE)
#' @return data frame with water quality data for multiple monitoring stations
#' @export
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#' station_ids <- stations$overview_list$surface_water.quality$Messstellennummer
#' swq <- wasserportal::get_surfacewater_qualities(station_ids)
#' str(swq)
#' }
get_surfacewater_qualities <- function(station_ids, dbg = TRUE) {
  n_stations <- length(station_ids)
  kwb.utils::catAndRun(
    messageText = "Downloading surface water quality data",
    newLine = 3,
  expr = {
    swq_list <- lapply(
    station_ids,
    FUN = function (station_id) {
      n <- which(station_id == station_ids)

      kwb.utils::catAndRun(
        messageText = sprintf(
          "%02d/%02d: station_id = '%s'",
          n,
          n_stations,
          station_id
        ),
        expr = {
          get_surfacewater_quality(station_id)
        },
        dbg = dbg
      )
    }
  )
  },
  dbg = dbg)

  dplyr::bind_rows(swq_list)

}
