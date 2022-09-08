
#' Helper function: get groundwater options
#'
#' @return return available groundwater data options and prepare for being used
#' as input for \code{\link{get_groundwater_data}}
#' @export
#' @examples
#' get_groundwater_options()
#'
get_groundwater_options <- function ()
{
  overview_options <- unlist(get_overview_options())

  is_groundwater <- startsWith(names(overview_options), "groundwater")

  overview_options[is_groundwater] %>%
    gsub(pattern = "gws", replacement = "gwl")
}

#' Get Groundwater Data
#'
#' @description wrapper function to scrape all available raw data, i.e. groundwater
#' level and quality data and save in list
#' @param stations stations list as retrieved by \code{\link{get_stations}}
#' @param groundwater_options as retrieved by \code{\link{get_groundwater_options}}
#' @param debug print debug messages (default: TRUE)
#'
#' @return list with elements "groundwater.level" and "groundwater.quality" data
#' frames
#' @export
#' @importFrom stats setNames
#' @importFrom kwb.utils catAndRun
#' @importFrom data.table rbindlist
#' @examples
#' \dontrun{
#' stations <- wasserportal::get_stations()
#' gw_data_list <- get_groundwater_data(stations)
#' str(gw_data_list)
#' }
get_groundwater_data <- function(
    stations,
    groundwater_options = get_groundwater_options(),
    debug = TRUE
)
{
  result <- lapply(
    X = seq_along(groundwater_options),
    FUN = function(i) {
      gw_option <- groundwater_options[i]
      kwb.utils::catAndRun(
        messageText = sprintf(
          "Importing '%s' data (%d/%d)",
          names(gw_option), i, length(groundwater_options)
        ),
        expr = {
          lapply(
            X = stations$overview_list[[names(gw_option)]]$Messstellennummer,
            FUN = function(station) {
              kwb.utils::catAndRun(
                sprintf("Downloading Messstellennummer == '%s'", station),
                expr = {
                  read_wasserportal_raw_gw(station, stype = gw_option)
                },
                dbg = debug
              )
            }) %>%
            data.table::rbindlist()
        }
      )
    })

  stats::setNames(result, names(groundwater_options))
}
