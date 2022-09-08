
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
    gsub("gws", "gwl")
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
get_groundwater_data <- function(stations,
                                 groundwater_options = get_groundwater_options(),
                                 debug = TRUE) {
  stats::setNames(lapply(seq_len(length(
    groundwater_options
  )), function (i) {
    gw_option <- groundwater_options[i]
    msg <- sprintf("Importing '%s' data (%d/%d)",
                   names(gw_option),
                   i,
                   length(groundwater_options))
    kwb.utils::catAndRun(messageText = msg,
                         expr = {
                           data.table::rbindlist(lapply(stations$overview_list[[names(gw_option)]]$Messstellennummer,
                                                        function(id) {
                                                          kwb.utils::catAndRun(
                                                            sprintf("Downloading Messstellennummer == '%s'",
                                                                    id),
                                                            expr = {
                                                              read_wasserportal_raw_gw(station = id, stype = gw_option)
                                                            },
                                                            dbg = debug
                                                          )
                                                        }))
                         })
  }), nm = names(groundwater_options))
}
