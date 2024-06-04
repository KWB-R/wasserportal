#' Get Stations
#'
#' @param type vector of character describing the type(s) of output(s) to be
#'   returned. Expected values (and default): \code{c("list", "data.frame",
#'   "crosstable")}. If only one value is given the data is returned in the
#'   expected type. If more than one values are given, a list is returned with
#'   one list element per type.
#' @param run_parallel default: TRUE
#' @param n_cores number of cores to use if \code{run_parallel = TRUE}.
#'   Default: one less than the detected number of cores.
#' @param debug logical indicating whether or not to show debug messages
#' @return list with general station "overview" (either as list "overview_list"
#' or as data.frame "overview_df") and a crosstable with information which
#' parameters is available per station ("x" if available, NA if not)
#' @export
#' @importFrom data.table rbindlist
#' @importFrom dplyr left_join mutate select
#' @importFrom parallel makeCluster parLapply stopCluster
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider separate
#' @examples
#' stations <- wasserportal::get_stations(n_cores = 2L)
#' str(stations)
#'
get_stations <- function(
    type = c("list", "data.frame", "crosstable"),
    run_parallel = TRUE,
    n_cores = parallel::detectCores() - 1L,
    debug = TRUE
)
{
  expected_types <- c("list", "data.frame", "crosstable")

  stopifnot(is.character(type))
  stopifnot(all(type %in% expected_types))
  stopifnot(!anyDuplicated(type))

  overview_options <- unlist(get_overview_options())

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
  overview_list <- cat_and_run(
    sprintf(
      "Importing %d station overviews from Wasserportal Berlin",
      length(overview_options)
    ),
    dbg = debug,
    expr = {
      if (run_parallel) {
        parallel::parLapply(cl, overview_options, FUN)
      } else {
        lapply(overview_options, FUN)
      }
    }
  )

  # Return the list if only the list is requested
  if (identical(type, "list")) {
    return(overview_list)
  }

  # Function to convert overview_options to a data frame
  overview_options_to_df <- function(overview_options) {
    tidyr::separate(
      data.frame(
        key = names(overview_options),
        station_type = as.vector(overview_options)
      ),
      .data$key,
      into = c("water_body", "variable"),
      sep = "\\.",
      remove = FALSE
    )
  }

  # Convert overview_list to a data frame and append metadata from options
  overview_df <- overview_list %>%
    data.table::rbindlist(fill = TRUE, idcol = "key") %>%
    dplyr::left_join(overview_options_to_df(overview_options), by = "key")

  # Return the data frame if only the data frame is requested
  if (identical(type, "data.frame")) {
    return(overview_df)
  }

  # Create crosstable if requested
  crosstable <- if ("crosstable" %in% type) {
    overview_df %>%
      dplyr::select("Messstellennummer", "Messstellenname", "station_type") %>%
      dplyr::mutate(value = "x") %>%
      tidyr::pivot_wider(names_from = "station_type", values_from = "value")
  } # else NULL

  # Return the crosstable if only the crosstable is requested
  if (identical(type, "crosstable")) {
    return(crosstable)
  }

  # If we arrive here, there are at least two types of output requested
  stopifnot(length(type) > 1L)

  # Return a list with all requested types of output
  c(
    if ("list" %in% type) {
      list(overview_list = overview_list)
    },
    if ("data.frame" %in% type) {
      list(overview_df = overview_df)
    },
    if (!is.null(crosstable)) {
      list(crosstable = crosstable)
    }
  )
}
