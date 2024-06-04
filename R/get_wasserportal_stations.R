# get_wasserportal_stations ----------------------------------------------------

#' Get Names and IDs of the Stations of wasserportal.berlin.de
#'
#' @param type one of "quality", "level", "flow"
#' @export
get_wasserportal_stations <- function(type = "quality")
{
  if (! is.null(type)) {
    type <- match.arg(type, c("quality", "level", "flow"))
  }

  file <- "stations_wasserportal.csv"

  stations <- readPackageFile(file, fileEncoding = "UTF-8")

  stations$id <- as.character(select_columns(stations, "id"))
  stations$name <- subst_special_chars(select_columns(stations, "name"))

  if (!is.null(type)) {
    stations <- stations[nzchar(select_columns(stations, type)), ]
  }

  to_lookup_list(
    keys = select_columns(stations, "name"),
    values = select_columns(stations, "id")
  )
}
