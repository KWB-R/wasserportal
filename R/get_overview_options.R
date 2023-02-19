#' Wasserportal Berlin: get overview options for stations
#'
#' @return list with shortcuts to station overview tables
#' (`wasserportal.berlin.de/messwerte.php?anzeige=tabelle&thema=<shortcut>`)
#' @export
#'
#' @examples
#' get_overview_options()
#'
get_overview_options <- function()
{
  list(
    surface_water = list(
      water_level = "ows",
      flow = "odf",
      temperature = "owt",
      conductivity = "olf",
      ph = "oph",
      oxygen_concentration = "oog",
      oxygen_saturation = "oos"
    ),
    groundwater = list(
      level = "gws",
      quality = "gwq"
    )
  )
}
