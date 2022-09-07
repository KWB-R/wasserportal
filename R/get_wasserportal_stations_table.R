#' Wasserportal Berlin: get overview options for stations
#'
#' @return list with shortcuts to station overview tables
#' (`wasserportal.berlin.de/messwerte.php?anzeige=tabelle&thema=<shortcut>`)
#' @export
#'
#' @examples
#' get_overview_options()
#'
get_overview_options <- function ()  {

  list(surface_water = list(water_level = "ws",
                            flow = "df",
                            temperature = "wt",
                            conductivity = "lf",
                            ph = "ph",
                            oxygen_concentration = "og",
                            oxygen_saturation = "os"),
       groundwater = list(level = "gws",
                          quality = "gwq")
       )

}


#' Wasserportal Berlin: get stations overview table
#'
#' @param type type of stations table to retrieve. Valid options defined in
#' \code{\link{get_overview_options}}, default: get_overview_options()$groundwater$level
#' @param url_wasserportal base url to Wasserportal berlin (default:
#' \code{\link{wasserportal_base_url}}
#' @return data frame with master data of selected monitoring stations
#' @export
#' @importFrom kwb.utils substSpecialChars
#' @importFrom rvest html_node html_table html_nodes html_attr
#' @importFrom stringr str_remove_all
#' @importFrom xml2 read_html
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @examples
#' types <- wasserportal::get_overview_options()
#' str(types)
#' sw_l <- wasserportal::get_wasserportal_stations_table(type = types$surface_water$water_level)
#' str(sw_l)

get_wasserportal_stations_table <- function (
  type = get_overview_options()$groundwater$level,
  url_wasserportal = wasserportal_base_url()
) {


  if (! is.null(type)) {
    type <- match.arg(type, unlist(get_overview_options()))
  }


overview_url <- sprintf("%s/messwerte.php?anzeige=tabelle&thema=%s",
                        url_wasserportal,
                        type)

html_overview <- xml2::read_html(overview_url)

overview_table <-  html_overview %>%
  rvest::html_node(xpath = '//*[@id="pegeltab"]') %>%
  rvest::html_table()

stammdaten_link <- html_overview %>%
  rvest::html_node(xpath = '//*[@id="pegeltab"]') %>%
  rvest::html_nodes("td") %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href") %>%
  stringr::str_extract(pattern = ".*anzeige=i.*|.*pegelonline.*|.*brandenburg.*")

stammdaten_link <- stammdaten_link[!is.na(stammdaten_link)]

is_wasserportal <- stringr::str_detect(stammdaten_link, pattern = "^station.php")

stammdaten_link[is_wasserportal] <- sprintf("%s/%s",
                                            url_wasserportal,
                                            stammdaten_link[is_wasserportal])

### hack to remove otherwise duplicated Brandenburg master data in case of
### type = c(2surface_water.water_level" = "ws"), i.e.
### "https://pegelportal.brandenburg.de/messstelle.php?fgid=6&pkz=<messstellennummer>&thema=ws_graph"
stammdaten_link <- stammdaten_link[!duplicated(stammdaten_link)]

names(overview_table) <- stringr::str_remove_all(names(overview_table),
                                                 "-") %>%
  kwb.utils::substSpecialChars()

stopifnot(nrow(overview_table) == length(stammdaten_link))
dplyr::bind_cols(overview_table,
                 tibble::tibble(stammdaten_link = stammdaten_link)
                 )

}

