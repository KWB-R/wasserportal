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
      water_level = "ws",
      flow = "df",
      temperature = "wt",
      conductivity = "lf",
      ph = "ph",
      oxygen_concentration = "og",
      oxygen_saturation = "os"
    ),
    groundwater = list(
      level = "gws",
      quality = "gwq"
    )
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
)
{
  if (! is.null(type)) {
    type <- match.arg(type, unlist(get_overview_options()))
  }

  overview_url <- sprintf(
    "%s/messwerte.php?anzeige=tabelle&thema=%s",
    url_wasserportal,
    type
  )

  html_overview <- xml2::read_html(overview_url)

  overview_table <- html_overview %>%
    rvest::html_node(xpath = '//*[@id="pegeltab"]') %>%
    rvest::html_table()

  # Look for hyperlinks in column 1
  hrefs_1 <- html %>%
    rvest::html_nodes(xpath = '//table[@id="pegeltab"]/tbody/tr/td[1]') %>%
    extract_hrefs()

  # Look for hyperlinks in column 8
  hrefs_8 <- html %>%
    rvest::html_nodes(xpath = '//table[@id="pegeltab"]/tbody/tr/td[8]') %>%
    extract_hrefs()

  # The wasserportal-related hyperlinks in column 8 are slightly different from
  # those in column 1. Adapt the links in column 8 before "merging" them with
  # the links in column 1.
  hrefs_8 <- kwb.utils::multiSubstitute(hrefs_8, list(
    "anzeige=[^&]+" = "anzeige=i",
    "stable=gwq" = "stable=gws"
  ))

  # "Merge" hrefs_1 with hrefs_8: Use hrefs_1 if not NA else hrefs_8 and warn
  # if both are given but different
  hrefs <- kwb.utils::parallelNonNA(hrefs_1, hrefs_8)

  # Report about differing hrefs in the two columns
  print_invalid_hrefs(hrefs)

  # Prefix the wasserportal-related hyperlinks with the wasserportal base URL
  is_wasserportal <- startsWith(hrefs, "station.php")

  hrefs[is_wasserportal] <- sprintf(
    "%s/%s",
    url_wasserportal,
    hrefs[is_wasserportal]
  )

  names(overview_table) <- names(overview_table) %>%
    stringr::str_remove_all("-") %>%
    kwb.utils::substSpecialChars()

  dplyr::bind_cols(
    overview_table,
    tibble::tibble(stammdaten_link = hrefs)
  )
}

# extract_hrefs ----------------------------------------------------------------
extract_hrefs <- function(x)
{
  hrefs <- rep(NA_character_, length(x))

  links <- rvest::html_node(x, "a")

  has_link <- !is.na(links)

  hrefs[has_link] <- rvest::html_attr(links[has_link], "href")

  hrefs
}

# print_invalid_hrefs ----------------------------------------------------------
print_invalid_hrefs <- function(hrefs)
{
  invalid <- attr(hrefs, "invalid")

  if (is.null(invalid)) {
    return()
  }

  message("There are different hrefs in column 1 and column 8 of the table.")
  print(invalid)
}
