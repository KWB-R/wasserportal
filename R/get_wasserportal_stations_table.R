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

  overview_url <- paste0(
    url_wasserportal,
    "/messwerte.php?",
    url_parameter_string(anzeige = "tabelle", thema = type)
  )

  html <- xml2::read_html(overview_url)

  pegeltab <- rvest::html_node(html, xpath = '//*[@id="pegeltab"]')

  if (is.na(pegeltab)) {
    stop(
      "Could not find element with id 'pegeltab' in HTML returned by ",
      overview_url, call. = FALSE
    )
  }

  # Convert the HTML table into a data frame
  overview_table <- rvest::html_table(pegeltab)

  # Get the column captions from the table header
  captions <- html %>%
    rvest::html_nodes(xpath = '//table[@id="pegeltab"]/thead/tr/th') %>%
    rvest::html_text()

  # Identify columns "Messstellennummer" and "Ganglinie"
  column_id <- grep("Mess.?stellen.?nummer", captions)
  column_graph <- grep("Gang.?linie", captions)

  stopifnot(length(column_id) == 1L)
  stopifnot(length(column_graph) == 1L)

  # Function to create xpath expression to match the cells in column i
  xpath_column <- function(i) {
    sprintf('//table[@id="pegeltab"]/tbody/tr/td[%d]', i)
  }

  # Look for hyperlinks in column "Messstellennummer"
  hrefs_id <- html %>%
    rvest::html_nodes(xpath = xpath_column(column_id)) %>%
    extract_hrefs()

  # Look for hyperlinks in column "Ganglinie"
  hrefs_graph <- html %>%
    rvest::html_nodes(xpath = xpath_column(column_graph)) %>%
    extract_hrefs()

  # Do not combine both links
  #
  # # The wasserportal-related hyperlinks in column "Ganglinie" are slightly
  # # different from those in column "Messstellennummer". Adapt the links in
  # # column "Ganglinie" before "merging" them with the links in column
  # # "Messstellennummer".
  # hrefs_graph <- kwb.utils::multiSubstitute(hrefs_graph, list(
  #   "anzeige=[^&]+" = "anzeige=i",
  #   "stable=gwq" = "stable=gws"
  # ))
  #
  # # "Merge" hrefs_id with hrefs_graph: Use hrefs_id if not NA else hrefs_graph
  # # and warn if both are given but different
  # hrefs <- kwb.utils::parallelNonNA(hrefs_id, hrefs_graph)
  #
  # # Report about differing hrefs in the two columns
  # #print_invalid_hrefs(hrefs)

  # Prefix the wasserportal-related hyperlinks with the wasserportal base URL
  add_baseurl <- function(hrefs) {

  is_not_na <- ! kwb.utils::isNaOrEmpty(hrefs)

  if(sum(is_not_na) > 0) {
  is_wasserportal <- startsWith(hrefs, "station.php") & is_not_na

  hrefs[is_wasserportal] <- sprintf(
    "%s/%s",
    url_wasserportal,
    hrefs[is_wasserportal]
  )
  } else {
   hrefs <- NA_character_
  }

  hrefs
  }

  overview_table[[column_graph]] <- add_baseurl(hrefs_graph)

  names(overview_table) <- names(overview_table) %>%
    stringr::str_remove_all("-") %>%
    kwb.utils::substSpecialChars()


  dplyr::bind_cols(
    overview_table,
    tibble::tibble(stammdaten_link = add_baseurl(hrefs_id))
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
