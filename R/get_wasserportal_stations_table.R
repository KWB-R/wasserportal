#' Wasserportal Berlin: get stations overview table
#'
#' @param type type of stations table to retrieve. Valid options defined in
#' \code{\link{get_overview_options}}, default: get_overview_options()$groundwater$level
#' @param url_wasserportal base url to Wasserportal berlin (default:
#' \code{\link{wasserportal_base_url}}
#' @return data frame with master data of selected monitoring stations
#' @export
#' @importFrom rvest html_node html_nodes html_attr
#' @importFrom stringr str_remove_all
#' @importFrom xml2 read_html xml_find_all xml_text
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble as_tibble
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

  html <- xml2::read_html(overview_url, encoding = "UTF-8")

  pegeltab <- rvest::html_node(html, xpath = '//*[@id="pegeltab"]')

  if (is.na(pegeltab)) {
    stop(
      "Could not find element with id 'pegeltab' in HTML returned by ",
      overview_url, call. = FALSE
    )
  }

  # Get the column captions from the table header
  captions <- html %>%
    rvest::html_nodes(xpath = '//table[@id="pegeltab"]/thead/tr/th') %>%
    rvest::html_text()

  # Convert the HTML table into a data frame manually rather than via
  # rvest::html_table(). On Windows R the latter pipes cell text through
  # gsub() in the C locale and chokes on the Latin-1 bytes the wasserportal
  # server returns (e.g. "Auspr<e4>gung"). Going through xml2 + enc2utf8
  # keeps the strings in UTF-8 throughout.
  overview_table <- html_table_utf8(pegeltab, n_cols = length(captions))

  # Apply the captions (with special characters preserved as UTF-8) as names
  names(overview_table) <- enc2utf8(captions)

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
  # hrefs_graph <- multi_substitute(hrefs_graph, list(
  #   "anzeige=[^&]+" = "anzeige=i",
  #   "stable=gwq" = "stable=gws"
  # ))
  #
  # # "Merge" hrefs_id with hrefs_graph: Use hrefs_id if not NA else hrefs_graph
  # # and warn if both are given but different
  # hrefs <- parallel_non_na(hrefs_id, hrefs_graph)
  #
  # # Report about differing hrefs in the two columns
  # #print_invalid_hrefs(hrefs)

  # Prefix the wasserportal-related hyperlinks with the wasserportal base URL
  add_baseurl <- function(hrefs) {

  is_not_na <- !is_na_or_empty(hrefs)

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
    subst_special_chars()


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

# html_table_utf8 --------------------------------------------------------------
# Lightweight replacement for rvest::html_table() that keeps cell text in UTF-8.
# rvest::html_table() routes cell text through gsub() in the current C locale;
# on Windows R that fails on Latin-1 bytes returned by wasserportal.berlin.de
# with "input string ... is invalid" / "unable to translate '...' to a wide
# string". We extract rows directly via xml2 and force-encode each value with
# enc2utf8() so downstream string operations succeed regardless of the locale.
html_table_utf8 <- function(table_node, n_cols)
{
  rows <- xml2::xml_find_all(table_node, ".//tbody/tr")

  if (length(rows) == 0L) {
    cols <- replicate(n_cols, character(0L), simplify = FALSE)
    names(cols) <- paste0("X", seq_len(n_cols))
    return(tibble::as_tibble(cols))
  }

  cell_values <- function(row) {
    cells <- xml2::xml_find_all(row, ".//td|.//th")
    text <- xml2::xml_text(cells)
    text <- trim_bytes(text)
    Encoding(text) <- "UTF-8"
    length(text) <- n_cols
    text
  }

  matrix_cells <- vapply(rows, cell_values, character(n_cols))

  if (is.null(dim(matrix_cells))) {
    matrix_cells <- matrix(matrix_cells, nrow = n_cols)
  }

  cols <- lapply(seq_len(n_cols), function(i) matrix_cells[i, ])
  names(cols) <- paste0("X", seq_len(n_cols))
  tibble::as_tibble(cols)
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
