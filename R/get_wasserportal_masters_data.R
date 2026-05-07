#' Wasserportal Berlin: get master data for a multiple stations
#'
#' @param master_urls URLs to master data as found in column "stammdaten_link"
#'   of the data frame returned by
#'   \code{\link{get_stations}}\code{(type = "list")}
#' @param run_parallel default: TRUE
#'
#' @return data frame with metadata for selected master urls
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom data.table rbindlist
#' @examples
#' \dontrun{
#' stations_list <- wasserportal::get_stations(type = "list")
#'
#' # Reduce  to monitoring stations maintained by Berlin
#' master_urls <- stations_list$surface_water.water_level %>%
#'   dplyr::filter(.data$Betreiber == "Land Berlin") %>%
#'   dplyr::pull(.data$stammdaten_link)
#'
#' system.time(master_parallel <- get_wasserportal_masters_data(
#'   master_urls
#' ))
#'
#' system.time(master_sequential <- get_wasserportal_masters_data(
#'   master_urls,
#'   run_parallel = FALSE
#' ))
#' }
#'
get_wasserportal_masters_data <- function(
    master_urls,
    run_parallel = TRUE
)
{
  # If applicable, prepare clusters for parallel processing
  if (run_parallel) {
    cl <- parallel::makeCluster(parallel::detectCores() - 1L)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterEvalQ(cl, loadNamespace("wasserportal"))
  }

  # Define function to be called within the loop. Use a namespace-qualified
  # call so that the function is found in worker processes regardless of
  # whether the wasserportal package is attached there.
  FUN <- function(master_url) {
    try(wasserportal::get_wasserportal_master_data(master_url))
  }

  master_list <- cat_and_run(
    messageText = sprintf(
      "Importing master data for %d stations from Wasserportal Berlin",
      length(master_urls)
    ),
    expr = if (run_parallel) {
      parallel::parLapply(cl, master_urls, FUN)
    } else {
      lapply(master_urls, FUN)
    }
  )

  failed <- sapply(master_list, is_try_error)

  if (any(failed)) {
    message("Failed fetching data from the following URLs:")
    print(master_urls[failed])
  }

  data.table::rbindlist(master_list[!failed], fill = TRUE)
}

#' Wasserportal Berlin: get master data for a single station
#'
#' @param master_url url with master data for single station as retrieved by
#' \code{\link{get_wasserportal_stations_table}}
#' @return data frame with metadata for selected station
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom xml2 read_html xml_find_all xml_text
#' @export
#' @examples
#' \dontrun{
#' stations_list <- wasserportal::get_stations(type = "list")
#'
#' # GW Station
#' master_url <- stations_list %>%
#'   kwb.utils::selectElements("groundwater.level") %>%
#'   kwb.utils::selectColumns("stammdaten_link")[1L]
#'
#' get_wasserportal_master_data(master_url)
#'
#' # SW Station
#'
#' # Reduce  to monitoring stations maintained by Berlin
#' master_urls <- stations_list %>%
#'   kwb.utils::selectElements("surface_water.water_level") %>%
#'   dplyr::filter(.data$Betreiber == "Land Berlin") %>%
#'   dplyr::pull(.data$stammdaten_link)
#'
#' get_wasserportal_master_data(master_urls[1L])
#' }
#'
get_wasserportal_master_data <- function(master_url)
{
  stop_on_external_data_provider(master_url)

  # The wasserportal pages have switched from the legacy HTML4 attribute
  # `summary="Pegel Berlin"` on the master-data <table> to a child
  # <caption class="sr-only">Pegel Berlin</caption>. Match on the caption so
  # the function works with the current HTML5 markup. The page renders the
  # table twice (desktop view + mobile view); html_node() returns the first
  # match, which is the desktop variant.
  node <- master_url %>%
    xml2::read_html(encoding = "UTF-8") %>%
    rvest::html_node(
      xpath = '//table[caption[normalize-space()="Pegel Berlin"]]'
    )

  if (inherits(node, "xml_missing")) {
    stop_formatted("No master table available at '%s'", master_url)
  }

  # Extract rows manually rather than via rvest::html_table(): on Windows R
  # the latter pipes the cell text through gsub() in the C locale and chokes
  # on the Latin-1 bytes returned by wasserportal (e.g. "Auspr<e4>gung").
  # Going through xml2 + manual byte-level trim keeps the strings intact;
  # xml_text(trim = TRUE) also fails on Windows because xml2's internal
  # trim_text() calls sub("^[[:space:] ]+", ...) which needs a wide-string
  # conversion that the C locale cannot provide.
  rows <- xml2::xml_find_all(node, ".//tbody/tr")

  pair_text <- function(row) {
    cells <- xml2::xml_find_all(row, ".//td|.//th")
    text <- xml2::xml_text(cells)
    text <- trim_bytes(text)
    Encoding(text) <- "UTF-8"
    length(text) <- 2L
    text
  }

  if (length(rows) == 0L) {
    stop_formatted("No master table available at '%s'", master_url)
  }

  pairs <- vapply(rows, pair_text, character(2L))

  if (is.null(dim(pairs))) {
    pairs <- matrix(pairs, nrow = 2L)
  }

  keys <- pairs[1L, ]
  values <- pairs[2L, ]

  if (all(is.na(keys))) {
    stop_formatted("No master table available at '%s'", master_url)
  }

  tibble::tibble(key = keys, value = values) %>%
    dplyr::mutate(key = stringr::str_remove_all(.data$key, "-")) %>%
    dplyr::mutate(key = subst_special_chars(.data$key)) %>%
    tidyr::pivot_wider(names_from = "key", values_from = "value")
}

# stop_on_external_data_provider -----------------------------------------------
stop_on_external_data_provider <- function(url)
{
  if (is_external_link(url)) {

    stop_formatted(
      paste0(
        "The master_url '%s' you provided refers to an external ",
        "data provider. Currently only master data within '%s' can be ",
        "requested by using the R package 'wasserportal'"
      ),
      url,
      wasserportal_base_url()
    )
  }
}
