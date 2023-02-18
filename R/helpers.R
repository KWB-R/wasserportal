#' Helper function: base url for download
#'
#' @return base url for download of csv/zip files prepared by R package
#' @export
#'
base_url_download <- function() {
  "https://kwb-r.github.io/wasserportal"
}

# is_external_link -------------------------------------------------------------
is_external_link <- function(url)
{
  !startsWith(url, wasserportal_base_url())
}

# to_base_filename -------------------------------------------------------------
to_base_filename <- function(x)
{
  x %>%
  stringr::str_replace_all("_", "-") %>%
    stringr::str_replace("\\.", "_")
}

# stop_on_external_data_provider -----------------------------------------------
stop_on_external_data_provider <- function(url)
{
  if (is_external_link(url)) {

    kwb.utils::stopFormatted(
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

#' Helper function: Base Url of Berlin Wassersportal
#'
#' @return string with base url of Berlin Wasserportal
#' @export
wasserportal_base_url <- function() {
  "https://wasserportal.berlin.de"
}
