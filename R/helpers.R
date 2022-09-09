# belongs_to_external_data_provider --------------------------------------------
belongs_to_external_data_provider <- function(url)
{
  !startsWith(url, wasserportal_base_url())
}

# stop_on_external_data_provider -----------------------------------------------
stop_on_external_data_provider <- function(url)
{
  if (belongs_to_external_data_provider(url)) {

    kwb.utils::stopFormatted(
      paste0(
        "The master_url '%s' you provided refers to an external ",
        "data provider. Currently only master data within '%s' can be ",
        "requested by using the R package 'wasserportal'"
      ),
      master_url,
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
