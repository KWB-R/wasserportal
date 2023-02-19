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

#' Helper function: Base Url of Berlin Wassersportal
#'
#' @return string with base url of Berlin Wasserportal
#' @export
wasserportal_base_url <- function()
{
  "https://wasserportal.berlin.de"
}
