# get_text_response_of_httr_request --------------------------------------------
#' @importFrom httr content http_error POST
get_text_response_of_httr_request <- function(
    url,
    method,
    handle = NULL,
    body = NULL,
    text = paste("Sending", method, "request to", url),
    dbg = FALSE,
    encoding = "Latin1"
)
{
  cat_and_run(
    text,
    dbg = dbg,
    expr = {

      # Post the request to the web server
      response <- if (method == "GET") {
        httr::GET(url, handle = handle)
      } else if (method == "POST") {
        httr::POST(url, body = body, handle = handle)
      } else {
        stop_formatted("Method must be one of 'GET', 'POST'.")
      }

      if (httr::http_error(response)) {

        message("POST request failed. Returning the response object.")
        response

      } else {

        # Read the response of the web server as text
        httr::content(response, as = "text", encoding = encoding)
      }
    }
  )
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

#' Helper function: Base Url of Berlin Wassersportal
#'
#' @return string with base url of Berlin Wasserportal
#' @export
wasserportal_base_url <- function()
{
  "https://wasserportal.berlin.de"
}
