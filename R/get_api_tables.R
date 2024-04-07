# get_api_tables -----------------------------------------------------------

#' Provide Tables of Wasserportal API Documentation
#'
#' The tables that appear in the API documentation of the wasserportal
#' (https://wasserportal.berlin.de/download/wasserportal_berlin_getting_data.pdf)
#' have been added to the wasserportal package. This function returns a list
#' of data frames with each element representing on of these tables.
#'
#' @param name of element from the list of data frames to be selected. If this
#'   argument is left blank (name = NULL), the default, the list of data frames
#'   is returned.
#' @return
#'   list of data frames or data frame specified by the \code{name} argument
#' @export
#' @examples
#' get_api_tables()
#'
get_api_tables <- function(name = NULL)
{
  files <- dir(
    system.file("extdata/api", package = "wasserportal"),
    "\\.csv$",
    full.names = TRUE
  )

  x <- files %>%
    lapply(function(file) {
      read.table(file, sep = ",", header = TRUE, na.strings = "")
    }) %>%
    stats::setNames(
      basename(files) %>%
        gsub(pattern = "^api_table_\\d+_", replacement = "") %>%
        remove_extension()
    )

  if (is.null(name)) {
    return(x)
  }

  select_elements(elements = name, x)
}
