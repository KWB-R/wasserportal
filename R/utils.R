# as_date_de -------------------------------------------------------------------
as_date_de <- function(x)
{
  as.Date(x, format = "%d.%m.%Y")
}

# assert_date ------------------------------------------------------------------
#' @importFrom kwb.utils isTryError
assert_date <- function(x)
{
  if (inherits(x, "Date")) {
    return(x)
  }

  result <- try(as.Date(x, origin = "1970-01-01"), silent = TRUE)

  if (kwb.utils::isTryError(result)) {
    stop(call. = FALSE, sprintf(
      "%s cannot be converted to a Date object: %s",
      deparse(substitute(x)),
      as.character(result)
    ))
  }

  result
}

# columns_to_labels ------------------------------------------------------------
#' Create Text Labels from Data Frame Columns
#'
#' @param data data frame
#' @param columns names of columns from which to create labels
#' @param fmt format string passed to \code{\link{sprintf}}
#' @param sep separator (default: ", ")
#' @return vector of character with as many elements as there are rows in data
#' @export
#' @importFrom kwb.utils selectColumns
#' @examples
#' data <- data.frame(number = 1:2, name = c("adam", "eva"), value = 3:4)
#' columns <- c("name", "value")
#' columns_to_labels(data, columns)
#' columns_to_labels(data, columns, fmt = "<p>%s: %s</p>", sep = "")
columns_to_labels <- function(data, columns, fmt = "%s: %s", sep = ", ")
{
  do.call(paste, c(list(sep = sep), lapply(columns, function(column) sprintf(
    fmt, column, select_columns(data, column)
  ))))
}

# date_string_de ---------------------------------------------------------------
date_string_de <- function(x)
{
  format(x, format = "%d.%m.%Y")
}

#' Helper function to read CSV
#'
#' @param text text
#' @param ...  \dots additional arguments passed to \code{\link[utils]{read.table}}
#'
#' @return data frame with values
#' @export
#' @importFrom kwb.utils isTryError
#' @importFrom utils read.table
#'
read <- function(text, ...) {

  result <- try(silent = TRUE, utils::read.table(
    text = text, sep = ";", dec = ",", stringsAsFactors = FALSE, ...
  ))

  if (kwb.utils::isTryError(result)) {
    return(NULL)
  }

  result
}

# readPackageFile --------------------------------------------------------------

#' Read CSV File from Package's "extdata" Folder
#'
#' @param file file name (without path)
#' @param \dots additional arguments passed to \code{\link[utils]{read.csv}}
#'
#' @return data frame representing the content of \code{\link{file}}
#'
#' @export
#' @importFrom kwb.utils readPackageFile
readPackageFile <- function(file, ...)
{
  kwb.utils::readPackageFile(file, package = "wasserportal", ...)
}

# select_columns ---------------------------------------------------------------
#' @importFrom kwb.utils selectColumns
select_columns <- kwb.utils::selectColumns

# select_elements --------------------------------------------------------------
#' @importFrom kwb.utils selectElements
select_elements <- kwb.utils::selectElements

# subst_special_chars ----------------------------------------------------------
#' @importFrom package kwb.utils substSpecialChars
subst_special_chars <- kwb.utils::substSpecialChars
