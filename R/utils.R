# assert_date ------------------------------------------------------------------
assert_date <- function(x)
{
  if (! inherits(x, "Date")) {

    x <- try(as.Date(x))

    if (inherits(x, "try-error")) {
      stop(call. = FALSE, sprintf(
        "%s cannot be converted to a Date object!", deparse(substitute(x))
      ))
    }
  }

  x
}


#' Helper function to read CSV
#'
#' @param text text
#' @param ...  \dots additional arguments passed to \code{\link[utils]{read.table}}
#'
#' @return data frame with values
#' @export
#' @importFrom utils read.table
#'
read <- function(text, ...) {

  result <- try(silent = TRUE, utils::read.table(
    text = text, sep = ";", dec = ",", stringsAsFactors = FALSE, ...
  ))

  if (! inherits(result, "try-error")) {
    result
  }
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
    fmt, column, kwb.utils::selectColumns(data, column)
  ))))
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
