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

# readPackageFile --------------------------------------------------------------

#' Read CSV File from Package's "extdata" Folder
#'
#' @param file file name (without path)
#' @param \dots additional arguments passed to \code{\link[utils]{read.csv}}
#'
#' @return data frame representing the content of \code{\link{file}}
#'
#' @export
#'
readPackageFile <- function(file, ...)
{
  kwb.utils::readPackageFile(file, package = "wasserportal", ...)
}
