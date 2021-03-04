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
