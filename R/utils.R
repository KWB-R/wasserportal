# all_are_identical ------------------------------------------------------------
#' @importFrom kwb.utils allAreIdentical
all_are_identical <- kwb.utils::allAreIdentical

# as_date_de -------------------------------------------------------------------
as_date_de <- function(x)
{
  as.Date(x, format = "%d.%m.%Y")
}

# assert_date ------------------------------------------------------------------
assert_date <- function(x)
{
  if (inherits(x, "Date")) {
    return(x)
  }

  result <- try(as.Date(x, origin = "1970-01-01"), silent = TRUE)

  if (is_try_error(result)) {
    stop(call. = FALSE, sprintf(
      "%s cannot be converted to a Date object: %s",
      deparse(substitute(x)),
      as.character(result)
    ))
  }

  result
}

# cat_and_run ------------------------------------------------------------------
#' @importFrom kwb.utils catAndRun
cat_and_run <- kwb.utils::catAndRun
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

# create_accessor --------------------------------------------------------------
#' @importFrom kwb.utils createAccessor
create_accessor <- kwb.utils::createAccessor

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
#' @importFrom utils read.table
#'
read <- function(text, ...) {

  result <- try(silent = TRUE, utils::read.table(
    text = text, sep = ";", dec = ",", stringsAsFactors = FALSE, ...
  ))

  if (is_try_error(result)) {
    return(NULL)
  }

  result
}

# default_if_na ----------------------------------------------------------------
#' @importFrom kwb.utils defaultIfNA
default_if_na <- kwb.utils::defaultIfNA

# default_if_null --------------------------------------------------------------
#' @importFrom kwb.utils defaultIfNULL
default_if_null <- kwb.utils::defaultIfNULL

# first_element ----------------------------------------------------------------
#' @importFrom kwb.utils firstElement
first_element <- kwb.utils::firstElement

# get_attribute ----------------------------------------------------------------
#' @importFrom kwb.utils getAttribute
get_attribute <- kwb.utils::getAttribute

# insert_columns ---------------------------------------------------------------
#' @importFrom kwb.utils insertColumns
insert_columns <- kwb.utils::insertColumns

# is_na_or_empty ---------------------------------------------------------------
#' @importFrom kwb.utils isNaOrEmpty
is_na_or_empty <- kwb.utils::isNaOrEmpty

# is_try_error -----------------------------------------------------------------
#' @importFrom kwb.utils isTryError
is_try_error <- kwb.utils::isTryError

# merge_all --------------------------------------------------------------------
#' @importFrom kwb.utils mergeAll
merge_all <- kwb.utils::mergeAll

# move_columns_to_front --------------------------------------------------------
#' @importFrom kwb.utils moveColumnsToFront
move_columns_to_front <- kwb.utils::moveColumnsToFront

# multi_substitute -------------------------------------------------------------
#' @importFrom kwb.utils multiSubstitute
multi_substitute <- kwb.utils::multiSubstitute

# parallel_non_na --------------------------------------------------------------
#' @importFrom kwb.utils parallelNonNA
parallel_non_na <- kwb.utils::parallelNonNA

# print_if ---------------------------------------------------------------------
#' @importFrom kwb.utils printIf
print_if <- kwb.utils::printIf

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

# remove_columns ---------------------------------------------------------------
#' @importFrom kwb.utils removeColumns
remove_columns <- kwb.utils::removeColumns

# remove_empty_columns ---------------------------------------------------------
#' @importFrom kwb.utils removeEmptyColumns
remove_empty_columns <- kwb.utils::removeEmptyColumns

# rename_columns ---------------------------------------------------------------
#' @importFrom kwb.utils renameColumns
rename_columns <- kwb.utils::renameColumns

# select_columns ---------------------------------------------------------------
#' @importFrom kwb.utils selectColumns
select_columns <- kwb.utils::selectColumns

# select_elements --------------------------------------------------------------
#' @importFrom kwb.utils selectElements
select_elements <- kwb.utils::selectElements

# split_into_lines -------------------------------------------------------------
split_into_lines <- function(x)
{
  stopifnot(is.character(x), length(x) == 1L)

  strsplit(x, "\n")[[1L]]
}

# stop_formatted ---------------------------------------------------------------
#' @importFrom kwb.utils stopFormatted
stop_formatted <- kwb.utils::stopFormatted

# stop_if_not_all_in -----------------------------------------------------------
stop_if_not_all_in <- function(x, set, type = "element")
{
  is_missing <- !(x %in% set)

  if (any(is_missing)) {
    stop_formatted(kwb.utils::noSuchElements(
      x = x[is_missing],
      available = set,
      type = type
    ))
  }
}

# string_list ------------------------------------------------------------------
#' @importFrom kwb.utils stringList
string_list <- kwb.utils::stringList

# subst_special_chars ----------------------------------------------------------
#' @importFrom kwb.utils substSpecialChars
subst_special_chars <- kwb.utils::substSpecialChars

# to_lookup_list ---------------------------------------------------------------
#' @importFrom kwb.utils toLookupList
to_lookup_list <- kwb.utils::toLookupList

# url_parameter_string ---------------------------------------------------------
url_parameter_string <- function(...)
{
  arguments <- list(...)

  stopifnot(!any(kwb.utils::is.unnamed(arguments)))

  paste(names(arguments), arguments, sep = "=", collapse = "&")
}

