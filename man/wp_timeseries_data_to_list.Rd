% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wp_timeseries_data_to_list.R
\name{wp_timeseries_data_to_list}
\alias{wp_timeseries_data_to_list}
\title{Wasserportal Time Series Data: download and Import in R List}
\usage{
wp_timeseries_data_to_list(
  overview_list_names,
  target_dir = tempdir(),
  is_zipped = TRUE
)
}
\arguments{
\item{overview_list_names}{names of elements in the list returned by
\code{\link{get_stations}(type = "list")}}

\item{target_dir}{target directory for downloading data (default:
tempdir())}

\item{is_zipped}{are the data to be downloaded zipped (default:
TRUE)}
}
\value{
downloads (zipped) data from wasserportal
}
\description{
Wasserportal Time Series Data: download and Import in R List
}
\examples{
\dontrun{
overview_list_names <- names(wasserportal::get_stations(type = "list"))
wp_timeseries_data_list <- wp_timeseries_data_to_list(overview_list_names)
}
}
