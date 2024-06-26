% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_daily_surfacewater_data.R
\name{get_daily_surfacewater_data}
\alias{get_daily_surfacewater_data}
\title{Get Daily Surfacewater Data: wrapper to scrape daily surface water data}
\usage{
get_daily_surfacewater_data(
  stations,
  variables = get_surfacewater_variables(),
  list2df = FALSE
)
}
\arguments{
\item{stations}{stations as retrieved by by \code{\link{get_stations}}}

\item{variables}{variables as retrieved by by \code{\link{get_surfacewater_variables}}}

\item{list2df}{convert result list to data frame (default: FALSE)}
}
\value{
list or data frame with all available data from Wasserportal
}
\description{
Get Daily Surfacewater Data: wrapper to scrape daily surface water data
}
\examples{
\dontrun{
stations <- wasserportal::get_stations()
variables <- wasserportal::get_surfacewater_variables()
variables
sw_data_daily <- wasserportal::get_daily_surfacewater_data(stations, variables)
}
}
