% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_wasserportal_raw_gw.R
\name{read_wasserportal_raw_gw}
\alias{read_wasserportal_raw_gw}
\title{read_wasserportal_raw_gw}
\usage{
read_wasserportal_raw_gw(
  station = 149,
  stype = "gws",
  type = "single_all",
  from_date = "",
  include_raw_time = FALSE,
  handle = NULL,
  as_text = FALSE,
  dbg = FALSE
)
}
\arguments{
\item{station}{station id}

\item{stype}{"gws" or "gwq"}

\item{type}{"single" or "single_all" (if stype = "gwq")}

\item{from_date}{(default: "")}

\item{include_raw_time}{default: FALSE}

\item{handle}{default: NULL}

\item{as_text}{if TRUE, the raw text that is returned by the HTTP request to
the Wasserportal is returned by this function. Otherwise (the default)
the raw text is tried to be interpreted as comma separated values and a
corresponding data frame is returned. Use as_text = TRUE to analyse the raw
text in case that an error occurs when trying to convert the text to a data
frame.}

\item{dbg}{logical indicating whether or not to show debug messages. The
default is FALSE}
}
\value{
data.frame with values
}
\description{
read_wasserportal_raw_gw
}
\examples{
\dontrun{
read_wasserportal_raw_gw(station = 149, stype = "gws")
read_wasserportal_raw_gw(station = 149, stype = "gwq")
}
}
