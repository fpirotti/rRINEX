% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getBaseStationFiles.R
\name{timestamp2parts}
\alias{timestamp2parts}
\title{timestamp2parts}
\usage{
timestamp2parts(timestamp)
}
\arguments{
\item{timestamp}{date and time to decompose.}
}
\value{
list with parts useful to replace variables in URL (loosely adopted from URL_LIST.txt of RTKLIB)
\itemize{
\item Y -> yyyy    : year (4 digits) (2000-2099)
\item y -> yy      : year (2 digits) (00-99)
\item m -> mm      : month           (01-12)
\item d -> dd      : day of month    (01-31)
\item h -> hh      : hours           (00-23)
\item H -> a       : hour code       (a-x)
\item M -> mm      : minutes         (00-59)
\item n -> ddd     : day of year  -julianday    (001-366)
\item W -> wwww    : gps week        (0001-9999)
\item D -> d       : day of gps week (0-6)
}
}
\description{
timestamp2parts
}
\examples{
timestamp2parts( (Sys.time() - 14*60*60*24) )
}
