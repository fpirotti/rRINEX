% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common.R
\name{decimalsAfterPrecision}
\alias{decimalsAfterPrecision}
\title{decimalsAfterPrecision}
\usage{
decimalsAfterPrecision(x, n = 0, limit = NA)
}
\arguments{
\item{x}{character with number}

\item{n}{integer number, DEFAULT=0}

\item{limit}{integer - maximum digits, DEFAULT=Inf}
}
\value{
a character string with the decimals after truncation
}
\description{
decimalsAfterPrecision
}
\examples{
decimalsAfterPrecision(pi, 0)
decimalsAfterPrecision(pi, 2)
}
