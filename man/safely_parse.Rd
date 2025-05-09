% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{safely_parse}
\alias{safely_parse}
\title{purrr:safely wrappers for various functions}
\usage{
safely_parse(...)
}
\arguments{
\item{...}{args to be passed}
}
\value{
A list with two elements:
\itemize{
\item \code{result}: The result of \code{parse}, or \code{NULL} if an error occurred.
\item \code{error}: The error that occurred, or \code{NULL} if no error occurred.
}
}
\description{
purrr:safely wrappers for various functions
}
\seealso{
\code{parse}
}
