% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{categorize_xvar}
\alias{categorize_xvar}
\title{Function that categorizes X-axis variable according to the quantile df}
\usage{
categorize_xvar(df, quantiles_df, xvar)
}
\arguments{
\item{df}{Name of dataframe containing columns corresponding to number of quantiles}

\item{quantiles_df}{Name of dataframe containing the quantiles limits}

\item{xvar}{Name of x-axis variable to split by}
}
\value{
a character string containing the quantile limits
}
\description{
Function that categorizes X-axis variable according to the quantile df
}
