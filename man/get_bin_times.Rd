% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_bin_times}
\alias{get_bin_times}
\title{Function that derives bin times of x_axis
group}
\usage{
get_bin_times(dfcol, bin_num = 20, relative_threshold = 0.05)
}
\arguments{
\item{dfcol}{Input dataframe column of x-axis}

\item{bin_num}{Maximum number of bins, integer}

\item{relative_threshold}{relative threshold of lumping bins that are close together}
}
\value{
a vector of unique bin times
}
\description{
Function that derives bin times of x_axis
group
}
