% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{compare_dist_histogram}
\alias{compare_dist_histogram}
\title{Sanity check plots comparing newly created
distributions vs NONMEM dataset (or databases)}
\usage{
compare_dist_histogram(
  df,
  variable_name,
  variable_label,
  lo_percentile = 0.025,
  hi_percentile = 0.975
)
}
\arguments{
\item{df}{Dataframe of newly created distribution}

\item{variable_name}{Name of variable (string) to use for plotting histograms}

\item{variable_label}{Nice label of variable_name}

\item{lo_percentile}{Default 0.025, (2.5th percentile)}

\item{hi_percentile}{Default 0.975, (97.5th percentile)}
}
\value{
a ggplot object
}
\description{
NOT CURRENTLY USED
}
