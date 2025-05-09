% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{print_cov_plot}
\alias{print_cov_plot}
\title{Prints distribution of a custom covariate}
\usage{
print_cov_plot(data, lo_percentile = 0.025, hi_percentile = 0.975)
}
\arguments{
\item{data}{input dataframe, must contain SEX (Male == 0, Female == 1),
AGE, WT}

\item{lo_percentile}{Default 0.025, (2.5th percentile)}

\item{hi_percentile}{Default 0.975, (97.5th percentile)}
}
\value{
a ggplot object
}
\description{
Prints distribution of a custom covariate
}
