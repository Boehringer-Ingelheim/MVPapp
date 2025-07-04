% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{calculate_tick_size}
\alias{calculate_tick_size}
\title{Calculates a nice tick size for tornado plots}
\usage{
calculate_tick_size(abs_y, max_ticks = 12, nice_values = c(1, 2, 5, 10))
}
\arguments{
\item{abs_y}{Range of y-axis values (absolute value)}

\item{max_ticks}{Maximum number of ticks}

\item{nice_values}{Ticks should be multiples of these values}
}
\value{
a numeric of optimal tick size
}
\description{
Calculates a nice tick size for tornado plots
}
