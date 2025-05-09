% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{check_cov_name}
\alias{check_cov_name}
\title{Function to return a DUMMY name if supplied string is
reserved}
\usage{
check_cov_name(
  orig_name,
  replaced_name = "DUMMY",
  list_of_reserved_strings = c("AGE", "AGEMO", "SEX", "WT", "BMI", "BSA")
)
}
\arguments{
\item{orig_name}{original name}

\item{replaced_name}{replacement name}

\item{list_of_reserved_strings}{unavailable names}
}
\value{
a string
}
\description{
Function to return a DUMMY name if supplied string is
reserved
}
