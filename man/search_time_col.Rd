% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_v_0_2_14.R
\name{search_time_col}
\alias{search_time_col}
\title{Search for Likely TIME Columns}
\usage{
search_time_col(
  orig_df,
  names_of_time_cols = c("TAFD", "TSFD", "ATFD", "ATSD")
)
}
\arguments{
\item{orig_df}{The dataframe used for searching}

\item{names_of_time_cols}{Likely column names, in order of search priority}
}
\value{
a dataframe
}
\description{
Searches through several common column names that could be used to create the
"TIME" column in the dataset
}
