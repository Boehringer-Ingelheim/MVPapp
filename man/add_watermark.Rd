% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{add_watermark}
\alias{add_watermark}
\title{Function to add a watermark to a ggplot}
\usage{
add_watermark(
  watermark_toggle = TRUE,
  lab = "For Internal Use Only",
  col = "grey",
  alpha = 0.7,
  fontface = "plain",
  rot = 0,
  width = 0.6,
  pos = c(0.5, 0.5),
  align = "centre"
)
}
\arguments{
\item{watermark_toggle}{Set to TRUE to insert a watermark layer for ggplot}

\item{lab}{Text to be displayed}

\item{col}{Color of watermark text}

\item{alpha}{Text transparency}

\item{fontface}{"plain" | "bold" | "italic" | "bold.italic"}

\item{rot}{rotation (0,360), NA = from lower left to upper right corner}

\item{width}{text width relative to plot}

\item{pos}{x- and y-position relative to plot (vector of length 2)}

\item{align}{"left" | "right" | "center" | "centre" | "top" | "bottom",
can also be given as vector of length 2, first horizontal, then vertical}
}
\description{
The function is based on the example found here:
https://www.r-bloggers.com/2012/05/adding-watermarks-to-plots/.
}
