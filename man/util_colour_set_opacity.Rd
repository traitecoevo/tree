% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{util_colour_set_opacity}
\alias{util_colour_set_opacity}
\title{Make colours transparent}
\usage{
util_colour_set_opacity(col, opacity = 0.5)
}
\arguments{
\item{col}{Vector of colours}

\item{opacity}{Vector of opacities}
}
\description{
Make colours transparent
}
\examples{
util_colour_set_opacity("red", seq(0, 1, length.out=6))
util_colour_set_opacity(c("red", "blue"), .5)
}
