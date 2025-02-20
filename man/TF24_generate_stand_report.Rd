% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tf24.R
\name{TF24_generate_stand_report}
\alias{TF24_generate_stand_report}
\title{Generates a report on stand grown with TF24 strategy}
\usage{
TF24_generate_stand_report(
  results,
  output_file = "TF24_report.html",
  overwrite = FALSE,
  target_ages = NA,
  input_file = system.file("reports", "TF24_report.Rmd", package = "plant"),
  quiet = TRUE
)
}
\arguments{
\item{results}{results of runnning \code{run_scm_collect}}

\item{output_file}{name of output file}

\item{overwrite}{logical value to determine whether to overwrite existing report}

\item{target_ages}{Patches ages at which to make plots}

\item{input_file}{report script (.Rmd) file to build study report}

\item{quiet}{An option to suppress printing during rendering from knitr, pandoc command line and others.}
}
\value{
html file of the rendered report located in the specified output folder.
}
\description{
Builds a detailed report on stand grown with TF24 strategy, based on the template Rmd file provided.  The reports are
rendered as html files and saved in the specified output folder.
}
