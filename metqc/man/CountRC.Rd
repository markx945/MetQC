% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CountRC.R
\name{CountRC}
\alias{CountRC}
\title{Calculate and plot RC}
\usage{
CountRC(
  dt.path = NULL,
  metadata.path = NULL,
  output.path = NULL,
  dt = NULL,
  metadata = NULL
)
}
\arguments{
\item{dt}{Data table}

\item{metadata}{Data table}
}
\value{
Numeric vector
}
\description{
Calculate correlation to reference datasets and plot related scatter plot
}
\examples{
CountRC(dt=sample_data,metadata=sample_metadata)

}
