% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CountSNR.R
\name{CountSNR}
\alias{CountSNR}
\title{Calculate SNR and plot PCA}
\usage{
CountSNR(
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
Plot PCA and calculate SNR based on the first two principal componants of PCA.
}
\examples{
CountSNR(dt=sample_data,metadata=sample_metadata)

}
