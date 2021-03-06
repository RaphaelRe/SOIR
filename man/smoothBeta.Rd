% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_betas.R
\name{smoothBeta}
\alias{smoothBeta}
\title{SmoothBeta}
\usage{
smoothBeta(x1, x2, random = FALSE)
}
\arguments{
\item{x1}{sequence between 0 and 1}

\item{x2}{see x1}
}
\value{
smooth image as matrix
}
\description{
Returns a smooth image
}
\examples{
require(magrittr)
grid <- seq(0,1, len = 64)
smoothBeta(grid,grid) \%>\% plot_coefficient_image()
}
