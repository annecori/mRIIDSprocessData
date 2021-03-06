% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_selection.R
\name{deviance_avg}
\alias{deviance_avg}
\title{Estimated average deviance}
\usage{
deviance_avg(I0, pij, R, si, x)
}
\arguments{
\item{I0}{list of matrices. Each element in the list is the incidence
matrix for a single simulation.}

\item{pij}{list of matrices. Each matrix is the probability of
movement for a single simulation. If the list is shorter than other
lists, it will be recycled.}

\item{R}{list of reproduction vectors. Same as for the other
parameters.}

\item{si}{list of serial intervals.}
}
\value{
Deviance averaged over the simulations.
}
\description{
Deviance averaged over the posterior distribution.
}
\details{
\deqn{\hat{D}_{avg}(y) =
\frac{1}{L}\sum_{l = 1}^{L}{D(y, \theta^{l})}}
}
\references{
Gelman, Andrew, et al. Bayesian data analysis. Chapman
and Hall/CRC, 1995.
}
\author{
Sangeeta Bhatia
}
