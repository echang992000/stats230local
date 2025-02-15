% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mvn.R
\name{mvn}
\alias{mvn}
\title{This function takes in a vector mu and a positive definite matrix sigma and returns realizations from the multivariate normal distribution using cholesky decomposition}
\usage{
mvn(mu, sigma, N)
}
\arguments{
\item{mu}{First input, n dimensional vector}

\item{sigma}{Second input, nxn positive definite matrix}

\item{N}{Third input, number of realizations}
}
\value{
Multivariate normal distribution
}
\description{
This function takes in a vector mu and a positive definite matrix sigma and returns realizations from the multivariate normal distribution using cholesky decomposition
}
\examples{
mvn(rnorm(4),Posdef(4))
}
