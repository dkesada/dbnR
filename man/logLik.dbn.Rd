% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bnlearn_compat.R
\name{logLik.dbn}
\alias{logLik.dbn}
\title{Calculate the log-likelihood of a dynamic Bayesian network}
\usage{
\method{logLik}{dbn}(object, dt, ...)
}
\arguments{
\item{object}{the structure of the network}

\item{dt}{the dataset to calculate the score of the network}

\item{...}{additional parameters for the network scoring}
}
\value{
the log-likelihood score of the network
}
\description{
Generic method for calculating the log-likelihood of a 
"dbn" S3 object given some data. Calls bnlearn's \code{\link{logLik}} underneath.
}
