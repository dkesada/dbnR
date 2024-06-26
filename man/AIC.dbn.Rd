% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bnlearn_compat.R
\name{AIC.dbn}
\alias{AIC.dbn}
\title{Calculate the AIC of a dynamic Bayesian network}
\usage{
\method{AIC}{dbn}(object, ..., k)
}
\arguments{
\item{object}{the structure of the network}

\item{...}{additional parameters for the network scoring}

\item{k}{the penalty parameter}
}
\value{
the AIC score of the network
}
\description{
Generic method for calculating the Akaike information criterion (AIC) of a 
"dbn" S3 object given some data. Calls bnlearn's \code{\link{AIC}} underneath.
}
