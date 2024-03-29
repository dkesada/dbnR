% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bnlearn_compat.R
\name{nodes.bn}
\alias{nodes.bn}
\title{Returns a list with the names of the nodes of a BN or a DBN}
\usage{
\method{nodes}{bn}(object, ...)
}
\arguments{
\item{object}{a "bn" object}

\item{...}{additional parameters}
}
\value{
the names of the nodes
}
\description{
Generic method for obtaining the names of the nodes in "bn" S3 object.
Calls bnlearn's \code{\link{nodes}} underneath.
}
\keyword{internal}
