% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{one_hot}
\alias{one_hot}
\title{One hot encoder for natural numbers without the 0.}
\usage{
one_hot(nat)
}
\arguments{
\item{nat}{the natural number to convert}
}
\value{
the converted number
}
\description{
Given a natural number, return the natural number equivalent to its
one-hot encoding. Examples: 3 -> 100 -> 4, 5 -> 10000 -> 16
}
\keyword{internal}
