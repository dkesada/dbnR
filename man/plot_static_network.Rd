% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbn_visualization.R
\name{plot_static_network}
\alias{plot_static_network}
\title{Plots a Bayesian network in a hierarchical way}
\usage{
plot_static_network(structure)
}
\arguments{
\item{structure}{the structure or fit of the network.}
}
\description{
This function calculates the levels of each node and then plots them in a hierarchical
layout in visNetwork. Can be used in place of the generic plot function 
offered by bnlearn for "bn" and "bn.fit" S3 objects.
}
\examples{
\donttest{
dt_train <- dbnR::motor[200:2500]
net <- bnlearn::mmhc(dt_train)
plot_static_network(net)
fit <- bnlearn::bn.fit(net, dt_train, method = "mle-g")
plot_static_network(fit) # Works for both the structure and the fitted net
}
}
