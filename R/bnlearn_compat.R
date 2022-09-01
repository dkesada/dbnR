#' Calculate the AIC of a dynamic Bayesian network
#'
#' Generic method for calculating the Akaike information criterion (AIC) of a 
#' "dbn" S3 object given some data. Calls bnlearn's \code{\link{AIC}} underneath.
#' @param object the structure of the network
#' @param ... additional parameters for the network scoring
#' @param k the penalty parameter
#' @return the prediction results
#' @export
AIC.dbn <- function(object, ..., k){
  NextMethod()
}

#' Calculate the AIC of a dynamic Bayesian network
#'
#' Generic method for calculating the Akaike information criterion (AIC) of a 
#' "dbn.fit" S3 object given some data. Calls bnlearn's \code{\link{AIC}} underneath.
#' @param object the fitted network
#' @param ... additional parameters for the network scoring
#' @param k the penalty parameter
#' @return the prediction results
#' @export
AIC.dbn.fit <- function(object, ..., k){
  NextMethod()
}

#' Calculate the log-likelihood of a dynamic Bayesian network
#'
#' Generic method for calculating the log-likelihood of a 
#' "dbn" S3 object given some data. Calls bnlearn's \code{\link{logLik}} underneath.
#' @param object the structure of the network
#' @param dt the dataset to calculate the score of the network
#' @param ... additional parameters for the network scoring
#' @return the prediction results
#' @export
logLik.dbn <- function(object, dt, ...){
  NextMethod()
}

#' Calculate the log-likelihood of a dynamic Bayesian network
#'
#' Generic method for calculating the log-likelihood of a 
#' "dbn.fit" S3 object given some data. Calls bnlearn's \code{\link{logLik}} underneath.
#' @param object the fitted network
#' @param dt the dataset to calculate the score of the network
#' @param ... additional parameters for the network scoring
#' @return the prediction results
#' @export
logLik.dbn.fit <- function(object, dt, ...){
  NextMethod()
}

#' Check if two network structures are equal to each other
#'
#' Generic method for checking the equality of two
#' "dbn" S3 objects. Calls bnlearn's \code{\link{all.equal}} underneath.
#' @param target "dbn" object
#' @param current the other "dbn" object
#' @param ... additional parameters
#' @return the prediction results
#' @export
all.equal.dbn <- function(target, current, ...){
  NextMethod()
}

#' Check if two fitted networks are equal to each other
#'
#' Generic method for checking the equality of two
#' "dbn.fit" S3 objects. Calls bnlearn's \code{\link{all.equal}} underneath.
#' @param target "dbn.fit" object
#' @param current the other "dbn.fit" object
#' @param ... additional parameters
#' @return the prediction results
#' @export
all.equal.dbn.fit <- function(target, current, ...){
  NextMethod()
}

#' Convert a network structure into a model string 
#'
#' Generic method for converting a "dbn" S3 object into a string.
#' Calls bnlearn's \code{\link{as.character}} underneath.
#' @param x a "dbn" object
#' @param ... additional parameters
#' @return the prediction results
#' @export
as.character.dbn <- function(x, ...){
  NextMethod()
}