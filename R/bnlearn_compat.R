#' Calculate the AIC of a dynamic Bayesian network
#'
#' Generic method for calculating the Akaike information criterion (AIC) of a 
#' "dbn" S3 object given some data. Calls bnlearn's \code{\link{AIC}} underneath.
#' @param object the structure of the network
#' @param ... additional parameters for the network scoring
#' @param k the penalty parameter
#' @return the prediction results
#' @importFrom stats AIC
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

#' Calculate the BIC of a dynamic Bayesian network
#'
#' Generic method for calculating the Akaike information criterion (BIC) of a 
#' "dbn" S3 object given some data. Calls bnlearn's \code{\link{BIC}} underneath.
#' @param object the structure of the network
#' @param ... additional parameters for the network scoring
#' @return the prediction results
#' @importFrom stats BIC
#' @export
BIC.dbn <- function(object, ...){
  NextMethod()
}

#' Calculate the BIC of a dynamic Bayesian network
#'
#' Generic method for calculating the Akaike information criterion (BIC) of a 
#' "dbn.fit" S3 object given some data. Calls bnlearn's \code{\link{BIC}} underneath.
#' @param object the fitted network
#' @param ... additional parameters for the network scoring
#' @return the prediction results
#' @export
BIC.dbn.fit <- function(object, ...){
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
#' @importFrom stats logLik
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

#' Convert a network structure into a model string 
#'
#' Generic method for converting a "dbn" S3 object into a string.
#' Calls bnlearn's \code{\link{degree}} underneath.
#' @param object a "dbn" object
#' @param Nodes which nodes to check
#' @param ... additional parameters
#' @return the prediction results
#' @importMethodsFrom bnlearn degree
#' @method degree bn
#' @export
degree.bn <- function(object, Nodes, ...){
  bnlearn::degree(object, Nodes, ...)
}

#' Convert a network structure into a model string 
#'
#' Generic method for converting a "dbn" S3 object into a string.
#' Calls bnlearn's \code{\link{degree}} underneath.
#' @param object a "dbn" object
#' @param Nodes which nodes to check
#' @param ... additional parameters
#' @return the prediction results
#' @importMethodsFrom bnlearn degree
#' @method degree dbn
#' @export
degree.dbn <- function(object, Nodes, ...){
  class(object) <- "bn" # The degree function specifically checks for a bn object. This won't be reflected outside the call
  bnlearn::degree(object, Nodes, ...)
}

#' Convert a network structure into a model string 
#'
#' Generic method for converting a "dbn" S3 object into a string.
#' Calls bnlearn's \code{\link{degree}} underneath. I have to redefine the
#' generic and mask the original for it to work on both bn and dbn objects.
#' @param object a "dbn" object
#' @param Nodes which nodes to check
#' @param ... additional parameters
#' @return the prediction results
#' @importFrom bnlearn degree
#' @export
degree <- function(object, Nodes, ...){
  UseMethod("degree")
}

