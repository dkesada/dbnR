#' Calculate the AIC of a dynamic Bayesian network
#'
#' Generic method for calculating the Akaike information criterion (AIC) of a 
#' "dbn" S3 object given some data. Calls bnlearn's \code{\link{AIC}} underneath.
#' @param object the structure of the network
#' @param ... additional parameters for the network scoring
#' @param k the penalty parameter
#' @return the AIC score of the network
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
#' @return the AIC score of the network
#' @export
AIC.dbn.fit <- function(object, ..., k){
  NextMethod()
}

#' Calculate the BIC of a dynamic Bayesian network
#'
#' Generic method for calculating the Bayesian information criterion (BIC) of a 
#' "dbn" S3 object given some data. Calls bnlearn's \code{\link{BIC}} underneath.
#' @param object the structure of the network
#' @param ... additional parameters for the network scoring
#' @return the BIC score of the network
#' @importFrom stats BIC
#' @export
BIC.dbn <- function(object, ...){
  NextMethod()
}

#' Calculate the BIC of a dynamic Bayesian network
#'
#' Generic method for calculating the Bayesian information criterion (BIC) of a 
#' "dbn.fit" S3 object given some data. Calls bnlearn's \code{\link{BIC}} underneath.
#' @param object the fitted network
#' @param ... additional parameters for the network scoring
#' @return the BIC score of the network
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
#' @return the log-likelihood score of the network
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
#' @return the log-likelihood score of the network
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
#' @return boolean result of the comparison
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
#' @return boolean result of the comparison
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
#' @return string representing the DBN model
#' @export
as.character.dbn <- function(x, ...){
  NextMethod()
}

#' Calculates the degree of a list of nodes
#'
#' Generic method for calculating the degree of a list of nodes in a
#' "bn" S3 object. Calls bnlearn's \code{\link{degree}} underneath.
#' @param object a "dbn" object
#' @param Nodes which nodes to check
#' @param ... additional parameters
#' @return the degree of the nodes
#' @keywords internal
#' @export
degree.bn <- function(object, Nodes, ...){
  bnlearn::degree(object, Nodes, ...)
}

#' Calculates the degree of a list of nodes
#'
#' Generic method for calculating the degree of a list of nodes in a
#' "dbn" S3 object. Calls bnlearn's \code{\link{degree}} underneath.
#' The degree function is specifically coded to look for either the "bn"
#' or the "bn.fit" method inside bnlearn, so I have to cast the "dbn" class
#' into "bn" for it to work.
#' @param object a "dbn" object
#' @param Nodes which nodes to check
#' @param ... additional parameters
#' @return the degree of the nodes
#' @keywords internal
#' @export
degree.dbn <- function(object, Nodes, ...){
  class(object) <- "bn" # This won't be reflected outside the call
  bnlearn::degree(object, Nodes, ...)
}

#' Calculates the degree of a list of nodes
#'
#' #' Generic method for calculating the degree of a list of nodes in a
#' "bn.fit" S3 object. Calls bnlearn's \code{\link{degree}} underneath.
#' @param object a "dbn" object
#' @param Nodes which nodes to check
#' @param ... additional parameters
#' @return the degree of the nodes
#' @keywords internal
#' @export
degree.bn.fit <- function(object, Nodes, ...){
  bnlearn::degree(object, Nodes, ...)
}

#' Calculates the degree of a list of nodes
#'
#' #' Generic method for calculating the degree of a list of nodes in a
#' "dbn.fit" S3 object. Calls bnlearn's \code{\link{degree}} underneath.
#' @param object a "dbn" object
#' @param Nodes which nodes to check
#' @param ... additional parameters
#' @return the degree of the nodes
#' @keywords internal
#' @export
degree.dbn.fit <- function(object, Nodes, ...){
  class(object) <- "bn.fit" # The degree function specifically checks for a bn object. This won't be reflected outside the call
  bnlearn::degree(object, Nodes, ...)
}

#' Calculates the degree of a list of nodes
#'
#' #' Generic method for calculating the degree of a list of nodes in a
#' BN or a DBN. Calls bnlearn's \code{\link{degree}} underneath. 
#' I have to redefine the generic and mask the original for it to work on both 
#' bn and dbn objects without the user having to import bnlearn.
#' @param object a "bn", "dbn", "bn.fit" or "dbn.fit" object
#' @param Nodes which nodes to check
#' @param ... additional parameters
#' @return the degree of the nodes
#' @importFrom bnlearn degree
#' @export
degree <- function(object, Nodes, ...){
  UseMethod("degree")
}

#' Returns a list with the names of the nodes of a BN or a DBN
#'
#' Generic method for obtaining the names of the nodes in "bn" S3 object.
#' Calls bnlearn's \code{\link{nodes}} underneath.
#' @param object a "bn" object
#' @param ... additional parameters
#' @return the names of the nodes
#' @keywords internal
#' @export
nodes.bn <- function(object, ...){
  bnlearn::nodes(object, ...)
}

#' Returns a list with the names of the nodes of a BN or a DBN
#'
#' Generic method for obtaining the names of the nodes in "dbn" S3 object.
#' Calls bnlearn's \code{\link{nodes}} underneath.
#' The nodes function is specifically coded to look for either the "bn"
#' or the "bn.fit" method inside bnlearn, so I have to cast the "dbn" class
#' into "bn" for it to work.
#' @param object a "dbn" object
#' @param ... additional parameters
#' @return the names of the nodes
#' @keywords internal
#' @export
nodes.dbn <- function(object, ...){
  class(object) <- "bn"
  bnlearn::nodes(object, ...)
}

#' Returns a list with the names of the nodes of a BN or a DBN
#'
#' Generic method for obtaining the names of the nodes in "bn.fit" S3 object.
#' Calls bnlearn's \code{\link{nodes}} underneath.
#' @param object a "bn.fit" object
#' @param ... additional parameters
#' @return the names of the nodes
#' @keywords internal
#' @export
nodes.bn.fit <- function(object, ...){
  bnlearn::nodes(object, ...)
}

#' Returns a list with the names of the nodes of a BN or a DBN
#'
#' Generic method for obtaining the names of the nodes in "dbn.fit" S3 object.
#' Calls bnlearn's \code{\link{nodes}} underneath.
#' @param object a "dbn.fit" object
#' @param ... additional parameters
#' @return the names of the nodes
#' @keywords internal
#' @export
nodes.dbn.fit <- function(object, ...){
  class(object) <- "bn.fit"
  bnlearn::nodes(object, ...)
}

#' Returns a list with the names of the nodes of a BN or a DBN
#'
#' Generic method for obtaining the names of the nodes in a BN or a DBN.
#' Calls bnlearn's \code{\link{nodes}} underneath. 
#' I have to redefine the generic and mask the original for it to work on both 
#' bn and dbn objects without the user having to import bnlearn.
#' @param object a "bn", "dbn", "bn.fit" or "dbn.fit" object
#' @param ... additional parameters
#' @return the names of the nodes
#' @importFrom bnlearn nodes
#' @export
nodes <- function(object, ...){
  UseMethod("nodes")
}

#' Relabel the names of the nodes of a BN or a DBN
#'
#' Generic method for renaming the nodes in a "bn" S3 object.
#' Calls bnlearn's \code{\link{nodes<-}} underneath.
#' @param object a "bn" object
#' @param value a list with the new names
#' @return the modified object
#' @keywords internal
#' @export
`nodes<-.bn` <- function(object, value){
  bnlearn::`nodes<-`(object, value)
}

#' Relabel the names of the nodes of a BN or a DBN
#'
#' Generic method for renaming the nodes in a "bn" S3 object.
#' Calls bnlearn's \code{\link{nodes<-}} underneath.
#' @param object a "dbn" object
#' @param value a list with the new names
#' @return the modified object
#' @keywords internal
#' @export
`nodes<-.dbn` <- function(object, value){
  prev <- class(object)
  class(object) <- "bn"
  object <- bnlearn::`nodes<-`(object, value)
  class(object) <- prev
  
  return(object)
}

#' Relabel the names of the nodes of a BN or a DBN
#'
#' Generic method for renaming the nodes in a "bn.fit" S3 object.
#' Calls bnlearn's \code{\link{nodes<-}} underneath.
#' @param object a "bn.fit" object
#' @param value a list with the new names
#' @return the modified object
#' @keywords internal
#' @export
`nodes<-.bn.fit` <- function(object, value){
  bnlearn::`nodes<-`(object, value)
}

#' Relabel the names of the nodes of a BN or a DBN
#'
#' Generic method for renaming the nodes in a "bn" S3 object.
#' Calls bnlearn's \code{\link{nodes<-}} underneath.
#' @param object a "dbn" object
#' @param value a list with the new names
#' @return the modified object
#' @keywords internal
#' @export
`nodes<-.dbn.fit` <- function(object, value){
  prev <- class(object)
  class(object) <- "bn.fit"
  object <- bnlearn::`nodes<-`(object, value)
  class(object) <- prev
  
  return(object)
}


#' Relabel the names of the nodes of a BN or a DBN
#'
#' Generic method for renaming the nodes in a BN or a DBN.
#' Calls bnlearn's \code{\link{nodes<-}} underneath. 
#' I have to redefine the generic and mask the original for it to work on both 
#' bn and dbn objects without the user having to import bnlearn.
#' @param object a "bn", "dbn", "bn.fit" or "dbn.fit" object
#' @param value a list with the new names
#' @return the modified object
#' @importFrom bnlearn `nodes<-`
#' @export
`nodes<-` <- function(object, value){
  UseMethod("nodes<-")
}

#' Computes the score of a BN or a DBN
#'
#' Generic method for computing the score of a "bn" S3 object.
#' Calls bnlearn's \code{\link{score}} underneath.
#' @param object a "bn" object
#' @param ... additional parameters
#' @return the score of the network
#' @keywords internal
#' @export
score.bn <- function(object, ...){
  bnlearn::score(object, ...)
}

#' Computes the score of a BN or a DBN
#'
#' Generic method for computing the score of a "dbn" S3 object.
#' Calls bnlearn's \code{\link{score}} underneath.
#' The nodes function is specifically coded to look for either the "bn"
#' or the "bn.fit" method inside bnlearn, so I have to cast the "dbn" class
#' into "bn" for it to work.
#' @param object a "dbn" object
#' @param ... additional parameters
#' @return the score of the network
#' @keywords internal
#' @export
score.dbn <- function(object, ...){
  class(object) <- "bn"
  bnlearn::score(object, ...)
}

#' Computes the score of a BN or a DBN
#'
#' Generic method for computing the score of a BN or a DBN.
#' Calls bnlearn's \code{\link{nodes}} underneath. 
#' I have to redefine the generic and mask the original for it to work on both 
#' bn and dbn objects without the user having to import bnlearn.
#' @param object a "bn" or "dbn" object
#' @param ... additional parameters
#' @return the score of the network
#' @importFrom bnlearn score
#' @export
score <- function(object, ...){
  UseMethod("score")
}

#' Print method for "dbn" objects
#'
#' Generic print method for "dbn" S3 objects. Calls bnlearn's print underneath
#' @param x the "dbn" object
#' @param ... additional parameters
#' @export
print.dbn <- function(x, ...){
  NextMethod()
}

#' Print method for "dbn.fit" objects
#'
#' Generic print method for "dbn.fit" S3 objects. Calls bnlearn's print underneath
#' @param x the "dbn.fit" object
#' @param ... additional parameters
#' @export
print.dbn.fit <- function(x, ...){
  NextMethod()
}

#' Average the parameters of multiple dbn.fit objects with identical structures
#'
#' Generic method for "dbn.fit" S3 objects. 
#' Calls bnlearn underneath.
#' @param x the fitted network
#' @param ... additional parameters
#' @return the averaged parameters
#' @export
mean.dbn.fit <- function(x, ...){
  NextMethod()
}

#' Simulates random samples from a fitted DBN
#'
#' Generic method for "dbn.fit" S3 objects. 
#' Calls bnlearn's \code{\link{rbn}} underneath.
#' @param x the fitted network
#' @param n number of samples
#' @param ... additional parameters
#' @return the sampled dataset
#' @importFrom bnlearn rbn
#' @export
rbn.dbn.fit <- function(x, n, ...){
  NextMethod()
}

#' Returns the residuals from fitting a DBN
#'
#' Generic method for "dbn.fit" S3 objects. 
#' Calls bnlearn underneath.
#' @param object the fitted network
#' @param ... additional parameters
#' @return the residuals of fitting the network
#' @importFrom stats residuals
#' @export
residuals.dbn.fit <- function(object, ...){
  NextMethod()
}

#' Returns the standard deviation of the residuals from fitting a DBN
#'
#' Generic method for "dbn.fit" S3 objects. 
#' Calls bnlearn underneath.
#' @param object the fitted network
#' @param ... additional parameters
#' @return the standard deviation residuals of fitting the network
#' @importFrom stats sigma
#' @export
sigma.dbn.fit <- function(object, ...){
  NextMethod()
}

#' Extracts the coefficients of a DBN
#'
#' Generic method for "dbn.fit" S3 objects. 
#' Calls bnlearn underneath.
#' @param object the fitted network
#' @param ... additional parameters
#' @return the coefficients of the network
#' @importFrom stats coef
#' @export
coef.dbn.fit <- function(object, ...){
  NextMethod()
}

#' Extracts the fitted values of a DBN
#'
#' Generic method for "dbn.fit" S3 objects. 
#' Calls bnlearn underneath.
#' @param object the fitted network
#' @param ... additional parameters
#' @return the fitted values of the network
#' @importFrom stats fitted
#' @export
fitted.dbn.fit <- function(object, ...){
  NextMethod()
}

#' Replacement function for parameters inside DBNs
#'
#' Generic parameter replacement method for "dbn.fit" S3 objects. 
#' Calls bnlearn underneath.
#' @param x the fitted network
#' @param name name of the node to replace its parameters
#' @param value the new parameters
#' @return the modified network
#' @export
`$<-.dbn.fit` <- function(x, name, value){
  NextMethod()
}

#' Replacement function for parameters inside DBNs
#'
#' Generic parameter replacement method for "dbn.fit" S3 objects. 
#' Calls bnlearn underneath.
#' @param x the fitted network
#' @param name name of the node to replace its parameters
#' @param value the new parameters
#' @return the modified network
#' @export
`[[<-.dbn.fit` <- function(x, name, value){
  NextMethod()
}