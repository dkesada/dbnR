#' Calculate the AIC of a dynamic Bayesian network
#'
#' Generic method for calculating the Akaike information criterion (AIC) of a 
#' "dbn" S3 object given some data. Calls \code{\link{AIC}} underneath.
#' @param object the structure of the network.
#' @param data the dataset to calculate the score of the network
#' @param ... additional parameters for the network scoring
#' @export
AIC.dbn <- function(object, data, ...){
  UseMethod("AIC")
}