#' Calculate the mu vector of means of a Gaussian linear network. Front end
#' of a C++ function.
#' 
#' @param fit a bn.fit or dbn.fit object
#' @return a named numeric vector of the means of each variable
#' @examples
#' dt_train <- dbnR::motor[200:2500]
#' net <- bnlearn::mmhc(dt_train)
#' fit <- bnlearn::bn.fit(net, dt_train, method = "mle")
#' mu <- calc_mu(fit)
#' @export
calc_mu <- function(fit){
  initial_fit_check(fit)
  
  return(calc_mu_cpp(fit, bnlearn::node.ordering(fit)))
}

#' Calculate the sigma covariance matrix of a Gaussian linear network. 
#' Front end of a C++ function.
#' 
#' @param fit a bn.fit or dbn.fit object
#' @return a numeric covariance matrix of the nodes
#' @examples
#' dt_train <- dbnR::motor[200:2500]
#' net <- bnlearn::mmhc(dt_train)
#' fit <- bnlearn::bn.fit(net, dt_train, method = "mle")
#' sigma <- calc_sigma(fit)
#' @export
calc_sigma <- function(fit){
  initial_fit_check(fit)
  
  ord <- bnlearn::node.ordering(fit)
  res <- calc_sigma_cpp(fit, ord)
  colnames(res) <- ord
  rownames(res) <- ord
  
  return(res)
}