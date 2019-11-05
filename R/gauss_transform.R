#' Calculate the mu vector of means of a Gaussian linear network. Front end
#' of a C++ function.
#' 
#' @param fit a bn.fit object
#' @return a named numeric vector of the means of each variable
#' @export
calc_mu <- function(fit){
  initial_fit_check(fit)
  
  return(calc_mu_cpp(fit, bnlearn::node.ordering(fit)))
}

#' Calculate the sigma covariance matrix of a Gaussian linear network. 
#' Front end of a C++ function.
#' 
#' @param fit a bn.fit object
#' @return a numeric covariance matrix of the nodes
#' @export
calc_sigma <- function(fit){
  initial_fit_check(fit)
  
  ord <- bnlearn::node.ordering(fit)
  res <- calc_sigma_cpp(fit, ord)
  colnames(res) <- ord
  rownames(res) <- ord
  
  return(res)
}

#' Calculate the mu mean vector and the sigma covariance matrix of a Gaussian 
#' linear network and return a list with the original fit, mu and sigma.
#' 
#' @param fit a bn.fit object
#' @return a list with the bnfit, mu and sigma
#' @export
transform_bnfit <- function(fit){
  initial_fit_check(fit)
  
  return(list(bn = fit, mu = calc_mu(fit), sigma = calc_sigma(fit)))
}