#' Calculate the mu vector from a fitted BN or DBN
#' 
#' Given a "bn.fit" or a "dbn.fit" object, calculate the mu vector of 
#' the equivalent multivariate Gaussian distribution. 
#' Front end of a C++ function.
#' 
#' @param fit a bn.fit or dbn.fit object
#' @return a named numeric vector of the means of each variable
#' @examples
#' dt_train <- dbnR::motor[200:2500]
#' net <- bnlearn::mmhc(dt_train)
#' fit <- bnlearn::bn.fit(net, dt_train, method = "mle-g")
#' mu <- dbnR::calc_mu(fit)
#' 
#' f_dt_train <- dbnR::fold_dt(dt_train, size = 2)
#' net <- dbnR::learn_dbn_struc(dt_train, size = 2)
#' fit <- dbnR::fit_dbn_params(net, f_dt_train)
#' mu <- dbnR::calc_mu(fit)
#' @export
calc_mu <- function(fit){
  initial_fit_check(fit)
  
  return(calc_mu_cpp(fit, bnlearn::node.ordering(fit)))
}

#' Calculate the sigma covariance matrix from a fitted BN or DBN
#' 
#' Given a "bn.fit" or a "dbn.fit" object, calculate the sigma covariance 
#' matrix of the equivalent multivariate Gaussian distribution. 
#' Front end of a C++ function.
#' 
#' @param fit a bn.fit or dbn.fit object
#' @return a named numeric covariance matrix of the nodes
#' @examples
#' dt_train <- dbnR::motor[200:2500]
#' net <- bnlearn::mmhc(dt_train)
#' fit <- bnlearn::bn.fit(net, dt_train, method = "mle-g")
#' sigma <- dbnR::calc_sigma(fit)
#' 
#' f_dt_train <- dbnR::fold_dt(dt_train, size = 2)
#' net <- dbnR::learn_dbn_struc(dt_train, size = 2)
#' fit <- dbnR::fit_dbn_params(net, f_dt_train)
#' sigma <- dbnR::calc_sigma(fit)
#' @export
calc_sigma <- function(fit){
  initial_fit_check(fit)
  
  ord <- bnlearn::node.ordering(fit)
  res <- calc_sigma_cpp(fit, ord)
  colnames(res) <- ord
  rownames(res) <- ord
  
  return(res)
}