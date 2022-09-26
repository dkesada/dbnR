#' Learns the structure of a markovian n DBN model from data
#'
#' Learns a gaussian dynamic Bayesian network from a
#' dataset. It allows the creation of markovian n nets rather than only markov 1.
#' @param dt the data.frame or data.table to be used
#' @param size number of time slices of the net. Markovian 1 would be size 2
#' @param method the structure learning method of choice to use
#' @param f_dt previously folded dataset, in case some specific rows have to be removed after the folding
#' @param ... additional parameters for \code{\link{rsmax2}} function
#' @return the structure of the net
#' @import data.table
#' @examples
#' data("motor")
#' net <- learn_dbn_struc(motor, size = 3)
#' @export
learn_dbn_struc <- function(dt, size = 2, method = "dmmhc", f_dt = NULL, ...){
  initial_size_check(size)
  initial_learning_method_check(method)
  initial_null_dt_check(dt, f_dt)
  if(!is.null(dt)){
    initial_df_check(dt)
    if(!is.data.table(dt))
      dt <- as.data.table(dt)
  }
  if(!is.null(f_dt))
    initial_folded_dt_check(f_dt)
  
  net <- do.call(method, list(dt = dt, size = size, f_dt = f_dt, ...))
  attr(net, "size") <- size
  
  return(net)
}

#' Adds the mu vector and sigma matrix as attributes to the bn.fit or dbn.fit object
#'
#' Adds the mu vector and sigma matrix as attributes to the bn.fit or dbn.fit 
#' object to allow performing exact MVN inference on both cases. It also adds
#' the number of time slices of the net for future inference.
#' @param fit a fitted bn or dbn
#' @param size number of time slices of the net
#' @return the fitted net with attributes
#' @keywords internal
add_attr_to_fit <- function(fit, size){
  initial_fit_check(fit)
  
  attr(fit, "mu") <- calc_mu(fit)
  attr(fit, "sigma") <- calc_sigma(fit)
  attr(fit, "size") <- size
 
  return(fit)
}

#' Fits a markovian n DBN model
#'
#' Fits the parameters of the DBN via MLE. The "mu" vector of means 
#' and the "sigma" covariance matrix are set as attributes of the dbn.fit 
#' object for future exact inference. 
#' @param net the structure of the DBN
#' @param f_dt a folded data.table
#' @param ... additional parameters for the \code{\link{bn.fit}} function
#' @return the fitted net
#' @examples
#' size = 3
#' dt_train <- dbnR::motor[200:2500]
#' net <- learn_dbn_struc(dt_train, size)
#' f_dt_train <- fold_dt(dt_train, size)
#' fit <- fit_dbn_params(net, f_dt_train, method = "mle-g")
#' @export
fit_dbn_params <- function(net, f_dt, ...){
  initial_folded_dt_check(f_dt)
  initial_dbn_check(net)

  fit <- bnlearn::bn.fit(net, f_dt, ...)
  class(fit)[grep("dbn", class(fit))] <- "dbn.fit"
  
  fit <- add_attr_to_fit(fit, attr(net, "size"))
  
  return(fit)
}
