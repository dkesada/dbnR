#' Calculate the mu vector of means of a Gaussian linear network. Front end
#' of a C++ function
#' 
#' @param fit a bn.fit object
#' @param order a topological ordering of the nodes
#' @return a named numeric vector of the means of each variable
#' @export
calc_mu <- function(fit, node){
  initial_fit_check(fit)
  character_arg_check(node)
  
  return(calc_mu_cpp(fit, node))
}