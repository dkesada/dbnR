is_bnfit <- function(obj){
  return(inherits(obj, "bn.fit"))
}

is_dbnfit <- function(obj){
  return(inherits(obj, "dbn.fit"))
}

is_bn_or_bnfit <- function(obj){
  return(inherits(obj, "bn") || is_bnfit(obj))
}

is_dbn_or_dbnfit <- function(obj){
  return(inherits(obj, "dbn") || is_dbnfit(obj))
}

initial_bn_check <- function(obj){
  if(!is_bn_or_bnfit(obj))
    stop(sprintf("%s must be of class 'bn' or 'bn.fit'.",
                 deparse(substitute(obj))))
}

initial_fit_check <- function(obj){
  if(!is_bnfit(obj))
    stop(sprintf("%s must be of class 'bn.fit'.",
                 deparse(substitute(obj))))
}

initial_dbnfit_check <- function(obj){
  if(!is_dbnfit(obj))
    stop(sprintf("%s must be of class 'dbn.fit'.",
                 deparse(substitute(obj))))
}

initial_dbn_check <- function(obj){
  if(!is_dbn_or_dbnfit(obj))
    stop(sprintf("%s must be of class 'dbn' or 'dbn.fit'.",
                 deparse(substitute(obj))))
}

initial_df_check <- function(obj){
  if(!is.data.frame(obj))
    stop(sprintf("%s must be of class 'data.frame' or 'data.table'.",
                 deparse(substitute(obj))))
}

numeric_arg_check <- function(...){
  invisible(sapply(list(...), function(x){
    if(!is.numeric(x))
      stop(sprintf("%s has to be numeric.", deparse(substitute(x))))
    if(length(x) > 1)
      stop(sprintf("%s can not be a vector.", deparse(substitute(x))))
  }))
}

logical_arg_check <- function(...){
  invisible(sapply(list(...), function(x){
    if(!is.logical(x))
      stop(sprintf("%s has to be logical", deparse(substitute(x))))
    if(length(x) > 1)
      stop(sprintf("%s can not be a vector.", deparse(substitute(x))))
  }))
}

character_arg_check <- function(...){
  invisible(sapply(list(...), function(x){
    if(!is.character(x))
      stop(sprintf("%s has to be of type character.", deparse(substitute(x))))
  }))
}

null_or_character_arg_check <- function(...){
  if(!is.null(...)){
    character_arg_check(...)
  }
}

numeric_vector_check <- function(obj){
  if(!is.numeric(obj))
    stop(sprintf("%s has to be numeric.", deparse(substitute(obj))))
}

initial_size_check <- function(size){
  numeric_arg_check(size)
  if(size < 2)
    stop("the size of the DBN cannot be lesser than 2.")
}

initial_dt_vs_size_check <- function(dt, size){
  if(size > dim(dt)[1])
    stop("the size is bigger than the number of rows in the data.table")
}

initial_slice_check <- function(slice){
  numeric_arg_check(slice)
  if(slice < 1)
    stop("the slice cannot be lesser than 1.")
}

initial_size_slice_check <- function(size, slice){
  initial_size_check(size)
  initial_slice_check(slice)
}

#' Checks if the vector of names are time formatted to t0
#'
#' This will check if the names are properly time formatted in t_0
#' to be folded into more time slices. A vector is well formatted
#' in t_0 when all of its column names end in '_t_0'.
#' @param obj the vector of names
#' @return TRUE if it is well formatted. FALSE in other case.
check_time0_formatted <- function(obj){
  return(sum(grepl(".*_t_0$", names(obj))) == length(obj))
}

check_time_formatted <- function(obj){
  return(sum(grepl(".*_t_[0-9]*$", names(obj))) == length(obj))
}

initial_folded_dt_check <- function(obj){
  initial_df_check(obj)
  if(!check_time_formatted(obj))
    stop("the data.frame is not properly time formatted.")
}

check_pkg_available <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("Package %s needed for this function to work.", pkg))
}

check_opt_pkgs_available <- function(){
  pkgs <- optional_packages()
  sapply(pkgs, check_pkg_available)
}

optional_packages <- function(){
  ret <- c("visNetwork",
           "magrittr",
           "grDevices")
  return(ret)
}

check_named_vector <- function(obj){
  numeric_vector_check(obj)
  if(is.null(names(obj)))
    stop(sprintf("%s has to be a named vector.", deparse(substitute(obj))))
}

check_named_symmetric_matrix <- function(obj){
  if(!is.matrix(obj))
    stop(sprintf("%s has to be a matrix.", deparse(substitute(obj))))
  if(any(dim(obj) <= 0))
    stop(sprintf("%s has to be at least a 1x1 matrix.", deparse(substitute(obj))))
  if(!is.numeric(obj[1,1]))
    stop(sprintf("%s has to be a numeric matrix.", deparse(substitute(obj))))
  if(is.null(rownames(obj)))
    stop(sprintf("%s has to be a named matrix.", deparse(substitute(obj))))
  if(is.null(colnames(obj)))
    stop(sprintf("%s has to be a named matrix.", deparse(substitute(obj))))
  if(!isSymmetric.matrix(obj))
    stop(sprintf("%s has to be symmetric, including row and column names.", deparse(substitute(obj))))
}

initial_mu_sigma_check <- function(mu, sigma){
  check_named_vector(mu)
  check_named_symmetric_matrix(sigma)
  
  if(!all(names(mu) %in% colnames(sigma)))
    stop("the mu and sigma names do not match.")
}

check_duplicated_elements <- function(obj){
  if(anyDuplicated(obj) > 0)
    stop(sprintf("duplicated elements in %s.", deparse(substitute(obj))))
}

initial_evidence_check <- function(evidence, variables){
  check_named_vector(evidence)
  check_duplicated_elements(names(evidence))
  
  if(!all(names(evidence) %in% variables))
    stop("some variables of the evidence are not part of the model.")
}

check_empty_net <- function(obj){
  if(dim(bnlearn::arcs(obj))[1] == 0)
    stop(sprintf("all nodes in %s are independent. The resulting net has no arcs.",
                 deparse(substitute(obj))))
}

warn_empty_net <- function(obj){
  ret = FALSE
  if(dim(bnlearn::arcs(obj))[1] == 0){
    warning(sprintf("all nodes in %s are independent. The resulting net has no arcs.\n",
                    deparse(substitute(obj))))
    ret = TRUE
  }
  
  return(ret)
}

modes <- function(){
  ret <- c("exact",
           "approx")
  return(ret)
}

initial_mode_check <- function(obj){
  if(!obj %in% modes())
    stop(paste("unknown mode. Valid modes are:", Reduce(function(acu,x){paste(acu, x, sep = ", ")}, modes())))
}

initial_attr_check <- function(fit){
  if(is.null(attr(fit, "mu")) || is.null(attr(fit, "sigma")))
    fit <- add_attr_to_fit(fit)
  
  return(fit)
}

obj_prov_check <- function(obj_vars, prov_ev){
  if(any(obj_vars %in% prov_ev))
    stop("some objective variables are also provided as evidence.")
}

struc_learning_methods <- function(){
  ret <- c("dmmhc",
           "psoho")
  return(ret)
}

initial_learning_method_check <- function(obj){
  if(!obj %in% struc_learning_methods())
    stop(paste("unknown structure learning method. Valid methods are:", Reduce(function(acu,x){paste(acu, x, sep = ", ")}, struc_learning_methods())))
}

is_causlist <- function(obj){
  return(inherits(obj, "causlist"))
}

initial_causlist_check <- function(obj){
  if(!is_causlist(obj))
    stop(sprintf("%s must be of class 'causlist'.",
                 deparse(substitute(obj))))
}

no_intraslice_check <- function(net){
  idx <- which(grepl("t_0", names(net$nodes)))
  for(i in idx)
    if(length(net$nodes[[i]]$children) > 0)
      stop("DBNs with intraslice arcs are not permitted.")
}

no_parents_check <- function(net){
  idx <- which(!grepl("t_0", names(net$nodes)))
  for(i in idx)
    if(length(net$nodes[[i]]$parents) > 0)
      stop("Only DBNs with no parents in any timeslice other than t_0 are permitted.")
}

initial_dbn_to_causlist_check <- function(obj){
  #initial_dbn_check(obj) --ICO-Merge
  no_parents_check(obj)
  no_intraslice_check(obj)
}

numeric_prob_vector_check <- function(obj, l){
  if(!is.numeric(obj))
    stop(sprintf("%s has to be numeric.", deparse(substitute(obj))))
  if(length(obj) != l)
    stop(sprintf("%s has to of length %s.", deparse(substitute(obj)), l))
  # Not checking for positive numbers. Negative ones are also valid.
}
