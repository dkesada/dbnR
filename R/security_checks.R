is_bnfit <- function(obj){
  return(is(obj, "bn.fit"))
}

is_bn_or_bnfit <- function(obj){
  return(is(obj, "bn") || is_bnfit(obj))
}

is_dbn_or_dbnfit <- function(obj){
  return(is(obj, "dbn") || is(obj, "dbn.fit"))
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

numeric_arg_check <- function(obj){
  if(!is.numeric(obj))
    stop(sprintf("%s has to be numeric.", deparse(substitute(obj))))
  if(length(obj) > 1)
    stop(sprintf("%s can not be a vector.", deparse(substitute(obj))))
}

initial_size_check <- function(size){
  numeric_arg_check(size)
  if(size < 2)
    stop("the size of the DBN cannot be lesser than 2.")
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
  return(sum(grepl(".*_t_0$", obj)) == length(obj))
}

check_time_formatted <- function(obj){
  return(sum(grepl(".*_t_[0-9]*$", obj)) == length(obj))
}

initial_folded_dt_check <- function(obj){
  initial_df_check(obj)
  if(!check_time_formatted(names(obj)))
    stop("the data.frame is not properly time formatted.")
}

check_pkg_available <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("Package %s needed for this function to work."), pkg)
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
