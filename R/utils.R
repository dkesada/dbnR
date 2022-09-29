mae <- function(orig, pred){
  return(sum(abs(orig - pred))/length(orig))
}

mae_by_col <- function(dt, col){
  return(mae(unlist(dt[,.SD, .SDcols = names(col)]), col))
}

#' @importFrom stats "sd"
sd_error <- function(orig, pred){
  return(sd((orig - pred)))
}

print_metrics <- function(metrics, obj_vars){
  cat("The average MAE per execution is:", fill = T)
  sapply(obj_vars, function(x){cat(paste0(x, ": ", round(metrics[x], 4)),
                                   fill = T)})
}

plot_single_result <- function(dt, results, var){
  min_val <- min(c(dt[, get(var)], results[, get(var)]))
  max_val <- max(c(dt[, get(var)], results[, get(var)]))
  plot(ts(dt[, get(var)]), ylim = c(min_val, max_val), ylab = var)
  idx <- "exec"
  for(i in results[, unique(get(idx))])
    lines(results[eval(idx) == i, get(var)], col = "red")
}

plot_results <- function(dt, results, obj_vars){
  invisible(sapply(obj_vars, function(x){plot_single_result(dt, results, x)}))
}

#' Converts a single row data.table into a named vector
#' 
#' Given a single row data.table, convert it into a named vector. Used in the
#' mvn_inference, to obtain an evidence named vector. For this case, the 
#' data.table should contain only the evidence that we want to provide. If a
#' named vector is provided instead of a data.table, nothing will be done and
#' the named vector will be returned.
#' @param dt a 1 row data.table or a named vector
#' @return a named vector with the values and the variable names
#' @keywords internal
as_named_vector <- function(dt){
  if(is.data.frame(dt)){
    initial_onerow_dt_check(dt)
    res <- as.numeric(dt)
    names(res) <- names(dt)
  }
  else{
    check_named_vector(dt)
    res <- dt
  }
  
  return(res)
}

#' One hot encoder for natural numbers without the 0.
#' 
#' Given a natural number, return the natural number equivalent to its
#' one-hot encoding. Examples: 3 -> 100 -> 4, 5 -> 10000 -> 16
#' 
#' @param nat the natural number to convert
#' @return the converted number
#' @keywords internal
one_hot <- function(nat){
  return(2^(nat-1))
}

#' Geometric distribution sampler truncated to a maximum
#' 
#' A geometric distribution sampler with probability 'p' restricted to values
#' inside [1, max]. Because of this restriction, very low values of 'p' 
#' coupled with low 'max' return increasingly uniform populations in 
#' the interval [1, max].
#' 
#' @param p the parameter of the geometric distribution
#' @param max the maximum value allowed to be sampled
#' @return the sampled value
#' @importFrom stats runif
#' @keywords internal
trunc_geom <- function(p, max){
  return(floor(log(1 - runif(1)*(1 - (1 - p)^max)) / log(1 - p)))
}

#' Experimental function that translates a natPosition vector into a DBN network.
#' 
#' @param ps a position vector of natural numbers
#' @param ordering_raw the ordering of the variables
#' @param n_arcs the total number of arcs 
#' @param nodes the name of all the nodes in the network
#' @return a bn object
#' @keywords internal
bn_translate_exp = function(ps, ordering_raw, n_arcs, nodes){
  arc_mat <- nat_cl_to_arc_matrix_cpp(ps, ordering_raw, n_arcs)
  
  net <- bnlearn::empty.graph(nodes)
  bnlearn::arcs(net) <- arc_mat
  class(net) <- c("dbn", class(net))
  
  return(net)
}

#' Experimental function that recounts the number of arcs in the position
#' @param ps a position vector of natural numbers
#' @return the number of arcs
#' @keywords internal
recount_arcs_exp = function(ps){
  n_arcs <- 0
  
  for(i in 1:length(ps))
    n_arcs <- n_arcs + bitcount(ps[i])
  
  return(n_arcs)
}

#' Generates the names of the nodes in t_0 and in all the network
#' 
#' Given the names of the desired variables, this function generates the names
#' of the variables in a DBN without needing a previous dataset. It's just a 
#' wrapper around the 'fold_dt' function.
#' @param ordering the names of the variables
#' @param size the desired size of the dbn
#' @return a dictionary with the variable names in t_0 and in all other time slices
#' @keywords internal
nodes_gen_exp <- function(ordering, size){
  res <- list(ordering_t_0 = NULL, nodes = NULL)
  
  res$ordering_t_0 <- sapply(ordering, function(x){paste0(x, "_t_0")}, USE.NAMES = F)
  tmp <- matrix(nrow = size, ncol = length(ordering))
  tmp <- as.data.table(tmp)
  names(tmp) <- ordering
  tmp <- fold_dt(tmp, size)
  res$nodes <- names(tmp)
  
  return(res)
}

#' Generates the names of n variables.
#' 
#' Given the total number of variables, this function generates a character
#' vector with variables named "Xi", where i is a number in the interval [0,n-1]
#' @param n the total number of variables desired
#' @return a character vector with the variable names
#' @keywords internal
ordering_gen_exp <- function(n){
  res <- rep("", n)
  for(i in 1:n)
    res[i] <- paste0("X", i-1)
  
  return(res)
}

#' Generate a random DBN and sample a dataset that defines it
#' 
#' This function generates both a random DBN and a dataset that can be used to 
#' learn its structure from data. It's intended for experimental use.
#' @param n_vars number of desired variables per time-slice
#' @param size desired size of the networks
#' @param min_mu minimum mean allowed for the variables
#' @param max_mu maximum mean allowed for the variables
#' @param min_sd minimum standard deviation allowed for the variables
#' @param max_sd maximum standard deviation allowed for the variables
#' @param min_coef minimum coefficient allowed for the parent nodes
#' @param max_coef maximum coefficient allowed for the parent nodes
#' @param seed the seed of the experiment
#' @return a list with the original network structure and the sampled dataset
#' @import data.table
#' @export
generate_random_network_exp <- function(n_vars, size, min_mu, max_mu,
                                        min_sd, max_sd, min_coef, max_coef, seed = NULL){
  positive_arg_check(n_vars, min_sd, max_sd)
  initial_size_check(size)
  numeric_arg_check(min_mu, max_mu, min_coef, max_coef, seed)
  lesser_than_arg_check(max_mu, min_mu)
  lesser_than_arg_check(max_sd, min_sd)
  lesser_than_arg_check(max_coef, min_coef)
  
  res <- list(net = NULL, f_dt = NULL)
  if(!is.null(seed))
    set.seed(seed)
  
  # First. we generate a random position and translate it into a DBN structure
  
  # Generate the names of the variables in the network
  ordering_raw <- ordering_gen_exp(n_vars)
  nodes_l <- nodes_gen_exp(ordering_raw, size)
  ps <- init_cl_cpp(n_vars * n_vars)
  
  # Generate a random position
  for(i in 1:length(ps))
    ps[i] <- floor(runif(1, 0, 2^(size-1)))
  
  n_arcs <- recount_arcs_exp(ps)
  res$net <- bn_translate_exp(ps, ordering_raw, n_arcs, nodes_l$nodes)
  
  # Second, we generate a dataset that represents the same relationships portrayed by the network
  dt <- as.data.table(matrix(nrow = 10000, ncol = length(nodes_l$nodes)))
  names(dt) <- nodes_l$nodes
  dt[, names(dt) := lapply(.SD, function(x){stats::rnorm(length(x),
                                                         runif(1, min_mu, max_mu), 
                                                         runif(1, min_sd, max_sd))})]
  
  # Apply the effects of the arcs in the dataset
  bn_arcs <- bnlearn::arcs(res$net)
  for(i in 1:n_arcs){
    from <- bn_arcs[i,1]
    to <- bn_arcs[i,2]
    dt[, (to) := get(to) + get(from) * runif(1, min_coef, max_coef)]
  }
  
  res$f_dt <- dt
  
  return(res)
}

# For internal use of the security_check functions. I'm fed up with giving 
# errors without the names of the variables due to substitute + deparse + ellipsis 
# shenanigans
deparse_names <- function(...){
  res <- substitute(list(...))
  res <- sapply(res, deparse)[-1]
  
  return(res)
}

# Calculates the size of a 'dbn.fit' based on its node names. The size is 
# stored automatically after fitting the net, it should only need to be calculated
# when learning a network without using the 'fit_dbn_params' function.
calc_size <- function(fit){
  res <- 0
  
  if(is_dbn_or_dbnfit(fit))
    res <- as.numeric(max(sapply(names(fit), function(x){strsplit(x, "_t_")[[1]][2]}, USE.NAMES = F)))
  
  return(res)
}

# Reverses the naming convention of a vector of nodes. This function transforms 
# t_0 into t_n , t_1 into t_n-1 and so on. 
reverse_names <- function(old_names, size){
  sapply(old_names, function(x){
    elems <- strsplit(x, "_t_")[[1]]
    elems[2] <- abs(size - 1 - as.numeric(elems[2]))
    paste0(elems[1], "_t_", elems[2])
  }, USE.NAMES = F)
}

#' Function that moves the window of values backwards in a folded dataset row
#' 
#' Move the values in t_0, t_1, ..., t_n-1 in a folded dataset row to
#' t_1, t_2, ..., t_n. This is useful to predict the values in the last row
#' of a folded dataset
#' @param f_dt a folded dataset
#' @param row the row that is going to be processed
#' @return the shifted row
#' @export
shift_values <- function(f_dt, row){
  var_names <- names(f_dt)
  max_size <- max(simplify2array(strsplit(var_names, "^.*_t_")))
  vars_first_idx <- grep("t_0", var_names)
  vars_last_idx <- grep(paste0("t_", max_size), var_names)
  vars_t0 <- var_names[vars_first_idx]
  vars_first <- var_names[-vars_last_idx]
  vars_last <- var_names[-vars_first_idx]
  res = copy(f_dt[row])
  
  res[, (vars_last) := .SD, .SDcols = vars_first]
  res[, (vars_t0) := NA]
  
  return(res)
}
