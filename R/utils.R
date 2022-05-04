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
  print("The average MAE per execution is:", quote = FALSE)
  sapply(obj_vars, function(x){print(paste0(x, ": ", round(metrics[x], 4)),
                                     quote = FALSE)})
}

plot_single_result <- function(dt, results, var){
  min_val <- min(c(dt[, get(var)], results[, get(var)]))
  max_val <- max(c(dt[, get(var)], results[, get(var)]))
  plot(ts(dt[, get(var)]), ylim = c(min_val, max_val), ylab = var)
  for(i in results[, unique(exec)])
    lines(results[exec == i, get(var)], col = "red")
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
#' @return a vector with the values and the variable names
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

#' Reduce the frequency of the time series data in a data.table
#' 
#' In a time series dataset, there is a time difference between one row and the 
#' next one. This function reduces the number of rows from its current frequency
#' to the desired one. Instead of the frequency in Hz, the number of seconds
#' between rows is asked (Hz = 1/s).
#' @param dt the original data.table
#' @param obj_freq the desired number of seconds between rows
#' @param curr_freq the number of seconds between rows in the original dataset
#' @param id_var optional variable that labels different time series in a dataset, to avoid averaging values from different processes
#' @return the data.table with the desired frequency
#' @import data.table
#' @export
reduce_freq <- function(dt, obj_freq, curr_freq, id_var = NULL){
  initial_df_check(dt)
  if(!is.data.table(dt))
    dt <- as.data.table(dt)
  numeric_arg_check(obj_freq, curr_freq)
  null_or_character_arg_check(id_var)
  
  dt_res <- copy(dt)
  obj_vars <- c("idx", id_var)
  n_rows <- ceiling(obj_freq / curr_freq)
  obj_rows <- seq(1, dim(dt_res)[1], n_rows)
  idx_vec <- as.vector(sapply(obj_rows, function(x, times){rep(x,times)}, times=n_rows))[1:dim(dt_res)[1]]
  dt_res[, "idx" := idx_vec]
  dt_res <- dt_res[, lapply(.SD, mean), by=obj_vars]
  dt_res[, "idx" := NULL]
  
  return(dt_res)
}

#' Filter the instances in a data.table that have values of different ids in each row
#' 
#' Given an id variable that labels the different instances of a time series
#' inside a dataset, discard the rows that have values from more than 1 id.
#' @param f_dt folded data.table
#' @param size the size of the data.table
#' @param id_var the variable that labels each individual instance of the time series
#' @return the filtered dataset
#' @export
filter_same_cycle <- function(f_dt, size, id_var){
  initial_folded_dt_check(f_dt)
  initial_size_check(size)
  character_arg_check(id_var)
  
  cond <- Reduce(function(acu, x){paste0(acu, " & ", id_var, "_t_0 == ", id_var, "_t_", x)}, 
                 seq_len(size-2)+1, init = paste0(id_var, "_t_0 == ", id_var, "_t_1"))
  
  return(f_dt[eval(parse(text=cond))])
}

#' Fold a dataset to a certain size and avoid overlapping of different time-series
#' 
#' If the dataset that is going to be folded contains several different time-series 
#' instances of the same process, folding it could introduce false rows with data
#' from different time-series. Given an id variable that labels the different 
#' instances of a time series inside a dataset and a desired size, this function 
#' folds the dataset and avoids mixing data from different origins in the same instance.
#' @param dt data.table to be folded
#' @param size the size of the data.table
#' @param id_var the variable that labels each individual instance of the time-series
#' @param clear_id_var boolean that decides whether or not the id_var column is deleted 
#' @return the filtered dataset
#' @export
filtered_fold_dt <- function(dt, size, id_var, clear_id_var = TRUE){
  logical_arg_check(clear_id_var)
  
  f_dt <- fold_dt(dt, size) 
  f_dt <- filter_same_cycle(f_dt, size, id_var)
  del_vars <- names(f_dt)[grepl(id_var, names(f_dt))]
  if(clear_id_var)
    f_dt[, (del_vars) := NULL]
  else{
    old_col <- del_vars[1]
    f_dt[, (del_vars[-1]) := NULL]
    data.table::setnames(f_dt, old_col, id_var)
  }
  
  return(f_dt)
}

#' One hot encoder for natural numbers without the 0.
#' 
#' Given a natural number, return the natural number equivalent to its
#' one-hot encoding. Examples: 3 -> 100 -> 4, 5 -> 10000 -> 16
#' 
#' @param nat the natural number to convert
#' @return the converted number
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
ordering_gen_exp <- function(n){
  res <- rep("", n)
  for(i in 1:n)
    res[i] <- paste0("X", i-1)
  
  return(res)
}

#' Experimental function that generates a random DBN and samples a dataset that defines it
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
#' @return a dictionary with the original network structure and the sampled dataset
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
