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
  plot(ts(dt[, .SD, .SDcols = var]))
  invisible(sapply(1:max(results[get("exec")]), function(x){
    lines(results[get("exec") == x, .SD, .SDcols = var], col = "red")}))
}

plot_results <- function(dt, results, obj_vars){
  invisible(sapply(obj_vars, function(x){plot_single_result(dt, results, x)}))
}

as_named_vector <- function(dt){
  res <- as.numeric(dt)
  names(res) <- names(dt)
  
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