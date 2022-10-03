#' Renames the columns in a data.table so that they end in '_t_0'
#'
#' This will rename the columns in a data.table so that
#' they end in '_t_0', which will be needed when folding the data.table. If
#' any of the columns already ends in '_t_0', a warning will be issued and
#' no further operation will be done. There is no need to use this function
#' to learn a DBN unless some operation with the variable names wants to be 
#' done prior to folding a dataset.
#' @param dt the data.table to be treated
#' @return the renamed data.table
#' @examples 
#' data("motor")
#' dt <- time_rename(motor)
#' @export
time_rename <- function(dt){
  initial_df_check(dt)
  if(sum(grepl("*._t_0", names(dt))) > 0)
    warning("One or more of the column names already ends in '_t_0'. No more suffixes will be added.\n")

  else{
    # If the data.table is not copied, the original one will be renamed. Can be unexpected behaviour
    dt <- data.table::copy(dt)
    sapply(names(dt), grep, pattern = "*._t_0")
    old <- names(dt[,.SD])
    new <- sapply(old,paste0, ... = paste0("_t_0"), simplify=T, USE.NAMES = F)
    setnames(dt, old, new)
  }

  return (dt)
}

#' Widens the dataset to take into account the t previous time slices
#'
#' This will widen the dataset to put the t previous time slices
#' in each row, so that it can be used to learn temporal arcs in the second
#' phase of the dmmhc. Recursive version not exported, the user calls from the
#' handler 'fold_dt'
#' @param dt the data.table to be treated
#' @param n_prev names of the previous time slice
#' @param size number of time slices to unroll. Markovian 1 would be size 2
#' @param slice the current time slice being treated. Should not be modified
#' when first calling.
#' @return the extended data.table
#' @keywords internal
fold_dt_rec <- function(dt, n_prev, size, slice = 1){
  if(size > slice){
    n <- sapply(n_prev,sub, pattern = paste0("_t_", slice-1),
                replacement = paste0("_t_",slice), simplify=T)
    dt[, (n) := shift(.SD, 1), .SDcols = n_prev]
    dt <- dt[-1]
    dt <- fold_dt_rec(dt, n, size, slice + 1)
  }

  return (dt)
}

#' Filter the instances in a data.table with different ids in each row
#' 
#' Given an id variable that labels the different instances of a time series
#' inside a dataset, discard the rows that have values from more than 1 id.
#' @param f_dt folded data.table
#' @param size the size of the data.table
#' @param id_var the variable that labels each individual instance of the time series
#' @return the filtered data.table
#' @examples
#' dt <- dbnR::motor[201:2500]
#' dt[, n_sec := rep(seq(46), each = 50)] # I'll create secuences of 50 instances each
#' f_dt <- dbnR::fold_dt(dt, size = 2)
#' f_dt[50, .SD, .SDcols = c("n_sec_t_0", "n_sec_t_1")]
#' f_dt <- dbnR::filter_same_cycle(f_dt, size = 2, id_var = "n_sec")
#' f_dt[50, .SD, .SDcols = c("n_sec_t_0", "n_sec_t_1")]
#' @export
filter_same_cycle <- function(f_dt, size, id_var){
  initial_folded_dt_check(f_dt)
  initial_size_check(size)
  character_arg_check(id_var)
  
  cond <- Reduce(function(acu, x){paste0(acu, " & ", id_var, "_t_0 == ", id_var, "_t_", x)}, 
                 seq_len(size-2)+1, init = paste0(id_var, "_t_0 == ", id_var, "_t_1"))
  
  return(f_dt[eval(parse(text=cond))])
}

#' Fold a dataset avoiding overlapping of different time series
#' 
#' If the dataset that is going to be folded contains several different time series 
#' instances of the same process, folding it could introduce false rows with data
#' from different time series. Given an id variable that labels the different 
#' instances of a time series inside a dataset and a desired size, this function 
#' folds the dataset and avoids mixing data from different origins in the same instance.
#' @param dt data.table to be folded
#' @param size the size of the data.table
#' @param id_var the variable that labels each individual instance of the time series
#' @param clear_id_var boolean that decides whether or not the id_var column is deleted 
#' @return the filtered data.table
#' @examples 
#' dt <- dbnR::motor[201:2500]
#' dt[, n_sec := rep(seq(46), each = 50)] # I'll create secuences of 50 instances each
#' f_dt <- dbnR::fold_dt(dt, size = 2)
#' dim(f_dt)
#' f_dt <- dbnR::filtered_fold_dt(dt, size = 2, id_var = "n_sec")
#' dim(f_dt)  # The filtered folded dt has a row less for each independent secuence
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

#' Widens the dataset to take into account the t previous time slices
#'
#' This function will widen the dataset to put the t previous time slices
#' in each row, so that it can be used to learn temporal arcs in the second
#' phase of the dmmhc.
#' @param dt the data.table to be treated
#' @param size number of time slices to unroll. Markovian 1 would be size 2
#' @return the extended data.table
#' @examples 
#' data(motor)
#' size <-  3
#' f_dt <- fold_dt(motor, size)
#' @export
fold_dt <- function(dt, size){
  initial_df_check(dt)
  if(!is.data.table(dt))
    dt <- data.table::as.data.table(dt)
  initial_size_check(size)
  initial_dt_vs_size_check(dt,size)
  
  dt <- data.table::copy(dt)
  if(!check_time0_formatted(dt))
    dt <- time_rename(dt)
  return(fold_dt_rec(dt, names(dt), size))
}

#' Reduce the frequency of the time series data in a data.table
#' 
#' In a time series dataset, there is a time difference between one row and the 
#' next one. This function reduces the number of rows from its current frequency
#' to the desired one by averaging batches of rows. Instead of the frequency in Hz, the 
#' number of seconds between rows is asked (Hz = 1/s).
#' @param dt the original data.table
#' @param obj_freq the desired number of seconds between rows
#' @param curr_freq the number of seconds between rows in the original dataset
#' @param id_var optional variable that labels different time series in a dataset, to avoid averaging values from different processes
#' @return the data.table with the desired frequency
#' @examples 
#' # Let's assume that the dataset has a frequency of 4Hz, 0.25 seconds between rows
#' dt <- dbnR::motor
#' dim(dt)
#' # Let's change the frequency to 2Hz, 0.5 seconds between rows
#' dt <- reduce_freq(dt, obj_freq = 0.5, curr_freq = 0.2)
#' dim(dt)
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

#' Move the window of values backwards in a folded dataset row
#' 
#' This function moves the values in t_0, t_1, ..., t_n-1 in a folded dataset row to
#' t_1, t_2, ..., t_n. All the variables in t_0 will be inputed with NAs and the
#' obtained row can be used to forecast up to any desired point.
#' @param f_dt a folded dataset
#' @param row the index of the row that is going to be processed
#' @return a one row data.table the shifted values
#' @examples 
#' dt <- dbnR::motor
#' f_dt <- dbnR::fold_dt(dt, size = 2)
#' s_row <- dbnR::shift_values(f_dt, row = 500)
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

