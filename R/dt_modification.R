#' Renames the columns in a data.table so that they end in '_t_0'
#'
#' This will rename the columns in a data.table so that
#' they end in '_t_0', which will be needed when folding the data.table. If
#' any of the columns already ends in '_t_0', a warning will be issued and
#' no further operation will be done.
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

#' Widens the dataset to take into account the t previous time slices
#'
#' This will widen the dataset to put the t previous time slices
#' in each row, so that it can be used to learn temporal arcs in the second
#' phase of the dmmhc.
#' @param dt the data.table to be treated
#' @param size number of time slices to unroll. Markovian 1 would be size 2
#' @return the extended data.table
#' @examples 
#' data(motor)
#' size <-  3
#' dt <- fold_dt(motor, size)
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
