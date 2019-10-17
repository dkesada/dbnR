#' Renames the columns in a data.table so that they end in '_t_0'
#'
#' This will rename the columns in a data.table so that
#' they end in '_t_0', which will be needed when folding the data.table. If
#' any of the columns already ends in '_t_0', a warning will be issued and
#' no further operation will be done.
#' @param dt the data.table to be treated
#' @return the renamed data.table
#' @export
time_rename <- function(dt){
  if(sum(grepl("*._t_0", names(dt))) > 0)
    warning("One or more of the column names already ends in '_t_0'. No more suffixes will be added.")

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
#' phase of the dmmhc.
#' @param dt the data.table to be treated
#' @param size number of time slices to unroll. Markovian 1 would be size 2
#' @param slice the current time slice being treated. Should not be modified
#' when first calling.
#' @return the extended data.table
#' @export
fold_dt = function(dt, n_prev, size, slice = 1){
  initial_dt_check(dt)
  if(!is.data.table(dt))
    dt <- data.table::as.data.table(dt)
  if(size > slice){
    n <- sapply(n_prev,sub, pattern = paste0("_t_", slice-1),
                replacement = paste0("_t_",slice), simplify=T)
    dt[, (n) := shift(.SD, 1), .SDcols = n_prev]
    dt <- dt[-1]
    dt <- fold_dt(dt, n, size, slice + 1)
  }

  return (dt)
}
