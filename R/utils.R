mae <- function(orig, pred){
  return(sum(abs(orig - pred))/length(orig))
}

mae_by_col <- function(dt, col){
  return(mae(unlist(dt[,.SD, .SDcols = names(col)]), col))
}

sd_error <- function(orig, pred){
  return(sd((orig - pred)))
}

print_metrics <- function(metrics, obj_vars){
  cat("The average MAE per execution is:\n")
  sapply(obj_vars, function(x){cat(paste0(x, ": ", round(metrics[x], 4), "\n"))})
}

plot_single_result <- function(dt, results, var){
  plot(ts(dt[, .SD, .SDcols = var]))
  invisible(sapply(1:max(results$exec), function(x){lines(results[exec == x, .SD, .SDcols = var],
                                            col = "red")}))
}

plot_results <- function(dt, results, obj_vars){
  invisible(sapply(obj_vars, function(x){plot_single_result(dt, results, x)}))
}

as_named_vector <- function(dt){
  res <- as.numeric(dt)
  names(res) <- names(dt)
  
  return(res)
}