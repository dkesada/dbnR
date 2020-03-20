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