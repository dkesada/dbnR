get_obj_nodes <- function(fit, evidence){
  n <- names(fit)
  return(n[which(!(n %in% names(evidence)))])
}

#' Performs inference over a fitted GBN
#'
#' Performs inference over a Gaussian BN. It's thought to be used in a map for
#' a data.table, to use as evidence each separate row.
#' @param fit the fitted bn
#' @param cols names of the columns used as evidence
#' @param evidence values of the variables used as evidence for the net
#' @examples
#' res <- dt_test[, predict(fit, names_cols, .SD), .SDcols = names_cols, by = 1:nrow(dt_test)]
#'
#' @return the mean of the particles for each row
#' @export
predict_bn <- function(fit, evidence){
  initial_fit_check(fit)

  obj_nodes <- get_obj_nodes(fit, evidence)

  pred <- bnlearn::cpdist(fit, nodes = obj_nodes, evidence = as.list(evidence),
                          method = "lw", n = 1000)

  # w <- attributes(pred)$weights
  # pred_w <- pred$Resultado[which(w > 0.8)]
  pred <- as.data.table(pred)[, sapply(.SD,mean)]

  return(pred)
}

#' Performs inference over a test data set with a GBN
#'
#' Performs inference over a test data set, plots the results
#' and gives metrics of the accuracy of the results.
#' @param fit the fitted bn
#' @param dt_test the test data set
#' @param verbose if TRUE, displays the metrics and plots the real values against the predictions
#' @return the prediction results
#' @export
predict_dt <- function(fit, dt, obj_nodes, verbose = T){
  initial_fit_check(fit)
  initial_df_check(dt)

  obj_dt <- dt[, .SD, .SDcols = obj_nodes]
  ev_dt <- copy(dt)
  ev_dt[, (obj_nodes) := NULL]

  res <- ev_dt[, predict_bn(fit, .SD), by = 1:nrow(ev_dt)]
  res <- as.data.table(t(apply(ev_dt, 1, predict_bn, fit=fit)))
  mae <- sapply(obj_nodes, function(x){mae(obj_dt[, get(x)], res[, get(x)])})
  sd_e <- sapply(obj_nodes, function(x){sd_error(obj_dt[, get(x)], res[, get(x)])})

  if(verbose){
    sapply(obj_nodes,
           function(x){plot(ts(obj_dt[, get(x)]), ylab = x) +
                       lines(ts(res[, get(x)]), col="red")})
    cat("MAE: \n"); print(mae)
    cat("SD: \n"); print(sd_e)
  }

  return(res)
}

#' Performs inference in a time slice of the dbn
#'
#' Given a bn.fit object and some variables, performs
#' particle inference over such variables in the net for a given time slice.
#' @param fit bn.fit object
#' @param variables variables to be predicted
#' @param particles a list with the provided provided
#' @return the inferred particles
predictionStep <- function(fit, variables, particles, n = 50){
  if(length(particles) == 0)
    particles <- bnlearn::cpdist(fit, nodes = variables, evidence = TRUE, method = "lw", n = n)
  else
    particles <- bnlearn::cpdist(fit, nodes = variables, evidence = particles, method = "lw", n = n)
  particles <- as.list(apply(particles, 2, mean))

  return(particles)
}

#' Performs forecasting with the GDBN over a data set
#'
#' Given a bn.fit object, the size of the net and a data.set,
#' performs a forecast over the initial evidence taken from the data set.
#' @param dt data.table object with the TS data
#' @param fit bn.fit object
#' @param size number of time slices of the net
#' @param ini starting point in the data set to forecast.
#' @param len number of points of the TS to forecast
#' @param rep number of times to repeat the forecasting
#' @return the results of the forecast
#' @export
forecast_ts <- function(dt, fit, size, obj_vars, ini = 1, len = dim(dt)[1]-1, rep = 1, num_p = 50){
  # TODO: add security checks for the arguments
  exec_time <- Sys.time()
  var_names <- names(dt)
  vars_pred_idx <- grep("t_0", var_names)
  vars_subs_idx <- grep("t_1", var_names)
  vars_last_idx <- grep(paste0("t_", size-1), var_names)
  vars_pred <- var_names[vars_pred_idx]
  vars_subs <- var_names[vars_subs_idx]
  vars_prev <- var_names[-c(vars_pred_idx, vars_subs_idx)]
  vars_post <- var_names[-c(vars_pred_idx, vars_last_idx)]
  vars_ev <- var_names[-vars_pred_idx]

  evidence <- dt[ini, .SD, .SDcols = vars_ev]

  test <- NULL

  for(i in 1:rep){
    # First query
    particles <- predictionStep(fit, vars_pred, as.list(evidence), num_p)
    if(!is.null(vars_post))
      evidence[, (vars_prev) := .SD, .SDcols = vars_post]
    evidence[, (vars_subs) := particles]

    temp <- particles[obj_vars]
    temp["exec"] <- 1
    test <- rbindlist(list(test, temp))

    # Subsequent queries
    for(j in 1:len){
      particles <- predictionStep(fit, vars_pred, as.list(evidence), num_p)
      if(!is.null(vars_post))
        evidence[, (vars_prev) := .SD, .SDcols = vars_post]
      evidence[, (vars_subs) := particles]
      temp <- particles[obj_vars]
      temp["exec"] <- i
      test <- rbindlist(list(test, temp))
    }
  }

  metrics <- apply(test, 2, forecast::accuracy, x = dt[ini:len+1, t_promedio_p1_t_0])
  metrics <- apply(metrics, 1, mean)
  names(metrics) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  print(metrics)
  print(exec_time - Sys.time())

  plot(ts(dt[ini:dim(dt)[1],t_promedio_p1_t_0]))
  apply(test,2,function(colu){lines(ts(colu),col="red")})
  lines(ts(dt[ini:dim(dt)[1],t_promedio_p1_t_0]))

  return(as.list(metrics))
}

#' Performs forecasting with the GDBN over a data set (WIP)
#'
#' Given a bn.fit object, the size of the net and a data.set,
#' performs a forecast over the initial evidence taken from the data set.
#' @param dt data.table object with the TS data
#' @param fit bn.fit object
#' @param size number of time slices of the net
#' @param ini starting point in the data set
#' @param len number of points of the TS to forecast
#' @param rep number of times to repeat the forecasting
#' @return the results of the forecast
forecast_ts_parallel <- function(dt, fit, size, ini = 1, len = dim(dt)[1], rep = 1){
  exec_time <- Sys.time()
  variables <- name_variables(names(dt), NULL, size-1, 1)
  variables <- c(names(dt), variables)
  variables.prev <- name_variables(names(dt), NULL, size, 1)
  evidence <- assign_evidence(dt[ini:dim(dt)[1]], variables.prev, dt[ini:dim(dt)[1]][1], size, 2)
  test <- NULL
  registerDoParallel(cores = 4)

  test <- foreach(i = 1:rep, .combine = "cbind", .packages = "bnlearn", .export = "predictionStep") %dopar%{
    # First query
    particles <- predictionStep(fit, variables, particles = evidence)
    names(particles) <- variables.prev
    temp <- c(particles["t_promedio_p1_t_1"])

    # Subsequent queries
    for(i in 1:len){
      particles <- predictionStep(fit, variables,  particles = particles)
      names(particles) <- variables.prev
      temp = c(temp, particles["t_promedio_p1_t_1"])
    }

    temp
  }

  metrics <- apply(test, 2, forecast::accuracy, x = dt[ini:len+1, t_promedio_p1_t_0])
  metrics <- apply(metrics, 1, mean)
  names(metrics) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  print(metrics)
  print(exec_time - Sys.time())

  plot(ts(dt[ini:dim(dt)[1],t_promedio_p1_t_0]))
  apply(test,2,function(colu){lines(ts(colu),col="red")})
  lines(ts(dt[ini:dim(dt)[1],t_promedio_p1_t_0]))

  return(as.list(metrics))
}
