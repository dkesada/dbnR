#' Performs inference over a fitted GBN
#'
#' Performs inference over a Gaussian BN. It's thought to be used in a map for
#' a data.table, to use as evidence each separate row.
#' @param fit the fitted bn
#' @param evidence values of the variables used as evidence for the net
#' @examples
#' res <- dt_test[, predict_bn(fit, .SD), by = 1:nrow(dt_test)]
#'
#' @return the mean of the particles for each row
#' @export
predict_bn <- function(fit, evidence){
  
  n <- names(fit$bn)
  obj_nodes <- n[which(!(n %in% names(evidence)))]
  
  pred <- mvn_inference(fit$mu, fit$sigma, as_named_vector(evidence))
  pred <- as.data.table(t(pred$mu_p[,1]))
  if(length(obj_nodes) == 1)
    setnames(pred, names(pred), obj_nodes)
  
  return(pred)
}

#' Performs inference over a test data set with a GBN
#'
#' Performs inference over a test data set, plots the results
#' and gives metrics of the accuracy of the results.
#' @param fit the fitted bn
#' @param dt_test the test data set
#' @param obj_nodes the nodes that are going to be predicted. They are all predicted at the same time
#' @param verbose if TRUE, displays the metrics and plots the real values against the predictions
#' @return the prediction results
#' @export
predict_dt <- function(fit, dt, obj_nodes, verbose = T){
  initial_fit_check(fit)
  initial_df_check(dt)

  obj_dt <- dt[, .SD, .SDcols = obj_nodes]
  ev_dt <- copy(dt)
  ev_dt[, (obj_nodes) := NULL]
  fit_t <- transform_bnfit(fit)
  
  res <- ev_dt[, predict_bn(fit_t, .SD), by = 1:nrow(ev_dt)]
  
  mae <- sapply(obj_nodes, function(x){mae(obj_dt[, get(x)],
                                           res[, get(x)])})
  sd_e <- sapply(obj_nodes, function(x){sd_error(obj_dt[, get(x)], 
                                                 res[, get(x)])})

  if(verbose){
    sapply(obj_nodes,
           function(x){plot(ts(obj_dt[, get(x)]), ylab = x) +
                       lines(ts(res[, get(x)]), col="red")})
    cat("MAE: \n"); print(mae)
    cat("SD: \n"); print(sd_e)
  }

  return(res)
}

#' Performs approximate inference in a time slice of the dbn
#'
#' Given a bn.fit object and some variables, performs
#' particle inference over such variables in the net for a given time slice.
#' @param fit bn.fit object
#' @param variables variables to be predicted
#' @param particles a list with the provided evidence
#' @param n the number of particles to be used by bnlearn
#' @return the inferred particles
aprox_prediction_step <- function(fit, variables, particles, n = 50){
  if(length(particles) == 0)
    particles <- TRUE

  particles <- bnlearn::cpdist(fit, nodes = variables, evidence = particles, 
                                 method = "lw", n = n)
  particles <- as.list(apply(particles, 2, mean))

  return(particles)
}

#' Performs exact inference in a time slice of the dbn
#'
#' Given a bn.fit object and some variables, performs
#' exact MVN inference over such variables in the net for a given time slice.
#' @param fit list with the mu and sigma of the MVN model
#' @param variables variables to be predicted
#' @param evidence a list with the provided evidence
#' @return the inferred particles
exact_prediction_step <- function(fit, variables, evidence){
  if(length(evidence) == 0)
    evidence <- fit$mu[bnlearn::root.nodes(fit$bn)]
  
  res <- mvn_inference(fit$mu, fit$sigma, evidence)
  res$mu_p <- as.list(res$mu_p[,1])
  
  return(res)
}

#' Performs aproximate inference forecasting with the GDBN over a data set
#'
#' Given a bn.fit object, the size of the net and a data.set,
#' performs aproximate forecasting with bnlearns cpdist function over the 
#' initial evidence taken from the data set.
#' @param dt data.table object with the TS data
#' @param fit dbn.fit object
#' @param size number of time slices of the net
#' @param obj_vars variables to be predicted
#' @param ini starting point in the data set to forecast.
#' @param rep number of repetitions to be performed of the aproximate inference
#' @param len length of the forecast
#' @param num_p number of particles to be used by bnlearn
#' @return the results of the forecast
aproximate_inference <- function(dt, fit, size, obj_vars, ini, rep, len, num_p){
  var_names <- names(dt)
  vars_pred_idx <- grep("t_0", var_names)
  vars_subs_idx <- grep("t_1", var_names)
  vars_last_idx <- grep(paste0("t_", size-1), var_names)
  vars_pred <- var_names[vars_pred_idx]
  vars_subs <- var_names[vars_subs_idx]
  vars_prev <- var_names[-c(vars_pred_idx, vars_subs_idx)]
  vars_post <- var_names[-c(vars_pred_idx, vars_last_idx)]
  vars_ev <- var_names[-vars_pred_idx]
  
  test <- NULL
  
  for(i in 1:rep){
    evidence <- dt[ini, .SD, .SDcols = vars_ev]
    
    # Subsequent queries
    for(j in 1:len){
      particles <- aprox_prediction_step(fit, vars_pred, as.list(evidence), num_p)
      if(length(vars_post) > 0)
        evidence[, (vars_prev) := .SD, .SDcols = vars_post]
      evidence[, (vars_subs) := particles[vars_pred]]
      
      temp <- particles[obj_vars]
      temp["exec"] <- i
      test <- rbindlist(list(test, temp))
    }
  }
  
  return(test)
}

#' Performs exact inference forecasting with the GDBN over a data set
#'
#' Given a bn.fit object, the size of the net and a data.set,
#' performs exact forecasting over the initial evidence taken from the data set.
#' @param dt data.table object with the TS data
#' @param fit dbn.fit object
#' @param size number of time slices of the net
#' @param obj_vars variables to be predicted
#' @param ini starting point in the data set to forecast.
#' @param len length of the forecast
#' @return the results of the forecast
exact_inference <- function(dt, fit, size, obj_vars, ini, len){
  var_names <- names(dt)
  vars_pred_idx <- grep("t_0", var_names)
  vars_subs_idx <- grep("t_1", var_names)
  vars_last_idx <- grep(paste0("t_", size-1), var_names)
  vars_pred <- var_names[vars_pred_idx]
  vars_subs <- var_names[vars_subs_idx]
  vars_prev <- var_names[-c(vars_pred_idx, vars_subs_idx)]
  vars_post <- var_names[-c(vars_pred_idx, vars_last_idx)]
  vars_ev <- var_names[-vars_pred_idx]
  
  fit_t <- transform_bnfit(fit)
  test <- NULL
  evidence <- dt[ini, .SD, .SDcols = vars_ev]
  
  for(j in 1:len){
    particles <- exact_prediction_step(fit_t, vars_pred, as_named_vector(evidence))
    if(length(vars_post) > 0)
      evidence[, (vars_prev) := .SD, .SDcols = vars_post]
    evidence[, (vars_subs) := particles$mu_p[vars_pred]]
    
    temp <- particles$mu_p[obj_vars]
    temp["exec"] <- 1
    test <- rbindlist(list(test, temp))
  }
  
  return(test)
}

#' Performs forecasting with the GDBN over a data set
#'
#' Given a bn.fit object, the size of the net and a data.set,
#' performs a forecast over the initial evidence taken from the data set.
#' @param dt data.table object with the TS data
#' @param fit dbn.fit object
#' @param size number of time slices of the net
#' @param obj_vars variables to be predicted
#' @param ini starting point in the data set to forecast.
#' @param len length of the forecast
#' @param rep number of times to repeat the approximate forecasting
#' @param num_p number of particles in the approximate forecasting
#' @param print_res if TRUE prints the mae and sd metrics of the forecast
#' @param plot_res if TRUE plots the results of the forecast
#' @param mode "exact" for exact inference, "aprox" for approximate
#' @return the results of the forecast
#' @export
forecast_ts <- function(dt, fit, size, obj_vars, ini = 1, len = dim(dt)[1]-ini,
                        rep = 1, num_p = 50, print_res = TRUE, plot_res = TRUE,
                        mode = "exact"){
  initial_folded_dt_check(dt)
  initial_dbnfit_check(fit)
  numeric_arg_check(size, ini, len, rep, num_p)
  character_arg_check(obj_vars)
  logical_arg_check(print_res, plot_res)
  initial_mode_check(mode)

  exec_time <- Sys.time()
  
  if(mode == "exact")
    test <- exact_inference(dt, fit, size, obj_vars, ini, len)
  else if (mode == "aprox")
    test <- aproximate_inference(dt, fit, size, obj_vars, ini, rep, len, num_p)

  exec_time <- exec_time - Sys.time()

  metrics <- lapply(obj_vars, function(x){
    test[, mae_by_col(dt[ini:(ini+len-1)], .SD), .SDcols = x, by = exec]})
  metrics <- sapply(metrics, function(x){mean(x$V1)})
  names(metrics) <- obj_vars

  if(print_res){
    print(exec_time)
    print_metrics(metrics, obj_vars)
  }
    
  if(plot_res)
    plot_results(dt[ini:(ini+len-1)], test, obj_vars)

  return(list(orig = dt[ini:(ini+len-1)], pred = test))
}
