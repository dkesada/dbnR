#' Performs inference over a multivariate normal distribution
#'
#' Performs inference over a multivariate normal distribution given some 
#' evidence. After converting a Gaussian linear network to its MVN form, this
#' kind of inference can be performed. It's recommended to use
#' \code{\link{predict_dt}} functions instead unless you need the posterior
#' mean vector and covariance matrix.
#' @param sigma the covariance matrix
#' @param mu the mean vector
#' @param evidence a single row data.table or a named vector with the values and names of the variables given as evidence
#' @return the posterior mean and covariance matrix
#' @examples 
#' size = 3
#' data(motor)
#' dt_train <- motor[200:2500]
#' dt_val <- motor[2501:3000]
#' obj <- c("pm_t_0")
#' 
#' net <- learn_dbn_struc(dt_train, size)
#' f_dt_train <- fold_dt(dt_train, size)
#' f_dt_val <- fold_dt(dt_val, size)
#' ev <- f_dt_val[1, .SD, .SDcols = obj]
#' fit <- fit_dbn_params(net, f_dt_train, method = "mle-g")
#' 
#' pred <- mvn_inference(calc_mu(fit), calc_sigma(fit), ev)
#' @export
mvn_inference <- function(mu, sigma, evidence){
  initial_mu_sigma_check(mu, sigma)
  evidence <- as_named_vector(evidence)
  initial_evidence_check(evidence, names(mu))
  
  mu <- mu[rownames(sigma)] # Avoid positioning bugs
  ev_pos <- which(names(mu) %in% names(evidence))
  evidence <- evidence[names(mu)[ev_pos]]
  
  sigma_22_inv <- tryCatch({solve(sigma[ev_pos, ev_pos], tol = sqrt(.Machine$double.eps))},
                           error = function(cond){warning("The sigma matrix is computationally singular. Using the pseudo-inverse instead.\n")
                             sigma_inv <- solve(sigma[ev_pos, ev_pos])
                             return(sigma_inv)})
  
  mu_post <- mu[-ev_pos] + sigma[-ev_pos, ev_pos] %*% 
             sigma_22_inv %*% (evidence - mu[ev_pos])
  
  sigma_post <- sigma[-ev_pos, -ev_pos] - sigma[-ev_pos, ev_pos] %*%
                sigma_22_inv %*% sigma[ev_pos, -ev_pos]
  
  return(list(mu_p = mu_post, sigma_p = sigma_post))
}
