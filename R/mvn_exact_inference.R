#' Performs inference over a multivariate normal distribution
#'
#' Performs inference over a multivariate normal distribution given some 
#' evidence. After converting a Gaussian linear network to its MVN form, this
#' kind of inference can be performed.
#' @param sigma the covariance matrix
#' @param mu the mean vector
#' @param evidence values and names of the variables given as evidence
#' @return the posterior mean and covariance matrix
#' @export
mvn_inference <- function(mu, sigma, evidence){
  initial_mu_sigma_check(mu, sigma)
  initial_evidence_check(evidence, names(mu))
  
  mu <- mu[rownames(sigma)] # Avoid positioning bugs
  ev_pos <- which(names(mu) %in% names(evidence))
  evidence <- evidence[names(mu)[ev_pos]]
  
  sigma_22_inv <- solve(sigma[ev_pos, ev_pos])
  
  mu_post <- mu[-ev_pos] + sigma[-ev_pos, ev_pos] %*% 
             sigma_22_inv %*% (evidence - mu[ev_pos])
  
  sigma_post <- sigma[-ev_pos, -ev_pos] - sigma[-ev_pos, ev_pos] %*%
                sigma_22_inv %*% sigma[ev_pos, -ev_pos]
  
  return(list(mu_p = mu_post, sigma_p = sigma_post))
}