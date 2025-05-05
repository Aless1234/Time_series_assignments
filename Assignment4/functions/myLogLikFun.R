myLogLikFun <- function(theta, y, R, x_prior = 0, P_prior = 10) {
  a <- theta[1]
  b <- theta[2]
  sigma1 <- theta[3]
  
  kf_result <- # call the Kalman filter function
  err <- kf_result$innovation       # Innovations
  S <- kf_result$innovation_var   # Innovation covariances
  
  # Compute log-likelihood contributions from each time step
  logL <- 
  return(-logL)  # Return negative log-likelihood for minimization
}
