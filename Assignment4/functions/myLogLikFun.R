myLogLikFun <- function(theta, y, R, x_prior = 0, P_prior = 10) {
  a <- theta[1]
  b <- theta[2]
  sigma1 <- theta[3]
  
  # call the Kalman filter function
  kf_result <- myKalmanFilter(
  y = y,
  theta = c(a, b, sigma1),
  R = R,
  x_prior = x_prior,
  P_prior = P_prior)

  err <- kf_result$innovation       # Innovations
  S <- kf_result$innovation_var   # Innovation covariances
  
  # Compute log-likelihood contributions from each time step
  logL_contributions <- -0.5 * (log(2 * pi * S) + (err^2) / S)
  logL <- sum(logL_contributions, na.rm = TRUE)

  return(-logL)  # Return negative log-likelihood for minimization
}
