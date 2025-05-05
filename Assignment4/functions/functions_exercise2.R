kf_logLik_dt <- function(par, df) {
  # par: vector of parameters
  # df: data frame with observations and inputs as columns (Y, Ta, S, I)
  # par: Could be on the form c(A11, A12, A21, A22, B11, B12, B21, B22, Q11, Q12, Q22)
  A   <- matrix() # transition matrix
  B   <- matrix() # input matrix
  Sigma1lt <- matrix() # lower-triangle of system covariance matrix
  Sigma1   <- Sigma1lt %*% t(Sigma1lt) # THAT IS!!! The system covariance matrix is given by Qlt %*% t(Qlt) (and is this symmetric positive definite)
  C   <- matrix() # observation matrix
  Sigma2 <- matrix() # observation noise covariance matrix
  X0  <- matrix() # initial state

  # Variables
  obs_cols <- c("Y") # observation column names
  input_cols <- c("Ta","S","I") # input column names

  # pull out data
  Y  <- as.matrix(df[, obs_cols])     # m×T
  U  <- as.matrix(df[, input_cols])   # p×T
  Tn <- nrow(df)

  # init
  n      <- nrow(A)
  x_est  <- matrix(Y[1,], n, 1)            # start state from first obs
  x_est <- X0 
  P_est  <- diag(1e1, n)                   # X0 prior covariance
  logLik <- 0

  for (t in 1:Tn) {
    # prediction step
    x_pred <- # write the prediction step
    P_pred <- # write the prediction step (Sigma_xx)

    # innovation step
    y_pred  <- # predicted observation
    S_t     <- # predicted observation covariance (Sigma_yy)
    innov   <- # innovation (one-step prediction error)

    # log-likelihood contribution
    logLik <- logLik - 0.5*(sum(log(2*pi*S_t)) + t(innov) %*% solve(S_t, innov))

    # update step
    K_t    <- # Kalman gain
    x_est  <- # reconstructed state
    P_est  <- # reconstructed covariance
  }

  as.numeric(logLik)
}

# Optimizer wrapper
estimate_dt <- function(start_par, df, lower=NULL, upper=NULL) {
  negLL <- function(par){ -kf_logLik_dt(par, df) }
  optim(
    par    = start_par, fn = negLL,
    method = "L-BFGS-B",
    lower  = lower, upper = upper,
    control= list(maxit=1000, trace=1)
  )
}
