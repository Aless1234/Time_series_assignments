### Read training data
# ! Perhaps you need to set the working directory!?

# Install the required packages
# install.packages("fpp2")
# install.packages("dplyr")
# install.packages("tidyverse")

# Load the packages
library(fpp2)
library(dplyr)
library(tidyverse)

setwd("Assignment1")
# Print the current working directory
print(getwd())

D <- read.csv("DST_BIL54.csv")
str(D)

# See the help
# ?strftime
D$time <- as.POSIXct(paste0(D$time, "-01"), "%Y-%m-%d", tz = "UTC")
D$time
class(D$time)

## Year to month for each of them
D$year <- 1900 + as.POSIXlt(D$time)$year + as.POSIXlt(D$time)$mon / 12

## Make the output variable a floating point (i.e.\ decimal number)
D$total <- as.numeric(D$total) / 1E6

## Divide intro train and test set
teststart <- as.POSIXct("2024-01-01", tz = "UTC")
Dtrain <- D[D$time < teststart, ]
Dtest <- D[D$time >= teststart, ]


# 1.1 and 1.2

### Plot train and test data
ggplot(Dtrain, aes(x=year, y=total)) +
  geom_point(col="red") + 
  geom_point(data=Dtrain, col="black") +
  xlim(2018, 2024) 

# 2 Linear trend model
# 2.2 Estimate theta 1 and theta 2 and standard error of estimates
# we will fit a linear model: Y_i = beta_0 + beta_1 * time_i + epsilon_i
# so we have two parameters: beta_0 and beta_1
p <- 2

# we also save the number of observations (26 obs. in the training data):
n <- length(Dtrain$year)
n

# X is the "design matrix"
X <- cbind(1, Dtrain$year)
print(X)

# Define covariance matrix for the OLS model
SIGMA_OLS <- diag(n)
SIGMA_OLS[7:12,7:12]
# y is vector with observations:
y <- cbind(Dtrain$total)
print(y)

# to estimate parameters we solve the "normal equations":
theta_OLS <- solve(t(X)%*%X)%*%t(X)%*%y
print(theta_OLS)

# these are the parameter estimates!
theta_0 <- theta_OLS[1]
theta_1 <- theta_OLS[2]

# now we compute y_hat values (so thet we can plot the regression line) 
yhat_ols <- X%*%theta_OLS

# plot:
ggplot(Dtrain, aes(x=year, y=total)) +
  geom_point(col="red") + 
  geom_line(aes(y=yhat_ols), col="red", size=.5) 

# we will now calculate the standard errors on the parameters beta_0 and beta_1:

# first compute residuals:
e_ols <- y - yhat_ols

# calculate sum of squared residuals:
RSS_ols <- t(e_ols)%*%e_ols

# calculate sigma^2:
sigma2_ols <- as.numeric(RSS_ols/(n - p))

# calculate variance-covariance matrix of _parameters_:
V_ols <- sigma2_ols * solve(t(X) %*% X)
print(V_ols)

# the variances of the parameters are the values in the diagonal:
diag(V_ols)
# and the standard errors are given by:
sqrt(diag(V_ols))

se_theta_0 <- (sqrt(diag(V_ols)))[1] # standard error of the intercept-parameter
se_theta_1 <- (sqrt(diag(V_ols)))[2] # standard error of the slope-parameter

# now we have both point estimates and standard errors:
# intercept:
theta_0
se_theta_0
# slope:
theta_1
se_theta_1

# 2.3 Predictions for future values ("forecast")
# now we use the model for predictions on future timepoints
# we use the timepoints from the testdata:
Xtest <- cbind(1, Dtest$year)
print(Xtest)

# compute predictions (we compute all 10 predictions at once):
y_pred <- Xtest%*%theta_OLS
print(y_pred)

# compute prediction variance-covariance matrix:
Vmatrix_pred <- sigma2_ols*(1+(Xtest%*%solve(t(X)%*%X))%*%t(Xtest))
# the variances of individual predictions are in the diagonal of the matrix above
print(diag(Vmatrix_pred))

# compute "prediction intervals" 
y_pred_lwr <- y_pred - qt(0.975, df=n-1)*sqrt(diag(Vmatrix_pred))
y_pred_upr <- y_pred + qt(0.975, df=n-1)*sqrt(diag(Vmatrix_pred))


# 2.4 plot forecast:
ggplot(Dtrain, aes(x=year, y=total)) +
  geom_point(col="red") + 
  geom_line(aes(y=yhat_ols), col="red", size=.5) +
  geom_point(data=Dtest, aes(x=year,y=y_pred), col="red", size=.5) +
  geom_ribbon(data=Dtest, aes(x=year,ymin=y_pred_lwr, ymax=y_pred_upr), inherit.aes=FALSE, alpha=0.2, fill="red") # +
  # xlim(1980, 2020) + ylim(0, 8)

# plot WITH true test data:
ggplot(Dtrain, aes(x=year, y=total)) +
  geom_point(col="red") + 
  geom_line(aes(y=yhat_ols), col="red", size=.5) +
  geom_point(data=Dtest, aes(x=year,y=y_pred), col="red", size=.5) +
  geom_ribbon(data=Dtest, aes(x=year,ymin=y_pred_lwr, ymax=y_pred_upr), inherit.aes=FALSE, alpha=0.2, fill="red") +
  geom_point(data=Dtest, aes(x=year,y=total), col="black") # +
  # xlim(1980, 2020) + ylim(0, 8)


#2.6 Investigate residuals
# qq plot of residuals:
qqnorm(e_ols)
qqline(e_ols)

# plot residuals versus x (year):
ggplot(Dtrain, aes(x=year)) +
  geom_point(aes(y=e_ols), col="blue") +
  geom_line(aes(y=e_ols), col="blue") + 
  ylim(-1,1)

# plot some white noise:
set.seed(876573)
white_noise = rnorm(n=72, mean = 0, sd = sqrt(sigma2_ols))
qqnorm(white_noise)
qqline(white_noise)

ggplot(Dtrain, aes(x=year)) +
  geom_point(aes(y=white_noise), col="blue") +
  geom_line(aes(y=white_noise), col="blue") + 
  ylim(-1, 1)

# Exercise 3: WLS
# Exericse 3.1 : Describe the covraiance matrix

lambda = 0.9
weights <- lambda^((n-1):0)

SIGMA <- diag(n)
diag(SIGMA) <- 1/weights
# print lower right corner to check:
print(SIGMA[20:26,20:26]) # Looks good

# Exercise 3.2: Plot lambda weights vs time
barplot(weights, names=1:72)

# Exercise 3: Calculate the sum of all lambda weights
WLS_weight_sum <- sum(weights)
OLS_weight_sum <- sum(SIGMA_OLS)
WLS_weight_sum
OLS_weight_sum

# Exercise 3.4: Estimate theta 1 and theta 2 with lambda = 0.9
theta_WLS <- solve(t(X)%*%solve(SIGMA)%*%X)%*%(t(X)%*%solve(SIGMA)%*%y)
print(theta_WLS)
yhat_wls <- X%*%theta_WLS

ggplot(Dtrain, aes(x=year, y=total)) +
  geom_point(col="black") + 
  geom_line(aes(y=yhat_ols), col="red", size=.5, linetype=2) +
  geom_line(aes(y=yhat_wls), col="blue", size=.5)

# Exercise 3.5: Make the forecast for the next 12 months
y_pred_wls <- Xtest%*%theta_WLS

# Compute the prediction intervals
e_wls <- y - yhat_wls
RSS_wls <- t(e_wls)%*%solve(SIGMA)%*%e_wls
sigma2_wls <- as.numeric(RSS_wls/(n - p))
Vmatrix_pred <- sigma2_wls * (1 + (Xtest %*% solve(t(X)%*%solve(SIGMA)%*%X)) %*% t(Xtest) )
y_pred_lwr_wls <- y_pred_wls - qt(0.975, df=n-1)*sqrt(diag(Vmatrix_pred))
y_pred_upr_wls <- y_pred_wls + qt(0.975, df=n-1)*sqrt(diag(Vmatrix_pred))

# Plot of observation for training set, OLS predictions, WLS predictions and prediction intervals
ggplot(Dtrain, aes(x=year, y=total)) +
  geom_point(col="red") + 
  geom_line(aes(y=yhat_ols), col="red", size=.5, linetype=2) +
  geom_point(data=Dtest, aes(x=year,y=y_pred), col="red", size=.5) +
  geom_ribbon(data=Dtest, aes(x=year,ymin=y_pred_lwr, ymax=y_pred_upr), inherit.aes=FALSE, alpha=0.1, fill="red") +
  geom_point(data=Dtest, aes(x=year,y=total), col="black") +
  geom_line(aes(y=yhat_wls), col="blue", size=.5) +
  geom_point(data=Dtest, aes(x=year,y=y_pred_wls), col="blue", size=.5) +
  geom_ribbon(data=Dtest, aes(x=year,ymin=y_pred_lwr_wls, ymax=y_pred_upr_wls), inherit.aes=FALSE, alpha=0.2, fill="blue") 
#coord_cartesian(ylim = c(0, 8), xlim = c(1980,2020)) 

#################################################
# 4 - RLS 
#################################################
# 4.2 Implement the update equations

theta <- c(0, 0)
theta
R <- diag(0.1, 2)
R

# Recursive Least Squares Loop
for (t in 1:3) {
    x_t <- matrix(X[t, ], nrow = 2, ncol = 1)  # Extract current x_t as a row vector
    
    # Update covariance matrix
    R <-  R + x_t %*% t(x_t)

    # Update parameter estimate
    theta <- theta + solve(R) %*% x_t %*% (y[t] - t(x_t) %*% theta)
    # Print results
    cat("Iteration", t, "\n")
    print(theta)
    print(R)
}


# 4.3 Calculate the estimates of theta_N and compare to the OLS estimates

# Initialize the parameter estimates and covariance matrix
theta <- c(50,-10)
# R <- diag(0.01, 2)
# R <- diag(1000, 2)
R <- diag(0.000001, 2)  # Start wit smaller values for the covariance matrix


# Recursive Least Squares Loop
for (t in 1:n) {
    x_t <- matrix(X[t, ], nrow = 2, ncol = 1)  # Extract current x_t as a row vector
    
    # Update covariance matrix
    R <- R + x_t %*% t(x_t)

    # Update parameter estimate
    theta <- theta + solve(R) %*% x_t %*% (y[t] - t(x_t) %*% theta)
}

# Print results
print(theta)
print(theta_OLS)

# R26 <- t(X)%*%X
# print(R26)

# h26 <- t(X)%*%y
# print(h26)

# RLS26 <- solve(R26)%*%h26
# print(RLS26)

# 4.4 Implement RLS with forgetting
RLS_forgetting <- function(lambda) {
  n <- nrow(X)  # Number of observations
  theta <- matrix(0, nrow = 2, ncol = 1)  # Initialize theta
  R <- diag(0.1, 2)  # Initialize R

  theta_estimates <- matrix(0, nrow = n, ncol = 2)  # Store estimates at each step

  for (t in 1:n) {
      x_t <- matrix(X[t, ], nrow = 2, ncol = 1)  # Extract current x_t as a column vector
      
      # Update covariance matrix with forgetting factor λ
      R <- lambda * R + x_t %*% t(x_t)

      # Update parameter estimate
      theta <- theta + solve(R) %*% x_t %*% (y[t] - t(x_t) %*% theta)

      # Store theta estimates
      theta_estimates[t, ] <- theta
  }
  return(theta_estimates)  # Return all estimates over time
}


lambda_1 <- 0.7
theta_1 <- RLS_forgetting(lambda_1)
print(theta_1)

lambda_2 <- 0.99
theta_2 <- RLS_forgetting(lambda_2)
print(theta_2)


# Plot

theta_WLS <- function(lambda) {
  weights <- lambda^((n-1):0)
  SIGMA <- diag(n)
  diag(SIGMA) <- 1/weights

  theta_WLS <- solve(t(X)%*%solve(SIGMA)%*%X)%*%(t(X)%*%solve(SIGMA)%*%y)
  return(theta_WLS)
}

theta_WLS_1 <- theta_WLS(0.7)
theta_WLS_2 <- theta_WLS(0.99)

library(ggplot2)
# install.packages("patchwork")
library(patchwork)

# Convert data into a format suitable for ggplot
time <- 1:nrow(X)
df <- data.frame(
  time = rep(time, 2),
  theta1 = c(theta_1[,1], theta_2[,1]),
  theta2 = c(theta_1[,2], theta_2[,2]),
  lambda = rep(c("λ = 0.7", "λ = 0.99"), each = nrow(X))
)

# Plot the first parameter (theta_1)
p1 <- ggplot(df, aes(x = time, y = theta1, color = lambda)) +
  geom_line() +
  geom_point(aes(x = max(time), y = theta_WLS_1[1]), color = "red", size = 3) +  # Point for WLS λ = 0.7
  geom_point(aes(x = max(time), y = theta_WLS_2[1]), color = "blue", size = 3) +   # Point for WLS λ = 0.99
  # ylim(-170,50) +
  labs(title = "Parameter Estimate θ1 Over Time", x = "Time", y = "θ1") +
  theme_minimal()

# Plot the second parameter (theta_2)
p2 <- ggplot(df, aes(x = time, y = theta2, color = lambda)) +
  geom_line() +
  geom_point(aes(x = max(time), y = theta_WLS_1[2]), color = "red", size = 3) +  # Point for WLS λ = 0.7
  geom_point(aes(x = max(time), y = theta_WLS_2[2]), color = "blue", size = 3) +   # Point for WLS λ = 0.99
  # ylim(-170,20) +
  labs(title = "Parameter Estimate θ2 Over Time", x = "Time", y = "θ2") +
  theme_minimal()

# Arrange plots side by side
p1 + p2


# 4.5 Make one step predictions 

# Define the RLS function with forgetting factor lambda, including one-step predictions and residuals
RLS_forgetting_with_residuals <- function(lambda) {
  n <- nrow(X)  # Number of observations
  theta <- matrix(0, nrow = 2, ncol = 1)  # Initialize theta
  R <- diag(0.1, 2)  # Initialize R

  theta_estimates <- matrix(0, nrow = n, ncol = 2)  # Store estimates at each step
  predictions <- numeric(n)  # Store predictions
  residuals <- numeric(n)  # Store residuals

  for (t in 1:n) {
      x_t <- matrix(X[t, ], nrow = 2, ncol = 1)  # Extract current x_t as a column vector
      y_t <- y[t]  # Current observation

      # Update covariance matrix with forgetting factor λ
      R <- lambda * R + x_t %*% t(x_t)

      # Update parameter estimate
      theta <- theta + solve(R) %*% x_t %*% (y_t - t(x_t) %*% theta)

      # Store theta estimates
      theta_estimates[t, ] <- theta

      # Make one-step ahead prediction (ˆyt+1|t = xt+1|t θ_t)
      predictions[t] <- t(x_t) %*% theta

      # Compute the residual (ˆεt|t−1 = ˆyt|t−1 − yt−1)
      residuals[t] <- y_t - predictions[t]
  }
  return(list(theta_estimates = theta_estimates, predictions = predictions, residuals = residuals))
}

# Calculate RLS with forgetting for both λ = 0.7 and λ = 0.99
lambda_1 <- 0.7
results_1 <- RLS_forgetting_with_residuals(lambda_1)
theta_1 <- results_1$theta_estimates
residuals_1 <- results_1$residuals

lambda_2 <- 0.99
results_2 <- RLS_forgetting_with_residuals(lambda_2)
theta_2 <- results_2$theta_estimates
residuals_2 <- results_2$residuals

# Convert data for plotting residuals
time <- 5:nrow(X)  # Remove burn-in period (1:4)

df_residuals <- data.frame(
  time = rep(time, 2),
  residuals = c(residuals_1[time], residuals_2[time]),
  lambda = rep(c("λ = 0.7", "λ = 0.99"), each = length(time))
)

# Plot residuals for both λ values
p3 <- ggplot(df_residuals, aes(x = time, y = residuals, color = lambda)) +
  geom_line() +
  labs(title = "One-Step Ahead Residuals", x = "Time", y = "Residuals") +
  theme_minimal()

# Display the plot
print(p3)

# QQ plot of residuals for λ = 0.7 and λ = 0.99
qqnorm(residuals_1)
qqline(residuals_1, col = "red", lwd = 2, lty = 2)
title("QQ Plot of Residuals (λ = 0.7)")

qqnorm(residuals_2)
qqline(residuals_2, col = "blue", lwd = 2, lty = 2)
title("QQ Plot of Residuals (λ = 0.99)")

# Plot residuals versus time (for λ = 0.7 and λ = 0.99)
df_residuals_time <- data.frame(
  time = rep(time, 2),
  residuals = c(residuals_1[time], residuals_2[time]),
  lambda = rep(c("λ = 0.7", "λ = 0.99"), each = length(time))
)

ggplot(df_residuals_time, aes(x = time, y = residuals, color = lambda)) +
  geom_point() +
  geom_line() +
  labs(title = "Residuals versus Time", x = "Time", y = "Residuals") +
  theme_minimal() +
  ylim(-0.15, 0.15)  # Adjust the y-axis for better visualization

