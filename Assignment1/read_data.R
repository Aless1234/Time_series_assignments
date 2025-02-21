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
