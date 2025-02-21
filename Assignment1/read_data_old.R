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

setwd("/mnt/c/Users/alessia/OneDrive - Danmarks Tekniske Universitet/Time_Series/Assignment1/")

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
x <- Dtrain$year
y <- Dtrain$total

X <- cbind(1, x)

plot(x, y,
    type = "l", col = "blue",
    xlab = "Year", ylab = "Total (millions)",
    main = "Training Data over Time"
)

# 2 Linear trend model
# 2.2 Estimate theta 1 and theta 2 and standard error of estimates
set.seed(123)
model <- lm(y ~ x)
summary(model)

# Scatter plot with regression line
plot(x, y, pch = 16, col = "blue", main = "Linear Model Fit")
abline(model, col = "red", lwd = 2) # Add regression line

# 2.3 Forecast next 12 months
theta <- coef(model)
epsilons <- rnorm(12, 0, summary(model)$sigma)
xnew <- seq(2024, 2024 + 11 / 12, 1 / 12)

Xnew <- cbind(1, xnew)
ynew <- Xnew %*% theta #+ epsilons 

# Present results as a table
results <- data.frame(time = xnew, forecast = ynew)
results

# 2.4 Plot the forecasted values together with the training data, the fitted model, and prediction intervals

N <- length(y) # Number of training observations
p <- length(theta) # Number of parameters (intercept and slope)
df <- N - p # Degrees of freedom
t_critical <- qt(0.975, df) # Critical t-value for 95% confidence level

XTX_inv <- solve(t(X) %*% X) # Inverse of (X'X)

# Compute the standard errors of the predictions
prediction_errors <- apply(Xnew, 1, function(x) {
    # Compute the variance term
    var_term <- t(x) %*% XTX_inv %*% x
    # Compute the standard error for the prediction
    se_pred <- summary(model)$sigma * sqrt(1 + var_term)
    return(se_pred)
})

lower_bound <- ynew - t_critical * prediction_errors
upper_bound <- ynew + t_critical * prediction_errors

# Make the plot
plot(x, y,
    pch = 16, col = "blue", main = "Linear Model Fit with Shaded Prediction Intervals",
    xlim = c(min(x), max(xnew)), ylim = c(min(c(y, ynew, lower_bound)) - 0.01, max(c(y, ynew, upper_bound)) + 0.01)
)

# Add regression line
abline(model, col = "red", lwd = 2)

# Add forecast points
points(xnew, ynew, pch = 16, col = "green")

# Shade the area between the lower and upper bounds
polygon(c(xnew, rev(xnew)), c(lower_bound, rev(upper_bound)), col = rgb(0.5, 0.5, 1, 0.3), border = NA)

# Plot the prediction intervals as two lines (lower and upper bounds)
lines(xnew, lower_bound, col = "purple", lty = 2) # Lower bound line
lines(xnew, upper_bound, col = "purple", lty = 2) # Upper bound line

# Legend to distinguish the data points and prediction intervals
legend("topleft",
    legend = c("Training Data", "Fitted Model", "Forecasted Points", "Prediction Interval"),
    col = c("blue", "red", "green", "purple"), pch = c(16, NA, 16, NA), lty = c(NA, 1, NA, 2), lwd = c(NA, 2, NA, 1), inset = c(0.05, 0.05)
)

# 2.6 Investigate the residuals
# Residual analysis is a crucial step in validating the assumptions of a linear regression model. The key assumptions are:

# - Independence: The residuals should not exhibit patterns (i.e., they should be white noise).
# - Normality: The residuals should follow a normal distribution.
# - Constant variance (Homoscedasticity): The variance of residuals should be constant across time.
# - No Autocorrelation: The residuals should not show any correlation with past residuals.

residuals <- model$residuals
fitted_values <- model$fitted.values

# Time series plot detecting patterns in the residuals over time, such as trends or cycles.
plot(x, residuals,
    type = "l", col = "blue",
    xlab = "Year", ylab = "Residuals",
    main = "Time Series Plot of Residuals"
)
abline(h = 0, col = "red") # Add a horizontal line at 0

# Scatter plot of residuals vs. input variable (x) to examine whether there is a relationship between residuals and the input variable
plot(x, residuals,
    pch = 16, col = "blue",
    xlab = "Year", ylab = "Residuals",
    main = "Residuals vs. Year"
)
abline(h = 0, col = "red") # Add a horizontal line at 0

# ACF of residuals to understand whether the residuals have any autocorrelation (i.e., whether a residual at time t is related to the residual at time t-k for some lag k)
acf(residuals, main = "ACF of Residuals")

# CCF between residuals and input variable to understand whether the residuals are correlated with the input variable at different time lags
ccf(x, residuals, main = "CCF between Residuals and Input")

# Histogram of residuals to inspect the distribution of the residuals.
hist(residuals,
    breaks = 20, col = "blue",
    xlab = "Residuals", main = "Histogram of Residuals"
)

# QQ plot of residuals to assess the normality of the residuals. If the residuals follow a normal distribution, the points should fall approximately along the diagonal line.
qqnorm(residuals)
qqline(residuals, col = "red")


# Since the residuals doesn't follow a normal distribution it means that the model is not the best fit for the data.



