# Libraries to be used in this assignment
# install.packages("forecast")
# install.packages("ggplot2")
install.packages("ggpubr")
# install.packages("tidyr")
library(ggplot2)
library(forecast)
library(tidyr)
library(ggpubr)

setwd("Assignment3")
print(getwd())

# ----------------------------- Exercise 1 -----------------------------------
## Exercise 1.1 ##

# Set phi values
phi1 <-  0.6
phi2 <-  0.3

polyroot(c(-0.2,-0.2,1))

# Define the AR coefficients vector in the order of AR(1), AR(2), ...
ar_coefs <- c(phi1, phi2)
n <- 200
sim_number <- rep(1:5, each = 200)

set.seed(42)

for(i in 1:5){
  
  if(i == 1){
  
  simulated_data <- arima.sim(n=n, model=list(ar=ar_coefs), sd=1)
  
  final_simulated_data <- simulated_data
  
  
  } else{
  
  simulated_data <- arima.sim(n=n, model=list(ar=ar_coefs), sd=1)
  
  final_simulated_data <- c(final_simulated_data,simulated_data)
  
  }
  
}

final_simulated_data <- data.frame(cbind(index = rep(1:200,times=5),value = final_simulated_data, sim_number = sim_number))

final_simulated_data$sim_number <- factor(final_simulated_data$sim_number)

plot <- ggplot(final_simulated_data, aes(x = index, y = value, color = sim_number, group = sim_number)) + 
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") + # Use a professional color palette
  theme_minimal() + # Use a clean theme
  theme(aspect.ratio = 2/3) + # Make the graph more horizontal
  labs(title = "Simulated with ϕ1 = - 0.6 and ϕ2 = - 0.3", 
     x = "Index", 
     y = "Value", 
     color = "Simulation Number")

ggsave("acf_ra.png", plot = plot, width = 12, height = 6, dpi = 300)
## Exercise 1.2 ##

# Theoretical ACF
calculate_acf <- function(phi1, phi2, k, acf_values) {
  if (k == 0) {
    return(1)
  } else if (k == 1) {
    return(phi1 / (1 - phi2))
  } else {
    return(phi1 * acf_values[k] + phi2 * acf_values[k - 1])
  }
}

n_lags <- 30
acf_values <- numeric(n_lags + 1)

for (k in 0:n_lags) {
  acf_values[k + 1] <- calculate_acf(phi1, phi2, k, acf_values)
}

# Create a data frame for theoretical ACF
acf_df <- data.frame(
  lag = 0:n_lags,
  acf_value = acf_values,
  type = "Theoretical"
)

# Empirical ACF for each simulation
for (i in 1:5) {
  sim_data <- subset(final_simulated_data, sim_number == i)
  acf_sim <- Acf(sim_data$value, lag.max = n_lags, plot = FALSE)
  
  temp_df <- data.frame(
    lag = 0:n_lags,
    acf_value = acf_sim$acf,
    type = paste("Simulation", i)
  )
  
  acf_df <- rbind(acf_df, temp_df)
}

# Plot all ACFs (no lines, just vertical segments + points)
acf <- ggplot(acf_df, aes(x = lag, y = acf_value, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_segment(aes(xend = lag, yend = 0), size = 0.7) +
  geom_point(size = 2) +
  labs(x = "Lag", y = "ACF", color = "Series") +
  theme_minimal() +
  ggtitle("Empirical ACFs of 5 Simulations vs. Theoretical ACF") +
  scale_color_brewer(palette = "Dark2")

ggsave("acf.png", plot = acf, width = 12, height = 6, dpi = 300)



## Exercise 1.3 , 1.4 ##

# Run Exercises 1.1 and 1.2 to get the simulated data and ACF values

## Exercises 1.5 and 1.6 ##

# Simulation function
sim <- function(model, n, nburnin = 100){
  n <- n + nburnin
  ar <- model$ar
  ma <- model$ma
  p <- length(ar)
  q <- length(ma)
  y <- numeric(n)
  eps <- rnorm(n)
  
  for(i in (max(p, q) + 1):n){
    y[i] <- eps[i] + sum(y[i - (1:p)] * ar) + sum(eps[i - (1:q)] * ma)
  }
  
  return(y[(nburnin + 1):n])
}

# ---- Parameters for Exercise 1.5 ---- #
phi1 <- 0.85
phi2 <- 0.2
n <- 200
model <- list(ar = c(phi1, phi2))  # no MA part
sim_number <- rep(1:5, each = n)


set.seed(42)

# ---- Simulate 5 series ---- #
final_simulated_data <- numeric()

for (i in 1:5) {
  simulated <- sim(model, n = n)
  final_simulated_data <- c(final_simulated_data, simulated)
}

final_df <- data.frame(
  index = rep(1:n, times = 5),
  value = final_simulated_data,
  sim_number = factor(sim_number)
)

# ---- Plot the Simulated Time Series ---- #
ggplot(final_df, aes(x = index, y = value, color = sim_number, group = sim_number)) + 
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = paste("Simulated Time Series (ϕ1 =", phi1, ", ϕ2 =", phi2, ")"),
    x = "Time", y = "Value", color = "Simulation"
  )

# Theoretical ACF Calculation
calculate_acf <- function(phi1, phi2, k, acf_values) {
  if (k == 0) return(1)
  if (k == 1) return(phi1 / (1 - phi2))
  return(phi1 * acf_values[k] + phi2 * acf_values[k - 1])
}

n_lags <- 30
acf_values <- numeric(n_lags + 1)
for (k in 0:n_lags) {
  acf_values[k + 1] <- calculate_acf(phi1, phi2, k, acf_values)
}

acf_df <- data.frame(lag = 0:n_lags, acf_value = acf_values, type = "Theoretical")

# Add empirical ACFs
for (i in 1:5) {
  sim_data <- subset(final_df, sim_number == i)
  acf_sim <- Acf(sim_data$value, lag.max = n_lags, plot = FALSE)
  acf_df <- rbind(acf_df, data.frame(
    lag = 0:n_lags,
    acf_value = acf_sim$acf,
    type = paste("Simulation", i)
  ))
}

# Plot ACFs
ggplot(acf_df, aes(x = lag, y = acf_value, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_segment(aes(xend = lag, yend = 0), size = 0.6) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = paste("ACFs for ϕ₁ =", phi1, "and ϕ₂ =", phi2), x = "Lag", y = "ACF", color = "Series")


# ----------------------------- Exercise 2 -----------------------------------

# Exercise 2.1 #
data <- read.csv("datasolar.csv",header = TRUE) # Load data
data

data$Xt <- log(data$power) - 5.72 #Define Xt

phi1 <- 0.38 # reverse the sign because of R
Phi1 <- 0.94 # reverse the sign because of R

model <- Arima(data$Xt, order=c(1,0,0), seasonal=list(order=c(1,0,0), period=12),
               fixed=c(phi1,Phi1, 0))

# Calculate residuals
residuals <- residuals(model)[-c(1:13)]

# Model validation

# PLOT - Set up a 2x2 layout for the plots
par(mfrow=c(2,2)) 

# Plot residuals
plot(residuals, pch=19, main="Residuals", ylab="Residual", xlab="Time", 
     ylim=c(min(residuals), max(residuals)))
segments(x0=1:length(residuals), y0=residuals, x1=1:length(residuals), y1=0, col="blue", lty=2)  # Lines to zero
abline(h=0, col="red", lty=1)  # Add horizontal line at zero

# Plot the ACF of the residuals - check for independence
Acf(residuals, lag.max = 30, main="ACF of Residuals")

# Plot the PACF of the residuals check for independence
Pacf(residuals, lag.max = 30, main="PACF of Residuals")

# Q-Q plot - check if the residuals are normally distributed 
qqnorm(residuals)
qqline(residuals, col="red", main="Q-Q Plot")

# Reset the plotting layout to default (optional)
par(mfrow=c(1,1)) 

# Exercise 2.2 #

# Use the ARIMA model to predict the next 12 months
predicted_values_12 <- predict(model, n.ahead = 12)

# Transform predicted values back to power using the inverse log transformation
predicted_values_12_pred <- exp(5.72 + predicted_values_12$pred)  # Exponentiate and add the mean (5.72)
predicted_values_12_se <- exp(5.72 + predicted_values_12$se)  # Standard errors (for uncertainty)

# Create a data frame for the final data (observed + predicted values)
final_data <- data.frame(index = 1:(36 + 12),  # Total indices
                         power = c(data$power, predicted_values_12_pred),  # Observed + predicted power
                         label = c(rep("train", 36), rep("prediction", 12)),  # Train and prediction labels
                         grouping = rep(1, 48))

# Convert label to a factor for plotting
final_data$label <- factor(final_data$label)

# Plot the observed and predicted values
plot <- ggplot(final_data, aes(x = index, y = power, color = label)) +
  geom_line(aes(color = label), lwd = 1.2) +  # Plot the lines for observed and predicted
  geom_point(size = 1.5, color = "black") +  # Add points for observed and predicted
  labs(x = "Time", y = "Power (MWh)")  #+
  # scale_color_manual(values = c("train" = "blue", "prediction" = "red"))

# Save the plot as a .png file (horizontal orientation)
ggsave("2_2.png", plot = plot, width = 12, height = 6, dpi = 300)


# Combine predicted values with their time indices
predicted_table <- data.frame(
  Time = 37:48,  # Time index for the next 12 months
  Predicted_Power = predicted_values_12_pred
)

# Print the table
print(predicted_table)


# Exercise 2.3 #

# forecasting steps:
k <- 12
phi = 0.38

# Initializing Variance for prediction errors:
V_pred_err <- numeric(k)
V_pred_err[1] <- 1 * 0.22^2
V_pred_err[2] <- (phi^2 + 1) * 0.22^2

# Computing Variance for prediction errors for all ks:
for (i in 3:k) {
  V_pred_err[i] <- (phi^(2*(i-1)) + (V_pred_err[i-1]) / 0.22^2) * 0.22^2
}

# Computing the prediction interval:
alpha = 0.05
u_alpha_2 <- qnorm(1 - (alpha / 2))

# Upper prediction interval
upper_val <- as.numeric(predicted_values_12$pred + u_alpha_2 * sqrt(V_pred_err))
upper_val <- exp(upper_val + 5.72)

# Lower prediction interval
lower_val <- as.numeric(predicted_values_12$pred - u_alpha_2 * sqrt(V_pred_err))
lower_val <- exp(lower_val + 5.72)

# Prepare the data frame for plotting
prediction_data <- data.frame(
  index = 37:48,
  pred = predicted_values_12_pred,
  low = lower_val,
  up = upper_val,
  label2 = rep("prediction", 12)
)

# Plot the data
data <- data.frame(index = 1:36, power = data$power, label = rep("train", 36))

plot <- ggplot(data, aes(x = index, y = power, color = label)) +
  geom_line(aes(color = label), lwd = 1.2) +  
  geom_point(size = 1.5, color = "black") +
  geom_line(data = prediction_data, aes(x = index, y = pred, color = label2), lwd = 1, inherit.aes = FALSE) +
  geom_point(data = prediction_data, aes(x = index, y = pred), color = "black") +
  geom_ribbon(data = prediction_data, aes(x = index, ymin = low, ymax = up, fill = "Prediction Interval"), 
              alpha = 0.2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Prediction Interval" = "red"))+
  labs(x = "Time", y = "Power (MWh)")

ggsave("2_3.png", plot = plot, width = 12, height = 6, dpi = 300)



# ----------------------------- Exercise 3 -----------------------------------