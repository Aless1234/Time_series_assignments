# Libraries to be used in this assignment
# install.packages("forecast")
# install.packages("ggplot2")
# install.packages("ggpubr")
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
phi2 <-  -0.5

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

ggplot(final_simulated_data, aes(x = index, y = value, color = sim_number, group = sim_number)) + 
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") + # Use a professional color palette
  theme_minimal() + # Use a clean theme
  theme(aspect.ratio = 2/3) + # Make the graph more horizontal
  labs(title = "Simulated with ϕ1 = 0.6 and ϕ2 = -0.3", 
     x = "Index", 
     y = "Value", 
     color = "Simulation Number")

## Exercise 1.2 ##

# Step 1: Theoretical ACF (same as before)
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

# Step 2: Empirical ACF for each simulation
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

# Step 3: Plot all ACFs (no lines, just vertical segments + points)
ggplot(acf_df, aes(x = lag, y = acf_value, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_segment(aes(xend = lag, yend = 0), size = 0.7) +
  geom_point(size = 2) +
  labs(x = "Lag", y = "ACF", color = "Series") +
  theme_minimal() +
  ggtitle("Empirical ACFs of 5 Simulations vs. Theoretical ACF") +
  scale_color_brewer(palette = "Dark2")



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




# ----------------------------- Exercise 3 -----------------------------------