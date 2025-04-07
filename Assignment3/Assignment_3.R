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
phi1 <- 0.6
phi2 <- - 0.5

# Define the AR coefficients vector in the order of AR(1), AR(2), ...
ar_coefs <- c(phi1, phi2)

sim_number <- rep(1:5, each = 200)

set.seed(42)

for(i in 1:5){
  
  if(i == 1){
  
  simulated_data <- arima.sim(n=200, model=list(ar=ar_coefs), sd=1)
  
  final_simulated_data <- simulated_data
  
  
  } else{
  
  simulated_data <- arima.sim(n=200, model=list(ar=ar_coefs), sd=1)
  
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
  labs(title = "Simulated with ϕ1 =−0.6 and ϕ2 = 0.5", 
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



## Exercise 1.3 ##

# Run the precious exercises with the new AR coefficients



# ----------------------------- Exercise 2 -----------------------------------




# ----------------------------- Exercise 3 -----------------------------------