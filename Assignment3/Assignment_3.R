# Libraries to be used in this assignment
# install.packages("forecast")
# install.packages("tidyr")
# install.packages("ggpubr")
library(forecast)
library(ggplot2)
library(tidyr)
library(ggpubr)

setwd("..") # Move up to Time_series_assignments
setwd("Assignment3") # Move into Assignment3
print(getwd())


# ----------------------------- Exercise 1 -----------------------------------
# 1.1
# AR: arima.sim expects the negative of the given parameters.
# MA: expects the parameters as they are.

# Set seed for reproducibility
set.seed(123)
phi <- c(0.6, -0.5) # AR coefficients (flipped sign)
sim_number <- rep(1:5, each = 200)

for(i in 1:5){ 
  if(i == 1){
    simulated_data <- arima.sim(n=200, model=list(ar=phi), sd=1)
    final_simulated_data <- simulated_data
  } else{
    simulated_data <- arima.sim(n=200, model=list(ar=phi), sd=1)   
    final_simulated_data <- c(final_simulated_data,simulated_data)
  }
  
}

final_simulated_data <- data.frame(cbind(index = rep(1:200,times=5),value = final_simulated_data, sim_number = sim_number))

final_simulated_data$sim_number <- factor(final_simulated_data$sim_number)

ggplot(final_simulated_data, aes(x = index, y = value, color = sim_number, group = sim_number)) + 
  geom_point() +
  geom_line()

# 1.2 Calculate the empirical ACF of the simulation

# Recursive function to compute ACF
calculate_acf <- function(phi1, phi2, k, acf_values) {
  if (k == 0) {
    return(1)
  } else if (k == 1) {
    return(phi1 / (1 - phi2))
  } else {
    
    if(k-2 < 0){
      
      return(phi1 * acf_values[k] + phi2 * acf_values[k-1])
      
    } else{
      
      return(phi1 * acf_values[k] + phi2 * acf_values[k-1])
      
    }
  }
}

# Theoretical ACF values
phi1 <- 0.6
phi2 <- -0.5

# Number of lags
n_lags <- 30

# Initialize vector to store ACF values
acf_values <- numeric(n_lags + 1)

# Calculate ACF values recursively
for (k in 0:n_lags) {
  acf_values[k + 1] <- calculate_acf(phi1, phi2, k, acf_values)
}

# Empirical ACF values
acf_values_simulation <- Acf(final_simulated_data$value, lag.max = 30, plot = FALSE)

acf_dataframe <- data.frame(theoretical = acf_values, empirical = acf_values_simulation$acf)


# Reshape from wide to long
acf_dataframe_long <- pivot_longer(acf_dataframe, cols = c(theoretical, empirical), names_to = "type", values_to = "acf_value")

acf_dataframe_long$lag <-  rep(1:31,each = 2)

# Plot with ggplot2
ggplot(acf_dataframe_long, aes(x = lag, y = acf_value, color = type)) +
  geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_segment(aes(xend = lag, yend = 0)) + 
  labs(x = "Lag", y = "Autocorrelation", color = "Series")

# 1.3


# ----------------------------- Exercise 2 -----------------------------------




# ----------------------------- Exercise 3 -----------------------------------

