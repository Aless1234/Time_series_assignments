# Libraries to be used in this assignment
# install.packages("forecast")
# install.packages("ggplot2")
library(ggplot2)
library(forecast)

setwd("Assignment2")
print(getwd())

# ----------------------------- Exercise 1 -----------------------------------
# 1.1 Determine if the following AR(2) process is stationary
# Define coefficients
phi1 <- -0.7
phi2 <- -0.2

# Solve characteristic equation 1 + phi1*z + phi2*z^2 = 0
roots <- polyroot(c(1, phi1, phi2))

# Check if roots are outside the unit circle
is_stationary <- all(Mod(roots) > 1)
print(roots)
cat("Is the process stationary? ", is_stationary, "\n")

# 1.3 Write the autocorrelation ρ(k) for the AR(2) process
nlag <- 30  # Number of lags

# Initialize autocorrelation vector
rho <- numeric(nlag)
rho[1] <- -phi1 / (1 + phi2)  # Compute rho(1)
rho[2] <- -phi2 + (phi1^2 / (1 + phi2))  # Compute rho(2)

# Compute rho(k) recursively for k >= 3
for (k in 3:nlag) {
  rho[k] <- -phi1 * rho[k - 1] - phi2 * rho[k - 2]
}

# Exercise 1.4 Plot autocorrelation function
# Create a data frame for ggplot2
acf_dataframe <- data.frame(lag = 1:nlag, acf_value = rho)

# Plot using ggplot2
acf_plot <- ggplot(acf_dataframe, aes(x = lag, y = acf_value)) +
  # Draw vertical blue segments from the autocorrelation value to 0
  geom_segment(aes(xend = lag, yend = 0), color = "blue") +
  # Add black dots at the tip of each stick
  geom_point(color = "black") +
  # Add a horizontal red dotted line at y = 0
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  # Label axes and title
  labs(x = "Lag (k)", y = "Autocorrelation ρ(k)") +
  # Use a minimal theme and adjust the aspect ratio to make the plot wider than high
  theme_light() + theme(aspect.ratio = 1/2)
# Print the plot to the active device
print(acf_plot)

# Save the ggplot2 plot as a PNG file
ggsave("acf_ra.png", plot = acf_plot, width = 8, height = 4, dpi = 300)

# ----------------------------- Exercise 2 ----------------------------------- 
# ----------------------------- Angel code -----------------------------------

# Function for plot
plotit <- function(x){
  layout(rbind(1,2:3))
  par(mar=c(3,3,1,1), mgp=c(2, 0.7,0))
  plot(x, ylab="X")
  acf(x, lag.max=50, lwd=2)
  pacf(x, lag.max=50, lwd=2)
}

n <- 1500

## Exercise 3.1 ##

phi1 <- -0.6

# Simulate the ARIMA(1,0,0) model
set.seed(44)  # Setting seed for reproducibility

sim_data <- arima.sim(n = n, model = list(ar = phi1, order = c(1,0,0)), sd = 1)

plotit(sim_data)

# # Convert simulated data into a data frame for ggplot
# time_series <- data.frame(Time = 1:length(sim_data), Value = as.numeric(sim_data))

# # Create the plot
# ggplot(time_series, aes(x = Time, y = Value)) +
#   geom_line(color = "black", size = 0.7) +  # Black line
#   geom_point(color = "red", size = 1.5) +  # Red points for visibility
#   theme_minimal() +  # Clean theme
#   labs(title = "Simulated AR(1) Process",
#        x = "Index",
#        y = "Simulated Data") +
#   theme(text = element_text(size = 16),  # Increase font size
#         axis.title = element_text(size = 18),  
#         plot.title = element_text(size = 20, face = "bold"))

# # Calculate ACF and PACF
# acf_1 <- acf(sim_data, plot = FALSE)
# pacf_1 <- pacf(sim_data, plot = FALSE)

# # Plot ACF with increased font size and grid
# acf(sim_data, main = "Autocorrelation Function (ACF)", cex.axis = 1.5, cex.lab = 1.5)
# grid()  # Add grid

# # Plot PACF with increased font size and grid
# pacf(sim_data, main = "Partial Autocorrelation Function (PACF)", cex.axis = 1.5, cex.lab = 1.5)
# grid()  # Add grid



## Exercise 3.2 ##

# Parameters
Phi1 <- 0.9

# Simulate seasonal AR(1) process
set.seed(42) # For reproducibility

sim_data_2 <- arima.sim(model =list(ar = c(numeric(11),Phi1)),n = n, sd = 1)
plotit(sim_data_2)

# acf(sim_data_2)
# pacf(sim_data_2)

## Exercise 3.3 ##

phi1 <- -0.9 # opposite sign
theta1 <- -0.7

sim_data_3 <- arima.sim(n = n, model = list(ar = phi1, ma = c(numeric(11),theta1)), sd = 1)
plotit(sim_data_3)

# acf(sim_data_3)
# pacf(sim_data_3)

# ----------------------------- Raul's code -----------------------------------






