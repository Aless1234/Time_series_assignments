# Libraries to be used in this assignment
# install.packages("forecast")
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







