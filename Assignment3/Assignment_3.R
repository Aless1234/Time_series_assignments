# Libraries to be used in this assignment
# install.packages("forecast")
library(ggplot2)
library(forecast)
library(lubridate)
library(patchwork)

setwd("Assignment3")
print(getwd())

# ----------------------------- Exercise 1 -----------------------------------




# ----------------------------- Exercise 2 -----------------------------------




# ----------------------------- Exercise 3 -----------------------------------
#3.1. Read the data and plot the three non-lagged time series (Ph,Tdelta,Gv). Describe the time series and if you can see some dependencies between the variables.
D3 <- read.csv("box_data_60min.csv")
str(D3)

# Convert tdate to datetime
D3$tdate <- ymd_hms(D3$tdate)
print(D3$tdate[1:5])

p1 <- ggplot(D3, aes(x = tdate, y = Ph)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Ph", x = "Time", y = "Ph") +
  theme_minimal()

p2 <- ggplot(D3, aes(x = tdate, y = Tdelta)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Tdelta", x = "Time", y = "Tdelta") +
  theme_minimal()

p3 <- ggplot(D3, aes(x = tdate, y = Gv)) +
  geom_line(color = "firebrick", size = 1) +
  labs(title = "Gv", x = "Time", y = "Gv") +
  theme_minimal()

# Combine plots into one figure
combined_plot <- p1 / p2 / p3 + plot_annotation(title = "Time Series of Ph, Tdelta, and Gv")
combined_plot
