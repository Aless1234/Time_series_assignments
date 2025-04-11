# Libraries to be used in this assignment
# install.packages("forecast")
library(ggplot2)
library(forecast)
library(lubridate)
library(patchwork)
library(GGally)
library(dplyr)

setwd("Assignment3")
print(getwd())

# ----------------------------- Exercise 1 -----------------------------------




# ----------------------------- Exercise 2 -----------------------------------




# ----------------------------- Exercise 3 -----------------------------------
#3.1. Read the data and plot the three non-lagged time series (Ph,Tdelta,Gv)
D3 <- read.csv("box_data_60min.csv")
str(D3)

# Convert tdate to datetime
D3$tdate <- ymd_hms(D3$tdate)
print(D3$tdate[1:5])

# Define a custom theme for readability
big_text_theme <- theme_minimal(base_size = 16) +  # base_size increases font sizes
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

# Apply this to your plots
p1 <- ggplot(D3, aes(x = tdate, y = Ph)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Time series of Ph", x = "Time", y = "Ph") +
  big_text_theme

p2 <- ggplot(D3, aes(x = tdate, y = Tdelta)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Time series of Tdelta", x = "Time", y = "Tdelta") +
  big_text_theme

p3 <- ggplot(D3, aes(x = tdate, y = Gv)) +
  geom_line(color = "firebrick", size = 1) +
  labs(title = "Time series of Gv", x = "Time", y = "Gv") +
  big_text_theme

ggsave("time_series_Ph.png", p1, width = 12, height = 10)
ggsave("time_series_Tdelta.png", p2, width = 12, height = 10)
ggsave("time_series_Gv.png", p3, width = 12, height = 10)


#3.2. Divide intro train and test set
# Define the split point
test_start <- ymd_hms("2013-02-06 00:00:00")

# Split into training and test sets
D3_train <- D3[D3$tdate <= test_start, ]
D3_test <- D3[D3$tdate > test_start, ]

str(D3_train) #should be 167 observations

#3.3. Investigate the variables and their relationships

# Select only non-lagged columns for training data
D3_train_small <- D3_train %>%
  select(Ph, Tdelta, Gv)


# Scatter plot matrix
scatter<-ggpairs(D3_train_small,
        title = "Scatterplot Matrix: Ph, Tdelta, Gv (Training Set)",
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("points", alpha = 0.6, size = 1)),
        diag = list(continuous = wrap("densityDiag")))
ggsave("3.2-scatterplot_matrix.png", scatter, width = 12, height = 10)

#acf of Ph
acf(D3_train$Ph, main = "ACF of Ph")

# Cross-correlation of Ph and Tdelta
ccf(D3_train$Ph, D3_train$Tdelta, main = "Cross-correlation: Ph vs Tdelta")

# Cross-correlation of Ph and Gv
ccf(D3_train$Ph, D3_train$Gv, main = "Cross-correlation: Ph vs Gv")
