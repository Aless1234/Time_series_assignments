# Libraries to be used in this assignment
# install.packages("forecast")
library(ggplot2)
library(forecast)
library(lubridate)
library(patchwork)
library(GGally)
library(dplyr)
library(tibble)

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

# 3.4. Estimate the impulse response from Tdelts and Gv to Ph, up to lag 10
# Standardize: use [,1] to extract numeric vector
D3_train_z <- D3_train %>%
  mutate(
    Ph_z = scale(Ph)[, 1],
    Tdelta_z = scale(Tdelta)[, 1],
    Gv_z = scale(Gv)[, 1]
  )

# CCF: From Tdelta to Ph
ccf_tdelta <- ccf(D3_train_z$Tdelta_z, D3_train_z$Ph_z, lag.max = 10, plot = FALSE)

# CCF: From Gv to Ph
ccf_gv <- ccf(D3_train_z$Gv_z, D3_train_z$Ph_z, lag.max = 10, plot = FALSE)

# Convert CCF output to data frames
irf_tdelta <- tibble(
  Lag = as.vector(ccf_tdelta$lag),
  Correlation = as.vector(ccf_tdelta$acf)
) %>% filter(Lag >= 0 & Lag <= 10)

irf_gv <- tibble(
  Lag = as.vector(ccf_gv$lag),
  Correlation = as.vector(ccf_gv$acf)
) %>% filter(Lag >= 0 & Lag <= 10)


# Plot: Impulse response from Tdelta to Ph
p_irf_tdelta <- ggplot(irf_tdelta, aes(x = Lag, y = Correlation)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Impulse Response: Tdelta → Ph", x = "Lag", y = "Cross-Correlation") +
  theme_minimal(base_size = 14)

# Plot: Impulse response from Gv to Ph
p_irf_gv <- ggplot(irf_gv, aes(x = Lag, y = Correlation)) +
  geom_col(fill = "firebrick") +
  labs(title = "Impulse Response: Gv → Ph", x = "Lag", y = "Cross-Correlation") +
  theme_minimal(base_size = 14)

# Save them
ggsave("3.4-irf_tdelta_ph.png", p_irf_tdelta, width = 8, height = 5)
ggsave("3.4-irf_gv_ph.png", p_irf_gv, width = 8, height = 5)

# 3.5. Fit the linear regression model

lm_fit <- lm(Ph ~ Tdelta + Gv, data = D3_train)
summary(lm_fit)

# 2. One-step-ahead predictions and residuals
D3_train$Ph_pred <- predict(lm_fit)
D3_train$resid <- D3_train$Ph - D3_train$Ph_pred

# 3. Plot actual vs predicted
p_pred <- ggplot(D3_train, aes(x = tdate)) +
  geom_line(aes(y = Ph, color = "Actual")) +
  geom_line(aes(y = Ph_pred, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "steelblue")) +
  theme_minimal(base_size = 14)

# 4. Plot residuals over time
p_resid <- ggplot(D3_train, aes(x = tdate, y = resid)) +
  geom_line(color = "firebrick") +
  labs(title = "Residuals over Time", y = "Residual", x = "Time") +
  theme_minimal(base_size = 14)

# 5. ACF of residuals
acf(D3_train$resid, main = "ACF of Residuals")

# 6. Cross-correlation: Residuals vs Tdelta
ccf(D3_train$resid, D3_train$Tdelta, main = "CCF: Residuals vs Tdelta")

# 7. Cross-correlation: Residuals vs Gv
ccf(D3_train$resid, D3_train$Gv, main = "CCF: Residuals vs Gv")

# 8. Save plots
ggsave("3.5-actual_vs_pred.png", p_pred, width = 8, height = 5)
ggsave("3.5-residuals.png", p_resid, width = 8, height = 5)
