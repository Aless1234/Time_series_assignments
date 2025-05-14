library(ggplot2)
library(forecast)
library(tidyr)
library(ggpubr)
library(dplyr)

setwd("Assignment4")
print(getwd())

# ------------------------------- Exercise 1 -------------------------------
# Exercise 1.1 : simulate 5 independent realizations of X_t

set.seed(123)
n       <- 100
a       <- 0.9
b       <- 1
sigma1  <- 1
X0      <- 5
n_paths <- 5

X <- matrix(0, nrow = n + 1, ncol = n_paths)
X[1, ] <- X0
for (i in seq_len(n_paths)) {
  for (t in 2:(n + 1)) {
    X[t, i] <- a * X[t - 1, i] + b + rnorm(1, 0, sigma1)
  }
}


df <- as.data.frame(X)
df$time <- 0:n

df_long <- pivot_longer(
  df,
  cols      = starts_with("V"),
  names_to  = "path",
  values_to = "value"
)

# Rename paths V1→1, etc.
df_long$path <- factor(
  sub("^V", "", df_long$path),
  levels = as.character(1:n_paths)
)

p <- ggplot(df_long, aes(x = time, y = value, color = path)) +
  geom_line(size = 1) +
  labs(
    # title = "Five Independent Realizations of X_t",
    x     = "Time step t",
    y     = expression(X[t]),
    color = "Path"
  ) +
  theme_minimal(base_size = 14)

print(p)  


ggsave(
  "Figures/1.1.png",
  plot   = p,
  width  = 15,    # inches
  height = 7,    # inches
  dpi    = 300
)



# Exercise 1.2 : simulate one sample path of the hidden state Xt and its noisy observations Yt

set.seed(456)        # new seed so it’s not identical to Ex.1.1
n       <- 100
a       <- 0.9
b       <- 1
sigma1  <- 1
sigma2  <- 1
X0      <- 5

# Pre‐allocate
X <- numeric(n+1)
Y <- numeric(n+1)
X[1] <- X0

# Simulate
for (t in 2:(n+1)) {
  X[t] <- a * X[t-1] + b + rnorm(1, 0, sigma1)
  Y[t] <- X[t]       + rnorm(1, 0, sigma2)
}

# (Optional) reshape for ggplot2
library(tidyr)
library(ggplot2)
df <- data.frame(
  t = 0:n,
  State = X,
  Obs   = Y
)
df_long <- pivot_longer(df, c("State","Obs"),
                        names_to = "Series",
                        values_to = "Value")

# Plot
p <- ggplot(df_long, aes(x=t, y=Value, color=Series, linetype=Series)) +
  geom_line(size=1) +
  scale_color_manual(values = c(State="steelblue", Obs="tomato")) +
  scale_linetype_manual(values = c(State="solid", Obs="dashed")) +
  labs(x="Time step t", y="Value") +
  theme_minimal(base_size = 14)

print(p)

ggsave("Figures/1.2.png", plot = p,
       width  = 15, height = 7, dpi = 300)

# Exercise 1.3 : Kalman filter on AR(1)+drift: True vs. Observed vs. Predicted
source("functions/kalmanfilter.R")

# drop the t=0 entry so that indexing matches filter 1…N
X <- X[-1]
Y <- Y[-1]
t <- 1:n


res <- myKalmanFilter(
  y       = Y,
  theta   = c(a, b, sigma1),
  R       = sigma2^2,
  x_prior = X0,       # we “know” X0
  P_prior = sigma1^2  # or a big var if you’re uncertain
)

# Build dataframe for plotting
df <- data.frame(
  t     = t,
  True  = X,
  Obs   = Y,
  Pred  = res$x_pred,
  Pvar  = res$P_pred
)

df <- pivot_longer(
  df,
  cols      = c(True, Obs, Pred),
  names_to  = "Series",
  values_to = "Value"
)

# compute 95% band for Pred
df_band <- data.frame(
  t     = t,
  lower = res$x_pred - 1.96 * sqrt(res$P_pred),
  upper = res$x_pred + 1.96 * sqrt(res$P_pred)
)

p <- ggplot() +
  # ribbon for 95% CI around the prediction
  geom_ribbon(data = df_band, 
              aes(x = t, ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.5) +
  # lines & points
  geom_line(data = subset(df, Series=="True"),
            aes(x = t, y = Value, color = "True state"), size = 1.1) +
  geom_point(data = subset(df, Series=="Obs"),
             aes(x = t, y = Value, color = "Obs"), size = 2.0, alpha = 0.6) +
  geom_line(data = subset(df, Series=="Pred"),
            aes(x = t, y = Value, color = "Pred"), linetype = "dashed", size = 1) +
  scale_color_manual(name = "", 
                     values = c("True state"="steelblue",
                                "Obs"="tomato",
                                "Pred"="darkgreen")) +
  labs(
       x = "Time step t",
       y = expression(X[t])) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "top")

print(p)

ggsave("Figures/1.3.png",
       plot   = p,
       width  = 15,    # inches
       height = 10,    # inches
       dpi    = 300)


# Exercise 1.4 : Estimation of parameters using maximum likelihood

simulate_data <- function(n, a, b, sigma1, sigma2 = 1, X0 = 0) {
  X <- numeric(n)
  Y <- numeric(n)
  X[1] <- a * X0 + b + rnorm(1, 0, sigma1)
  Y[1] <- X[1] + rnorm(1, 0, sigma2)
  
  for (t in 2:n) {
    X[t] <- a * X[t-1] + b + rnorm(1, 0, sigma1)
    Y[t] <- X[t] + rnorm(1, 0, sigma2)
  }
  list(X = X, Y = Y)
}

source("functions/myLogLikFun.R")
estimate_params <- function(n_sim = 100, n = 100, true_params, R = 1, x_prior = 0, P_prior = 10) {
  estimates <- matrix(NA, nrow = n_sim, ncol = 3)
  colnames(estimates) <- c("a", "b", "sigma1")

  for (i in 1:n_sim) {
    sim <- simulate_data(n, true_params[1], true_params[2], true_params[3], sigma2 = R)
    y <- sim$Y

    # Initial guess (can be tuned)
    theta0 <- c(0.5, 0.5, 0.5)

    fit <- optim(
      par = theta0,
      fn = myLogLikFun,
      y = y,
      R = R,
      x_prior = x_prior,
      P_prior = P_prior,
      method = "L-BFGS-B",
      lower = c(-10, -10, 1e-4),
      upper = c(10, 10, 10)
    )

    estimates[i, ] <- fit$par
  }

  as.data.frame(estimates)
}

set.seed(423)
params_list <- list(
  c(1, 0.9, 1),
  c(5, 0.9, 1),
  c(1, 0.9, 5)
)

results <- lapply(params_list, function(p) estimate_params(true_params = p))

# Add labels
for (i in 1:length(results)) {
  results[[i]]$scenario <- paste0("a=", params_list[[i]][1], ", sigma1=", params_list[[i]][3])
}

# Combine all
library(reshape2)
library(ggplot2)

all_results <- do.call(rbind, results)
long_results <- melt(all_results, id.vars = "scenario", variable.name = "parameter")

# Get the unique parameter names
params <- unique(long_results$parameter)

# Create a plot for each parameter and save separately
for (param in params) {
  p <- ggplot(filter(long_results, parameter == param), 
              aes(x = scenario, y = value)) +
    geom_boxplot() +
    theme_minimal(base_size = 16) +  # <-- Increase base font size
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels if long
      plot.title = element_text(size = 18, face = "bold"), # Larger title
      axis.title = element_text(size = 16)  # Larger axis titles
    ) +
    labs(y = "Estimated Value", x = "True Parameter Setting")         
  
  # Save with same increased dimensions
  ggsave(filename = paste0("Figures/1.4_", param, ".png"),
         plot = p,
         width = 5,    # Slightly wider
         height = 7,
         dpi = 300)
}

## 1.5. Simulate system noise from a Student’s t-distribution instead.

# Simulate data

set.seed(250)

# Parameters
a <- 1
b <- 0.9
sigma1 <- 1
sigma2 <- 1
n <- 100
n_sim <- 100
nu_values <- c(100, 5, 2, 1)

# Storage
sim_data <- list()

for (nu in nu_values) {
  sim_list <- vector("list", n_sim)
  
  for (sim in 1:n_sim) {
    X <- numeric(n)
    Y <- numeric(n)
    
    X[1] <- 0  # Initial state
    
    for (t in 2:n) {
      lambda_t <- rt(1, df = nu)
      X[t] <- a * X[t - 1] + b + sigma1 * lambda_t
    }
    
    Y <- X + rnorm(n, mean = 0, sd = sigma2)
    
    sim_list[[sim]] <- data.frame(
      time = 1:n,
      X = X,
      Y = Y,
      sim = sim
    )
  }
  
  sim_data[[as.character(nu)]] <- do.call(rbind, sim_list)
}

# Plot t-distribution densities vs Normal

# Define a range of x values for plotting the densities
x_vals <- seq(-5, 5, length.out = 1000)

# Create a data frame with density values
density_df <- data.frame(
  x = x_vals,
  Normal = dnorm(x_vals),
  t_100 = dt(x_vals, df = 100),
  t_5   = dt(x_vals, df = 5),
  t_2   = dt(x_vals, df = 2),
  t_1   = dt(x_vals, df = 1)
)

# Reshape to long format for ggplot
density_long <- pivot_longer(density_df, cols = -x,
                              names_to = "distribution",
                              values_to = "density")

# Plot
p <- ggplot(density_long, aes(x = x, y = density, color = distribution)) +
  geom_line(size = 1.2) +
  labs(x = "Value", y = "Density") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("black", "blue", "red", "orange", "purple")) +
  theme(legend.title = element_blank())

print(p)

ggsave("Figures/1.5.png",
       plot   = p,
       width  = 15,    # inches
       height = 7,    # inches
       dpi    = 300)

#1.5: Estimation using Kalman filter on t-noise simulations
# Estimate parameters on t-noise simulations
t_results <- list()

for (nu in names(sim_data)) {
  sims <- split(sim_data[[nu]], sim_data[[nu]]$sim)
  
  ests <- matrix(NA, nrow = length(sims), ncol = 3)
  colnames(ests) <- c("a", "b", "sigma1")
  
  for (i in 1:length(sims)) {
    y <- sims[[i]]$Y
    theta0 <- c(0.5, 0.5, 0.5)
    
    fit <- optim(
      par = theta0,
      fn = myLogLikFun,
      y = y,
      R = 1,
      x_prior = 0,
      P_prior = 10,
      method = "L-BFGS-B",
      lower = c(-10, -10, 1e-4),
      upper = c(10, 10, 10)
    )
    
    ests[i, ] <- fit$par
  }
  
  df <- as.data.frame(ests)
  df$nu <- as.numeric(nu)  # Store as numeric
  t_results[[nu]] <- df
}

# Combine all results
combined_estimates_t <- do.call(rbind, t_results)

# Reshape for plotting
long_estimates_t <- melt(combined_estimates_t, id.vars = "nu", variable.name = "parameter")

# Sort nu as a factor for proper order
long_estimates_t$nu <- factor(long_estimates_t$nu, levels = sort(unique(long_estimates_t$nu)))

# Optional: pretty labels for the x-axis
levels(long_estimates_t$nu) <- paste0("ν = ", levels(long_estimates_t$nu))

# Plot boxplots per parameter
for (param in unique(long_estimates_t$parameter)) {
  p <- ggplot(filter(long_estimates_t, parameter == param), 
              aes(x = nu, y = value)) +
    geom_boxplot() +
    theme_minimal(base_size = 16) +
    labs(y = "Estimated Value", x = expression(nu~"(degrees of freedom)"))
  
  plot(p)
  ggsave(filename = paste0("Figures/1.5_", param, ".png"),
         plot = p,
         width = 5,
         height = 7,
         dpi = 300)
}


# ------------------------------- Exercise 2 -------------------------------
# Exercise 2.1
# Load the data
data <- read.csv("transformer_data.csv", header = TRUE, sep = ",")

# Preview
head(data)

# Map variable codes to descriptive names with units
label_map <- c(
  Y = "Yt: Transformer Temp (°C)",
  Ta = "Ta,t: Outdoor Temp (°C)",
  S = "Φs,t: Solar Radiation (W/m²)",
  I = "ΦI,t: Transformer Load (kA)"
)

# Reshape and label
data_long <- data %>%
  pivot_longer(cols = c(Y, Ta, S, I), names_to = "Variable", values_to = "Value") %>%
  mutate(VariableLabel = factor(label_map[Variable]))

# Plot
ggplot(data_long, aes(x = time, y = Value)) +
  geom_line(aes(color = VariableLabel), show.legend = FALSE) +
  facet_wrap(~ VariableLabel, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(0, max(data$time), by = 12)) +  # Tick every 12 hours
  labs(
    title = "Time Series of Transformer Station Variables",
    x = "Time (hours)",
    y = NULL
  ) +
  theme_minimal()

ggsave("Figures/transformer_data_plot.png", width = 10, height = 8)

# Exercise 2.2
# Define the Kalman filter log-likelihood function
kf_logLik_dt <- function(par, df) {
  # Extract parameters
  A <- par[1]
  B <- matrix(par[2:4], nrow = 1)  # 1x3
  C <- par[5]
  sigma1 <- abs(par[6])  # system noise std
  sigma2 <- abs(par[7])  # observation noise std
  X0 <- par[8]
  
  # Matrices
  Sigma1 <- matrix(sigma1^2, 1, 1)
  Sigma2 <- matrix(sigma2^2, 1, 1)

  # Data
  Y <- as.matrix(df$Y)  # observed output
  U <- as.matrix(df[, c("Ta", "S", "I")])  # input matrix
  Tn <- nrow(Y)

  # Initialization
  x_est <- matrix(X0, nrow = 1, ncol = 1)
  P_est <- matrix(10, 1, 1)  # prior state variance
  logLik <- 0

  for (t in 1:Tn) {
    # Prediction step
    x_pred <- A * x_est + B %*% t(U[t, , drop = FALSE])  # predicted state
    P_pred <- A * P_est * A + Sigma1

    # Observation prediction
    y_pred <- C * x_pred
    S_t <- C * P_pred * C + Sigma2  # innovation covariance
    innov <- Y[t, ] - y_pred  # prediction error

    # log-likelihood update
    logLik <- logLik - 0.5 * (log(2 * pi * S_t) + (innov^2) / S_t)

    # Kalman gain and update
    K_t <- P_pred * C / S_t
    x_est <- x_pred + K_t * innov
    P_est <- (1 - K_t * C) * P_pred
  }

  return(logLik)
}

# Optimizer wrapper
estimate_dt <- function(start_par, df, lower=NULL, upper=NULL) {
  negLL <- function(par) { -kf_logLik_dt(par, df) }
  optim(
    par = start_par, fn = negLL,
    method = "L-BFGS-B",
    lower = lower, upper = upper,
    control = list(maxit = 1000, trace = 1)
  )
}

# Initial parameter estimates
start_par <- c(0.9, 0.05, 0.05, 0.05, 1, 0.3, 0.3, 23.5)  # match X0 to first Y
lower     <- c(0.5, -0.5, -0.5, -0.5, 0.1, 0.01, 0.01, 20)  # restrict to positive C and higher X0
upper     <- c(1.1,  0.5,  0.5,  0.5, 2,   5,    5,    30)

est <- estimate_dt(start_par, data, lower, upper)
est$par 

# Get residuals and predictions
kf_filter <- function(par, df) {
  A <- par[1]
  B <- matrix(par[2:4], nrow = 1)
  C <- par[5]
  sigma1 <- abs(par[6])
  sigma2 <- abs(par[7])
  X0 <- par[8]
  
  Sigma1 <- matrix(sigma1^2, 1, 1)
  Sigma2 <- matrix(sigma2^2, 1, 1)
  
  Y <- as.matrix(df$Y)
  U <- as.matrix(df[, c("Ta", "S", "I")])
  Tn <- nrow(Y)
  
  x_est <- matrix(X0, 1, 1)
  P_est <- matrix(10, 1, 1)
  
  y_pred_vec <- numeric(Tn)
  innov_vec <- numeric(Tn)
  
  for (t in 1:Tn) {
    x_pred <- A * x_est + B %*% t(U[t, , drop = FALSE])
    P_pred <- A * P_est * A + Sigma1
    
    y_pred <- C * x_pred
    S_t <- C * P_pred * C + Sigma2
    innov <- Y[t, ] - y_pred
    
    K_t <- P_pred * C / S_t
    x_est <- x_pred + K_t * innov
    P_est <- (1 - K_t * C) * P_pred
    
    y_pred_vec[t] <- y_pred
    innov_vec[t] <- innov
  }
  
  return(data.frame(
    time = df$time,
    Y = df$Y,
    Y_pred = y_pred_vec,
    Residuals = innov_vec
  ))
}

results <- kf_filter(est$par, data)

# Plot residuals
ggplot(results, aes(x = time, y = Residuals)) +
  geom_line() +
  labs(title = "Prediction Residuals", x = "Time (hours)", y = "Residual") +
  theme_minimal()

ggsave("Figures/residuals_plot.png", width = 10, height = 5)


#  Plot and save ACF and PACF of residuals
acf_data <- acf(results$Residuals, plot = FALSE)
pacf_data <- pacf(results$Residuals, plot = FALSE)

# Calculate significance limits
n <- length(results$Residuals)
conf_limit <- qnorm((1 + 0.95) / 2) / sqrt(n)

acf_plot <- ggplot(data = data.frame(Lag = acf_data$lag, ACF = acf_data$acf), aes(x = Lag, y = ACF)) +
    geom_segment(aes(xend = Lag, yend = 0), color = "black") +
    geom_point(color = "blue") +
    geom_hline(yintercept = c(-conf_limit, conf_limit), linetype = "dashed", color = "red") +
    labs(title = "ACF of Residuals", x = "Lag", y = "ACF") +
    theme_minimal()

pacf_plot <- ggplot(data = data.frame(Lag = pacf_data$lag, PACF = pacf_data$acf), aes(x = Lag, y = PACF)) +
    geom_segment(aes(xend = Lag, yend = 0), color = "black") +
    geom_point(color = "blue") +
    geom_hline(yintercept = c(-conf_limit, conf_limit), linetype = "dashed", color = "red") +
    labs(title = "PACF of Residuals", x = "Lag", y = "PACF") +
    theme_minimal()

ggsave("Figures/acf_residuals_plot.png", plot = acf_plot, width = 10, height = 5)
ggsave("Figures/pacf_residuals_plot.png", plot = pacf_plot, width = 10, height = 5)


# Q-Q plot of residuals and save
qq_plot <- ggplot(results, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

ggsave("Figures/qq_residuals_plot.png", plot = qq_plot, width = 10, height = 5)

logLik_value <- kf_logLik_dt(est$par, data)
k <- length(est$par)
n <- nrow(data)

AIC <- -2 * logLik_value + 2 * k
BIC <- -2 * logLik_value + log(n) * k

AIC; BIC


# Reshape results to long format for plotting
results_long <- results %>%
  select(time, Observed = Y, Predicted = Y_pred) %>%
  pivot_longer(cols = c("Observed", "Predicted"), names_to = "Type", values_to = "Temperature")

# Plot
ggplot(results_long, aes(x = time, y = Temperature, color = Type, linetype = Type)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("Observed" = "black", "Predicted" = "red")) +
  scale_linetype_manual(values = c("Observed" = "dashed", "Predicted" = "solid")) +
  scale_x_continuous(breaks = seq(0, max(results$time), by = 12)) +
  labs(
    title = "Observed vs Predicted Transformer Temperature",
    x = "Time (hours)",
    y = "Temperature (°C)",
    color = "Legend",
    linetype = "Legend"
  ) +
  theme_minimal()

# Save the plot
ggsave("Figures/observed_vs_predicted_plot.png", width = 10, height = 5)


