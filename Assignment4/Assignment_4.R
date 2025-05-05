library(ggplot2)
library(forecast)
library(tidyr)
library(ggpubr)

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
  "1.1.png",
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

ggsave("1.2.png", plot = p,
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
             aes(x = t, y = Value, color = "Obs"), size = 0.8, alpha = 0.6) +
  geom_line(data = subset(df, Series=="Pred"),
            aes(x = t, y = Value, color = "Pred"), linetype = "dashed", size = 1) +
  scale_color_manual(name = "", 
                     values = c("True state"="steelblue",
                                "Obs"="tomato",
                                "Pred"="darkgreen")) +
  labs(
       x = "Time step t",
       y = expression(X[t])) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

print(p)

ggsave("1.3.png",
       plot   = p,
       width  = 15,    # inches
       height = 10,    # inches
       dpi    = 300)

# ------------------------------- Exercise 2 -------------------------------


