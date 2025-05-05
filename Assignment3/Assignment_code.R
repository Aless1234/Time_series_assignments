
rm(list = ls())

setwd("C:/Users/emirh/OneDrive - Danmarks Tekniske Universitet/Programming/R/Time series analysis/Projects")

library(forecast)
library(ggplot2)
library(tidyr)
library(ggpubr)

## LOAD THE DATA ##

data <- read.csv("Project2/datasolar.csv",header = TRUE)



## EXERCISE 1.4 ##

# Set phi values
phi1 <- -0.7
phi2 <- -0.2

# Define the AR coefficients vector in the order of AR(1), AR(2), ...
ar_coefs <- c(phi1, phi2)

# Simulate an AR(2) process
simulated_data_start <- arima.sim(n=1000, model=list(ar=ar_coefs), sd=1)

# Calculate ACF
acf_values <- Acf(simulated_data_start, lag.max=30, plot=FALSE)

# Create a data frame for plotting
acf_df <- data.frame(Lag = acf_values$lag, ACF = acf_values$acf)

acf(simulated_data_start, lag.max=30, main='ACF for AR(2) Process')


## ANOTHER VERSION OF 1.4 ##

# Define the recursive function to calculate ACF
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

# Define phi values
phi1 <- -0.7
phi2 <- -0.2

# Number of lags
n_lags <- 30

# Initialize vector to store ACF values
acf_values <- numeric(n_lags + 1)

# Calculate ACF values recursively
for (k in 0:n_lags) {
  acf_values[k + 1] <- calculate_acf(phi1, phi2, k, acf_values)
}

# Print ACF values
acf_values

## Exercise 1.5 ##


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
  geom_line()


## Exercise 1.6 ##

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


## Exercise 1.7-1.10 ##

phi2 <- 0.2

phi1_list <- c(0.2,-0.7,0.8,0.85)

n_lags <- 30

sim_number <- rep(1:5, each = 200)

set.seed(42)


for(i in 1:length(phi1_list)){
  
  phi1_current <- phi1_list[i]
  
  acf_values <- numeric(n_lags + 1)
  
  ar_coefs_current <- c(phi1_current, phi2)
  
  print(ar_coefs_current)
  
  # Calculate ACF values recursively
  for (k in 0:n_lags) {
    acf_values[k + 1] <- calculate_acf(phi1_current, phi2, k, acf_values)
  }
  
  for(j in 1:5){
    
    if(j == 1){
      
      simulated_data <- arima.sim(model = list(ar=ar_coefs_current, order=c(2,0,0)), n = 200)
      
      final_simulated_data <- simulated_data
      
      
    } else{
      
      simulated_data <- arima.sim(model = list(ar=ar_coefs_current, order=c(2,0,0)), n = 200)
      
      final_simulated_data <- c(final_simulated_data,simulated_data)
      
    }
    
  }
  
  final_simulated_data <- data.frame(cbind(index = rep(1:200,times=5),
                                           value = final_simulated_data,
                                           sim_number = sim_number))
  
  final_simulated_data$sim_number <- factor(final_simulated_data$sim_number)
  
  print(ggplot(final_simulated_data, aes(x = index, y = value, color = sim_number, group = sim_number)) + 
    geom_point() +
    geom_line()+
    labs(title = paste("ACF of theoretical and empirical for phi1 =",phi1_current)))
  
  
  
  acf_values_simulation <- Acf(final_simulated_data$value, lag.max = 30, plot = FALSE)
  
  acf_dataframe <- data.frame(theoretical = acf_values, empirical = acf_values_simulation$acf)
  
  # Reshape from wide to long
  acf_dataframe_long <- pivot_longer(acf_dataframe, cols = c(theoretical, empirical), names_to = "type", values_to = "acf_value")
  
  acf_dataframe_long$lag <-  rep(1:31,each = 2)
  
  # Plot with ggplot2
  print(ggplot(acf_dataframe_long, aes(x = lag, y = acf_value, color = type)) +
    geom_point()+
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_segment(aes(xend = lag, yend = 0)) + 
    labs(title = paste("ACF of theoretical and empirical for phi1 =",phi1_current),
         x = "Lag",
         y = "Autocorrelation",
         color = "Series"))
}


#### SECTION NUMBER 2 ####

# Exercise 2.1 #

# Assuming 'data' is your time series data frame with a column 'Yt' for the energy production

data$Xt <- log(data$power) - 5.72

phi1 <- 0.38 # reverse the sign because of R
Phi1 <- 0.94 # reverse the sign because of R

# Fit the Seasonal ARIMA model using the `Arima` function from the `forecast` package
# The `order` argument specifies the non-seasonal part (p,d,q)
# The `seasonal` argument specifies the seasonal part (P,D,Q) and `period` for the seasonal cycle

model <- Arima(data$Xt, order=c(1,0,0), seasonal=list(order=c(1,0,0), period=12),
               fixed=c(phi1,Phi1, 0))

# Step 3: Calculate Residuals
residuals <- residuals(model)[-c(1:13)]

plot(residuals,pch=19) # plotting the residuals

# Step 4: Model Validation
# Plot the ACF of the residuals to check for independence

Acf(residuals, lag.max = 30) # plotting the ACF of the residuals

Pacf(residuals, lag.max = 30)

# Calculate the theoretical quantiles and the sample quantiles
qq_data <- ggqqplot(residuals, conf.int = FALSE)

# Plot
print(qq_data)

## Exercise 2.2 ##

predicted_values_12 <- predict(model,12)

predicted_values_12_pred <- exp((5.72 + predicted_values_12$pred))

predicted_values_12_se <- exp((5.72 + predicted_values_12$se))

final_data <- data.frame(index = 1:48,
                         power = c(data$power,predicted_values_12_pred),
                         label = c(rep("train",36),rep("prediction",12)),
                         grouping = rep(1,48))

final_data$label <- factor(final_data$label)


###### Question 2.3 and plot for 2.2

# forecasting steps:
k <- 12
phi = 0.38
# Intializing Variance for prediction errors:
V_pred_err <- numeric(k)
V_pred_err[1] <- 1*0.22^2
V_pred_err[2] <- (phi^2 + 1)*0.22^2

# Computing Variance for prediction errors for all ks:
for (i in 3:k){
  V_pred_err[i] <- (phi^(2*(i-1)) + (V_pred_err[i-1])/0.22^2)*0.22^2
}

# Computing the prediction interval:
alpha = 0.05
u_alpha_2 <- qnorm(1 - (alpha/2))

# Upper prediction interval
upper_val <- as.numeric(predicted_values_12$pred + u_alpha_2*sqrt(V_pred_err))

upper_val <- exp(upper_val+5.72)

# Lower prediction interval
lower_val <- as.numeric(predicted_values_12$pred - u_alpha_2*sqrt(V_pred_err))

lower_val <- exp(lower_val+5.72)

prediction_data <- data.frame(index = c(37:48), pred =predicted_values_12_pred,
                              low = lower_val,
                              up = upper_val,
                              label2 = rep("prediction",12))

data <- data.frame(index = 1:36,
                         power = data$power,
                         label = rep("train",36))

ggplot(data, aes(x = index, y = power, color = label)) +
  geom_line(aes(color = label), lwd =1.2) +  
  geom_point(size = 1.5, color = "black")+
  geom_line(data=prediction_data, aes(x = index,y=pred, color=label2), lwd=1, inherit.aes = FALSE) +
  geom_point(data=prediction_data, aes(x = index, y = pred),color = "black")+
  geom_ribbon(data=prediction_data, aes(x=index, ymin = low, ymax = up, fill="Prediction Interval"), alpha=0.2, inherit.aes = FALSE)+
  scale_fill_manual(values = c("Prediction Interval" = "red"))



# plot for 2.2

ggplot(data, aes(x = index, y = power, color = label)) +
  geom_line(aes(color = label), lwd =1.2) +  
  geom_point(size = 1.5, color = "black")+
  geom_line(data=prediction_data, aes(x = index,y=pred, color=label2), lwd=1, inherit.aes = FALSE) +
  geom_point(data=prediction_data, aes(x = index, y = pred),color = "black")


#### SECTION 3 ####

n <- 1000

## Exercise 3.1 ##

phi1 <- -0.6

# Simulate the ARIMA(1,0,0) model
set.seed(42)  # Setting seed for reproducibility

sim_data <- arima.sim(n = n, model = list(ar = phi1, order = c(1,0,0)), sd = 1)

acf(sim_data)

pacf(sim_data)

## Exercise 3.2 ##

# Parameters

Phi1 <- 0.9

# Simulate seasonal AR(1) process

set.seed(42) # For reproducibility

sim_data_2 <- arima.sim(model =list(ar = c(numeric(11),Phi1)),n = n, sd = 1)

acf(sim_data_2)

pacf(sim_data_2)

## Exercise 3.3 ##

phi1 <- -0.9 # opposite sign

theta1 <- -0.7

sim_data_3 <- arima.sim(n = n, model = list(ar = phi1, ma = c(numeric(11),theta1)), sd = 1)

acf(sim_data_3)

pacf(sim_data_3)

## Exercise 3.4 ##

phi1 <- 0.6 # opposite sign

Phi1 <- 0.8 # opposite sign

# adding the indiivual simulations together

sim_data_total_test <- arima.sim(n = n, model = list(ar = c(phi1,rep(0,10),Phi1,-phi1*Phi1)), sd = 1)

acf(sim_data_total_test)

pacf(sim_data_total_test)

# # trying Arima
# 
# model4 <- Arima(ts(rnorm(n),freq=1), order=c(1,0,0), seasonal=list(order = c(1,0,0), period = 12),
#                fixed=c(phi=phi1,Phi=Phi1,0))
# 
# 
# foo <- simulate(model4, nsim=n)
# 
# acf(foo, lag.max = 100)
# 
# pacf(foo,lag.max = 30)
# 
# acf(sim_data_4)
# 
# pacf(sim_data_4)



## Exercise 3.5

# Set the seed for reproducibility
set.seed(42)

# Define the model order and parameters

theta1 <- 0.4  # Non-seasonal MA(1) parameter
Theta1 <- -0.8  # Seasonal MA(1) parameter at lag 12

simulated_data_total_3 <- arima.sim(n = n, model = list(ma = c(theta1,rep(0,10),Theta1, theta1*Theta1)), sd = 1)

acf(simulated_data_total_3)

pacf(simulated_data_total_3)

## Exercise 3.6

set.seed(42)

# Define the model order and parameters

theta1 <- -0.4  # MA(1) parameter
Phi1  <- -0.7  # Seasonal AR(1) parameter at lag 12

# Simulate the ARIMA(0,0,1)(1,0,0)[12] model

sim_data_6 <- arima.sim(n = n, model = list(ar = c(rep(0,11),Phi1),ma =theta1), sd = 1)

acf(sim_data_6)

pacf(sim_data_6)


acf(arima.sim(n = n, model = list(ar = c(0.8), ma = c(0.8)), sd = 1))

pacf(arima.sim(n = n, model = list(ar = c(0.8), ma = c(0.8)), sd = 1))

