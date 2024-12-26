#install.packages("tsutils")
#install.packages("forecast")
#install.packages("xts")
#install.packages("zoo")
#install.packages("lubridate")
#install.packages("tseries")
#install.packages("tidyverse")
#install.packages("EnvStats")
#install.packages("readxl")
#install.packages("smooth")
#install.packages("MASS")


library(tsutils)
library(forecast)
library(xts)
library(zoo)
library(lubridate)
library(tseries)
library(tidyverse)
library(EnvStats)
library(readxl)
library(smooth)
library(readxl)
library(MASS)

data <- read_excel("data.xlsx")
data_d <- ts(data$DATA, frequency = 365, start = c(1996,77), end=c(1998,81))
plot(data_d)
# Check for outliers
outliers <- tsoutliers(data_d)
outliers
# outliers removed
data <- data[-outliers$index, ]
# missing observations removed
data <- data[!is.na(data$DATA), ]
# time series
data_d = ts(data$DATA, frequency = 365 , start=c(1996,77), end=c(1998,81))
plot(data_d, main= "Time series - Daily data",xlab = 'Time', ylab = 'NN5_027')

#Decomposition for daily series
decomp(data_d,decomposition='additive',outplot=TRUE)
data_d_length = length(data_d)
train_length = 507
cast_horizon = data_d_length - train_length
#training set 
train = ts(data_d[1:train_length], frequency=365,start =c(1996,77))
plot(train , main ="Train Data")
train_1 = ts(data_d[1:train_length],frequency=365,start =c(1996,77))
plot(train_1)
#test set
test <- ts(data_d[(train_length+1):data_d_length],frequency=365, end=c(1998,81))
plot(test , main = "Test Data" , col="red")

#################
#ETS Modelling
# Auto ETS Function
ets_auto = es(train_1, model='ZZN',h=cast_horizon,holdout=TRUE)
ets_auto_forecast <- forecast(ets_auto, h=cast_horizon)
plot(ets_auto_forecast)
accuracy(ets_auto_forecast, test)
print(ets_auto)
ets_auto$accuracy
# Manual ETS Forecasting
ets_m1 <- es(train_1, model="ANN",persistence=0.05, h=cast_horizon, holdout=TRUE)
ets_m1_forecast = forecast(ets_m1,h=cast_horizon)
plot(ets_m1_forecast)
accuracy(ets_m1_forecast, test)
print(ets_m1)
ets_m1$accuracy
# Manual ETS Forecasting
ets_m2 <- es(train_1, model="MAN",persistence=0.1, h=cast_horizon, holdout=TRUE)
ets_m2_forecast = forecast(ets_m2,h=cast_horizon)
plot(ets_m2_forecast)
accuracy(ets_m2_forecast, test)
print(ets_m2)
# Manual ETS Forecasting
ets_m3 <- es(train_1, model="AAN",persistence=0.07, h=cast_horizon, holdout=TRUE)
ets_m3_forecast = forecast(ets_m3,h=cast_horizon)
plot(ets_m3_forecast)
accuracy(ets_m3_forecast, test)
print(ets_m3)
ets_m3$accuracy

# match the start dates of the forecasted series and test series
ets_a_mean <- ts(ets_auto_forecast[["mean"]], frequency = 365, start = start(test))
ets_m3_mean <- ts(ets_m3_forecast[["mean"]], frequency = 365, start = start(test))

#RMSE for auto ETS model
error_ets <- sqrt(mean((ets_a_mean - test)^2, na.rm = TRUE))
error_ets
#sMAPE for auto ETS model
smape_a_error <- mean(2 * abs(test - ets_a_mean) / (abs(test) + abs(ets_a_mean)), na.rm = TRUE) * 100
smape_a_error
#MSE for auto ETS model
MSE_a_error <- mean((test - ets_a_mean)^2, na.rm = TRUE)
MSE_a_error

#RMSE for manual ETS model
RMSE_m_ets <- sqrt(mean((ets_m3_mean - test)^2, na.rm = TRUE))
RMSE_m_ets
#sMAPE for manual ETS model
smape_m_error <- mean(2 * abs(test - ets_m3_mean) / (abs(test) + abs(ets_m3_mean)), na.rm = TRUE) * 100
smape_m_error
#MSE for manual ETS model
MSE_M_error <- mean((test - ets_m3_mean)^2, na.rm = TRUE)
MSE_M_error

checkresiduals(ets_auto)
checkresiduals(ets_m1)

###############
#ARIMA Model
kpss.test(train_1)
adf.test(train_1)
#differencing data
data_d_diff <- diff(train_1)
plot(data_d_diff, main="Differenced Daily Data")
kpss.test(data_d_diff)
adf.test(data_d_diff)
#ACF and PACF for training data
tsdisplay(train_1)
#ACf and PACF for differenciated training data
tsdisplay(data_d_diff)
#Auto ARIMA
arima_auto = auto.arima(train)
arima_auto
tsdisplay(residuals(arima_auto))
#manual arima
fit1 <- arima(x= train,order = c(4,1,5))
fit1
AIC(fit1)
BIC(fit1)
tsdisplay(residuals(fit1))

fit2 <- arima(x = train, order = c(4,1,2))
AIC(fit2)
BIC(fit2)
tsdisplay(residuals(fit2))

fit3 = arima(x=train,order = c(3,1,1))
AIC(fit3)
BIC(fit3)
tsdisplay(residuals(fit3))

fit4 = arima(x=train,order = c(2,1,2))
AIC(fit4)
BIC(fit4)
tsdisplay(residuals(fit4))

arima_data_auto <- forecast(arima_auto, h=cast_horizon)
arima_data_manual1 <- forecast(fit1,h=cast_horizon)
arima_data_manual2 <- forecast(fit2,h=cast_horizon)
arima_data_manual3 <- forecast(fit3,h=cast_horizon)
arima_data_manual4 <- forecast(fit4,h=cast_horizon)

autoplot(arima_data_auto)
autoplot(arima_data_manual1)
autoplot(arima_data_manual2)
autoplot(arima_data_manual3)
autoplot(arima_data_manual4)
#Seperating the mean
s1 = ts(arima_data_auto[["mean"]], frequency = 365,start=c(1997,219))
s2 = ts(arima_data_manual1[["mean"]], frequency = 365,start=c(1997,219))
s3 = ts(arima_data_manual2[["mean"]], frequency = 365,start=c(1997,219))
s4 = ts(arima_data_manual3[["mean"]], frequency = 365,start=c(1997,219))
s5 = ts(arima_data_manual4[["mean"]], frequency = 365,start=c(1997,219))
# Aligning the forecasted with the test series
s1_aligned <- window(s1, start = start(test), end = end(test))
s2_aligned <- window(s2, start = start(test), end = end(test))
s3_aligned <- window(s3, start = start(test), end = end(test))
s4_aligned <- window(s4, start = start(test), end = end(test))
s5_aligned <- window(s5, start = start(test), end = end(test))
# RMSE calculations
error_arima_1 <- sqrt(mean((s1_aligned - test)^2, na.rm = TRUE))
error_arima_1
error_arima_2 <- sqrt(mean((s2_aligned - test)^2, na.rm = TRUE))
error_arima_2
error_arima_3 <- sqrt(mean((s3_aligned - test)^2, na.rm = TRUE))
error_arima_3
error_arima_4 <- sqrt(mean((s4_aligned - test)^2, na.rm = TRUE))
error_arima_4
error_arima_5 <- sqrt(mean((s5_aligned - test)^2, na.rm = TRUE))
error_arima_5
# sMAPE calculations
sMAP_error_1 <- mean(2 * abs(test - s1_aligned) / (abs(test) + abs(s1_aligned)), na.rm = TRUE) * 100
sMAP_error_1
sMAP_error_2 <- mean(2 * abs(test - s2_aligned) / (abs(test) + abs(s2_aligned)), na.rm = TRUE) * 100
sMAP_error_2
sMAP_error_3 <- mean(2 * abs(test - s3_aligned) / (abs(test) + abs(s3_aligned)), na.rm = TRUE) * 100
sMAP_error_3
sMAP_error_4 <- mean(2 * abs(test - s4_aligned) / (abs(test) + abs(s4_aligned)), na.rm = TRUE) * 100
sMAP_error_4
sMAP_error_5 <- mean(2 * abs(test - s5_aligned) / (abs(test) + abs(s5_aligned)), na.rm = TRUE) * 100
sMAP_error_5
#MSE
MSE_a1_error <- mean((test - s1_aligned)^2, na.rm = TRUE)
MSE_a1_error
MSE_a2_error <- mean((test - s2_aligned)^2, na.rm = TRUE)
MSE_a2_error
MSE_a3_error <- mean((test - s3_aligned)^2, na.rm = TRUE)
MSE_a3_error
MSE_a4_error <- mean((test - s4_aligned)^2, na.rm = TRUE)
MSE_a4_error
MSE_a5_error <- mean((test - s5_aligned)^2, na.rm = TRUE)
MSE_a5_error

accuracy(arima_data_auto, test)
accuracy(arima_data_manual1, test)
accuracy(arima_data_manual2, test)
accuracy(arima_data_manual3, test)
accuracy(arima_data_manual4, test)

residuals_plot <- residuals(fit1)
plot(residuals_plot)
hist(residuals_plot)
qqnorm(residuals_plot)
qqline(residuals_plot)

checkresiduals(arima_auto)
checkresiduals(fit1)

############
#auto regression model
lag1 <- stats::lag(data_d, -1)
data_d_lagged <- cbind(lag1, data_d)
colnames(data_d_lagged) <- c("lag1", "target")

data_d_lagged <- (as.data.frame(data_d_lagged))
train_reg <- data_d_lagged[1:train_length, ]
test_reg <- data_d_lagged[(train_length+1):nrow(data_d_lagged), ]
#model
fit_stepwise <- stepAIC(lm(target ~ ., data=train_reg), direction="both")
summary(fit_stepwise)
AIC(fit_stepwise)
BIC(fit_stepwise)
# Predictions
predictions <- predict(fit_stepwise, newdata=test_reg)

# RMSE
rmse_reg <- sqrt(mean((predictions - test_reg$target)^2, na.rm = TRUE))
rmse_reg
# sMAPE
sMAPE_reg <- mean(2 * abs(test_reg$target - predictions) / (abs(test_reg$target) + abs(predictions)), na.rm = TRUE) * 100
sMAPE_reg
#MSE
mse_reg <- mean((test_reg$target - predictions)^2, na.rm = TRUE)
mse_reg

par(mfrow = c(1,1))

plot(test_reg$target, type = "l", col = "red")
lines(predictions, col = "blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1)

#residuals
residuals_linear <- residuals(fit_stepwise)

# Plotting residuals
plot(residuals_linear, main = "Residuals of Linear Model", ylab = "Residuals")
abline(h = 0, col = "red")

# normality of residuals
hist(residuals_linear, main = "Histogram of Residuals", xlab = "Residuals", breaks = 30)
qqnorm(residuals_linear)
qqline(residuals_linear)

accuracy(predictions, test)

# Manual regression model
lag1 <- stats::lag(data_d, -1)
lag2 <- stats::lag(data_d, -2)
lag3 <- stats::lag(data_d, -3)

data_d_lagged <- cbind(lag1,lag2,lag3, data_d)
colnames(data_d_lagged) <- c("lag1","lag2","lag3", "target")

data_d_lagged <- as.data.frame(data_d_lagged)
train_reg <- data_d_lagged[1:train_length, ]
test_reg <- data_d_lagged[(train_length+1):nrow(data_d_lagged), ]
cor(train_reg)
# model
linear_model_1 <- lm(target ~ ., data=train_reg)
summary(linear_model_1)
AIC(linear_model_1)
BIC(linear_model_1)

# Predictions
predictions <- predict(linear_model_1, newdata=test_reg)
plot(test_reg$target, type = "l", col = "red")
lines(predictions, col = "blue")
legend("topright", legend=c("Actual", "Predicted"), col=c("red", "blue"), lty=1)

par(mfrow = c(1,1))
par(mfrow = c(2,2))
# residuals
residuals_linear <- residuals(linear_model_1)

# Plotting residuals
plot(residuals_linear, main = "Residuals of Linear Model", ylab = "Residuals")
abline(h = 0, col = "red")

# normality of residuals
hist(residuals_linear, main = "Histogram of Residuals", xlab = "Residuals", breaks = 30)
qqnorm(residuals_linear)
qqline(residuals_linear)

# RMSE
rmse_reg1 <- sqrt(mean((predictions - test_reg$target)^2, na.rm = TRUE))
rmse_reg1
# sMAPE
sMAPE_reg1 <- mean(2 * abs(test_reg$target - predictions) / (abs(test_reg$target) + abs(predictions)), na.rm = TRUE) * 100
sMAPE_reg1
#MSE
mse_reg1 <- mean((test_reg$target - predictions)^2, na.rm = TRUE)
mse_reg1

############
#ANN
ann1 <-nnetar(data$DATA)
print(ann1)
accnfcst<-forecast(ann1,h=218)
autoplot(accnfcst)
fcast <- forecast(ann1, PI=TRUE, h=14)
autoplot(fcast)
print(fcast)
autoplot(ann1$residuals, main="Final Model Residuals")
checkresiduals(ann1)
ann_fore_mean <- ts(accnfcst[["mean"]], frequency = 365, start = start(test))
#MSE
mse_ANN <- mean((test - ann_fore_mean)^2, na.rm = TRUE)
mse_ANN
#RSME
rmse_ANN <- sqrt(mean((ann_fore_mean - test)^2, na.rm = TRUE))
rmse_ANN
#sMAPE
sMAPE_ANN <- mean(2 * abs(test - ann_fore_mean) / (abs(test) + abs(ann_fore_mean)), na.rm = TRUE) * 100
sMAPE_ANN
###########

#Naive Forecast
naive_cast = naive(train, h = cast_horizon)
naive_cast
naive_mean = naive_cast[["mean"]]
naive_method = ts(naive_mean, frequency = 365, start = c(1997,218))
plot(naive_cast)
accuracy(naive_cast,test)

naive_rmse = sqrt(mean((naive_method-test)^2))
naive_rmse
smape_error_naive <- mean(2 * abs(test - naive_method) / (abs(test) + abs(naive_method)), na.rm = TRUE) * 100
smape_error_naive
MSE_error_naive <- mean((test - naive_method)^2, na.rm = TRUE)
MSE_error_naive

error_table <- data.frame(Model = c("Manual ETS","Auto ETS",'ARIMA Fit 1','ARIMA Fit
2','ARIMA Fit3','Auto ARIMA','Regression','Naive'),
                          RMSE =
                            c(error_m_ets,error_ets,error_arima_1,error_arima_2,error_arima_3,error_arima_4,error_reg,
                              naive_rmse) ,
                          MAPE =c(MAPE_m_error ,
                                  MAPE_a_error,MAP_error_1,MAP_error_2,MAP_error_3,MAP_error_4,MAPE_error_reg,
                                  MAPE_error_naive))
error_table

h <- 14
origin <- 22
total <- total_length
train_length1 <- data_d_length - h - origin + 1 
test_length1 <- h + origin - 1
data_cast = matrix(NA, nrow=origin, ncol=h)
data_holdout = matrix(NA,nrow = origin , ncol=h)
data_test <- data_d[(test_length1+1):data_d_length]

colnames(data_cast) <- paste0("horizon",c(1:h)) 
rownames(data_cast) <- paste0("origin",c(1:origin))
dimnames(data_holdout) <- dimnames(data_cast)

### 14 day forecast for ETS(ANN)
for(i in 1:origin)
{
  #new training set for each origin 
  origin_train_set = ts(data_d[1:(train_length1+i-1)], 
                        frequency=frequency(data_d),start=start(data_d))
  data_holdout[i,] <- data_test[i-1+(1:h)]
  data_cast[i,] = forecast(es(origin_train_set,model="ANN",persistence=0.05),h=h)$mean 
  
}
data_cast

daily_forecast = ts(colMeans(data_cast),frequency =365,start =end(test))
plot(data_d,type='l', col = "black")
lines(daily_forecast, col = "red")

### 14 day forecast for Auto arima (5,1,2)
for(i in 1:origin)
{
  origin_train_set = ts(data_d[1:(train_length1+i-1)], 
                        frequency=frequency(data_d),start=start(data_d))
  data_holdout[i,] <- data_test[i-1+(1:h)]
  data_cast[i,] = forecast(auto.arima(origin_train_set),h=h)$mean 
  
}
data_cast

daily_forecast = ts(colMeans(data_cast),frequency =365,start =end(test))
plot(data_d,type='l', col = "black")
lines(daily_forecast, col = "red")


### 14 day forecast for ANN
for(i in 1:origin)
{
  origin_train_set = ts(data_d[1:(train_length1+i-1)], 
                        frequency=frequency(data_d),start=start(data_d))
  data_holdout[i,] <- data_test[i-1+(1:h)]
  data_cast[i,] = forecast(nnetar(origin_train_set), PI=TRUE, h=h)$mean 
  
}
data_cast

daily_forecast = ts(colMeans(data_cast),frequency =365,start =end(test))
plot(data_d,type='l', col = "black")
lines(daily_forecast, col = "red")

#Naive Forecast for 14 days
naive_cast_f = naive(train, h = 14)
naive_cast_f
naive_mean = naive_cast_f[["mean"]]
naive_mean
accuracy(naive_method, test)
naive_method = ts(naive_mean, frequency = 365, start = end(train))
plot(naive_cast_f)
naive_rmse_f = sqrt(mean((naive_method-test)^2))
naive_rmse_f
smape_error_naive <- mean(2 * abs(test - naive_method) / (abs(test) + abs(naive_method)), na.rm = TRUE) * 100
smape_error_naive
MSE_error_naive <- mean((test - naive_method)^2, na.rm = TRUE)
MSE_error_naive

# Seasonal Naive Forecast for the original horizon
snaive_cast = snaive(train, h = cast_horizon)
plot(snaive_cast)
accuracy(snaive_cast, test)

snaive_mean = snaive_cast[["mean"]]
snaive_method = ts(snaive_mean, frequency = 365, start = c(1997,218))

snaive_rmse = sqrt(mean((snaive_method - test)^2))
snaive_rmse
snaive_smape_error <- mean(2 * abs(test - snaive_method) / (abs(test) + abs(snaive_method)), na.rm = TRUE) * 100
snaive_smape_error
snaive_MSE_error <- mean((test - snaive_method)^2, na.rm = TRUE)
snaive_MSE_error
# Seasonal Naive Forecast for 14 days
snaive_cast_f = snaive(train, h = 14)
plot(snaive_cast_f)

snaive_mean_f = snaive_cast_f[["mean"]]
snaive_method_f = ts(snaive_mean_f, frequency = 365, start = end(train))

snaive_rmse_f = sqrt(mean((snaive_method_f - test)^2))
snaive_smape_error_f <- mean(2 * abs(test - snaive_method_f) / (abs(test) + abs(snaive_method_f)), na.rm = TRUE) * 100
snaive_MSE_error_f <- mean((test - snaive_method_f)^2, na.rm = TRUE)

