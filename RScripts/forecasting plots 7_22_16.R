# Forecasting plots

library(xlsx)
library(astsa)
library(TSA)
library(vars)
library(forecast)

# Load the seasonally adjusted data
load("C:\\Users\\LILLEYT\\Downloads\\Data_Prep (7).rda")
names(econ.sa) <- c("unem", "ipi", "orders", "house", "constr", "retail", "recession")
attach(econ.sa)

# All these plots are forecasts with the ARIMA(1, 2, 1) model with seasonally adjusted data
# and no predictors

# Also, if we want to include it, the FRED website has posted the unemployment rate
# for June 2016 as 5.1%

# Here are the basic sarima( ) plots for the forecasts, h = 5, 6, 12, and 24
sarima.for(unem, p = 1, d = 2, q = 1, n.ahead = 5)
sarima.for(unem, p = 1, d = 2, q = 1, n.ahead = 6)
sarima.for(unem, p = 1, d = 2, q = 1, n.ahead = 12)
sarima.for(unem, p = 1, d = 2, q = 1, n.ahead = 24)

# Here are the basic Arima( ) plots for the forecasts, h = 5, 6, 12, and 24
fit <- Arima(unem, order = c(1, 2, 1))
plot(forecast(fit, h = 5))
plot(forecast(fit, h = 6))
plot(forecast(fit, h = 12))
plot(forecast(fit, h = 24))

# Here are the sarima( ) plots with the forecasts and the predicted values
# for h = 5, and h = 6
sarima.for(unem, p = 1, d = 2, q = 1, n.ahead = 5)
points(seq(2015+11/12, 2016+4/12, by = 1/12), c(5.080477, 5.3, 5.2, 5.1, 4.7, 4.5))
lines(seq(2015+11/12, 2016+4/12, by = 1/12), c(5.080477, 5.3, 5.2, 5.1, 4.7, 4.5))

sarima.for(unem, p = 1, d = 2, q = 1, n.ahead = 6)
points(seq(2015+11/12, 2016+5/12, by = 1/12), c(5.080477, 5.3, 5.2, 5.1, 4.7, 4.5, 5.1))
lines(seq(2015+11/12, 2016+5/12, by = 1/12), c(5.080477, 5.3, 5.2, 5.1, 4.7, 4.5, 5.1))

# Here are the Arima( ) plots with the forecasts and the predicted values
# for h = 5, and h = 6
plot(forecast(fit, h = 5))
lines(seq(2015+11/12, 2016+4/12, by = 1/12), c(5.080477, 5.3, 5.2, 5.1, 4.7, 4.5))

plot(forecast(fit, h = 6))
lines(seq(2015+11/12, 2016+5/12, by = 1/12), c(5.080477, 5.3, 5.2, 5.1, 4.7, 4.5, 5.1))
