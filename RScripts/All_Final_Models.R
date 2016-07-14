library(astsa)
library(xtable)
library(knitr)
library(tseries)

load("Data/Data_Prep.rda")

## Unemployment rate with seasonality
unem = econ$unem_rate
unem = ts(unem, start = c(1993,1), frequency = 12)

## Seasonally adjusted unemployment
unem.sa = econ.sa$unem_rate_sa

##Plotting first differences also (for write-up)
unem1 = diff(diff(unem1), lag = 12)
unem1.sa = diff(unem1.sa)
par(mfrow = c(1, 2))
plot.ts(unem1, main = "First differences, Seasonal")
plot.ts(unem1.sa, main = "First differences, Seasonally Adjusted")

## Everyone seems to agree that 2 differences gets us stationarity... since we specify differencing
## in the arima model parameters, it feels like we should not be differencing the data beforehand
##
## Differencing 
unem = diff(diff(unem, differences = 2), lag = 12)
unem.sa = diff(unem.sa, differences = 2)




## Stationary plots
par(mfrow = c(1, 2))
plot.ts(unem, main = "Second differences, Seasonal")
plot.ts(unem.sa, main = "Second differences, Seasonally Adjusted")

#ADF of differenced data
adf.test(unem1)
adf.test(unem1.sa)
adf.test(unem)
adf.test(unem.sa)

## ACF/PACF Plots
par(mfrow = c(2,2))
acf(unem); pacf(unem)
acf(unem.sa); pacf(unem.sa)

## Seasonal Models
mdl.1 = sarima(econ$unem_rate, p = 0, d = 2, q = 1, P = 1, D = 1, Q = 0, S = 12)
mdl.2 = sarima(econ$unem_rate, p = 0, d = 2, q = 1, P = 3, D = 1, Q = 0, S = 12)
mdl.3 = sarima(econ$unem_rate, p = 4, d = 2, q = 1, P = 3, D = 1, Q = 0, S = 12)

## Seasonally Adjusted Models
mdl.4 = sarima(econ.sa$unem_rate_sa, p = 0, d = 2, q = 1, P = 1, D = 0, Q = 0, S = 12)
mdl.5 = sarima(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1)

## Seaonally Adjusted Models with Regressors
mdl.6 = sarima(econ.sa$unem_rate_sa, p = 0, d = 2, q = 1, P = 1, D = 0, Q = 0, S = 12, xreg = econ[, 2:6])
mdl.7 = sarima(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1, xreg = econ[, 2:7])


## Comparison of various models
compare = data.frame(
  Data = c("Unem", "Unem", "Unem", "Unem.sa", "Unem.sa", "Unem.sa", "Unem.sa"),
  Model = c("Mdl.1", "Mdl.2", "Mdl.3", "Mdl.4", "Mdl.5", "Mdl.6", "Mdl.7"),
  Order = c("0,2,1", "0,2,1", "4,2,1", "0,2,1", "1,2,1", "0,2,1", "1,2,1"),
  Seasonal.Order = c("1,1,0", "3,1,0", "3,1,0", "1,0,0", NA, "1,0,0", NA),
  Xregs = c("N", "N", "N", "N", "N", "Y", "Y"),
  AIC = c(mdl.1$AIC, mdl.2$AIC, mdl.3$AIC, mdl.4$AIC, mdl.5$AIC, mdl.6$AIC, mdl.7$AIC),
  BIC = c(mdl.1$BIC, mdl.2$BIC, mdl.3$BIC, mdl.4$BIC, mdl.5$BIC, mdl.6$BIC, mdl.7$BIC)
)

kable(compare)
xtable(compare)
