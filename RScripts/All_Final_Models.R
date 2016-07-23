rm(list = ls())

library(astsa)
library(xtable)
library(knitr)
library(tseries)
library(forecast)
library(vars)

load("Data/Data_Prep.rda")


## Seasonally adjusted unemployment
unem = econ.sa$unem_rate_sa

## Everyone seems to agree that 2 differences gets us stationarity... since we specify differencing
## in the arima model parameters, it feels like we should not be differencing the data beforehand
##
## Differencing 
unem1 = diff(unem)
unem2 = diff(unem, differences = 2)

## Differencing plots
par(mfrow = c(1, 2))
plot.ts(unem1, main = "First difference")
plot.ts(unem2, main = "Second difference")

#ADF of differenced data
adf.test(unem1)
adf.test(unem2)

## ACF/PACF Plots
par(mfrow = c(1,2))
Acf(unem2); Pacf(unem2)


###########################################################333
## ARIMA Models

## Models with no regressors
mdl.1 = sarima(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1)
mdl.2 = sarima(econ.sa$unem_rate_sa, p = 2, d = 2, q = 2)
mdl.3 = sarima(econ.sa$unem_rate_sa, p = 3, d = 2, q = 3)


econ.sa.lag = ts.intersect(
  unem_rate_sa = econ.sa$unem_rate_sa,
  industrial_production_sa = lag(econ.sa$industrial_production_sa, -2),
  manufacturers_new_orders_sa = lag(econ.sa$manufacturers_new_orders_sa, -4),
  house_price_sa = lag(econ.sa$house_price_sa, -5),
  construction_spend_sa = lag(econ.sa$construction_spend_sa, -3),
  retail_sales_sa = lag(econ.sa$retail_sales_sa, -0),
  dframe = TRUE
)

## Models with Regressors
mdl.4 = sarima(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1, xreg = econ.sa[, 2:6])
mdl.5 = sarima(econ.sa$unem_rate_sa, p = 1, d = 2, q = 2, xreg = econ.sa[, 2:6])
mdl.6 = sarima(econ.sa$unem_rate_sa, p = 3, d = 2, q = 3, xreg = econ.sa[, 2:6])


## Models with lagged Regressors
mdl.7 = sarima(econ.sa.lag$unem_rate_sa, p = 1, d = 2, q = 1, xreg = econ.sa.lag[, 2:6])
mdl.8 = sarima(econ.sa.lag$unem_rate_sa, p = 1, d = 2, q = 2, xreg = econ.sa.lag[, 2:6])
mdl.9 = sarima(econ.sa.lag$unem_rate_sa, p = 3, d = 2, q = 3, xreg = econ.sa.lag[, 2:6])

## Comparison of various models
compare = data.frame(
  Model = c("Mdl.1", "Mdl.2", "Mdl.3", "Mdl.4", "Mdl.5", "Mdl.6", "Mdl.7", "Mdl.8", "Mdl.9"),
  Order = c("1,2,1", "2,2,2", "3,2,3", "1,2,1", "2,2,2", "3,2,3", "1,2,1", "2,2,2", "3,2,3"),
  Xregs = c("","","","Y","Y","Y","","",""),
  Lag.Xregs = c("","","","","","","Y","Y","Y"),
  AIC = c(AIC(mdl.1$fit), AIC(mdl.2$fit), AIC(mdl.3$fit), AIC(mdl.4$fit), AIC(mdl.5$fit),
          AIC(mdl.6$fit), AIC(mdl.7$fit), AIC(mdl.8$fit), AIC(mdl.9$fit)),
  BIC = c(BIC(mdl.1$fit), BIC(mdl.2$fit), BIC(mdl.3$fit), BIC(mdl.4$fit), BIC(mdl.5$fit),
          BIC(mdl.6$fit), BIC(mdl.7$fit), BIC(mdl.8$fit), BIC(mdl.9$fit)),
  Best = c("Best BIC", "", "", "", "", "", "Best AIC", "", "")
)

kable(compare)

xtable(compare)

#############################################################
## VAR Model

## Fit a VAR on non stationary data
mdl.var1 = VAR(y = econ.sa[, 1:6], type = "both", p = 1)
mdl.var2 = VAR(y = econ.sa[, 1:6], type = "both", p = 2)
mdl.var3 = VAR(y = econ.sa[, 1:6], type = "both", p = 3)

## Residuals of the Unemployment rate series
# par(mfrow = c(1,3))
# acf(residuals(mdl.var1)[,1])
# acf(residuals(mdl.var2)[,1])
# acf(residuals(mdl.var3)[,1])
# 
# plot(mdl.var1)
# plot(mdl.var2)
# plot(mdl.var3)
# 
# plot(predict(mdl.var1, n.ahead = 12))
# plot(predict(mdl.var2, n.ahead = 12))
# plot(predict(mdl.var3, n.ahead = 12))


## Fit a VAR on non stationary data
mdl.var4 = VAR(y = econ.sa.lag, type = "both", p = 1)
mdl.var5 = VAR(y = econ.sa.lag, type = "both", p = 2)
mdl.var6 = VAR(y = econ.sa.lag, type = "both", p = 3)

## Residuals of the Unemployment rate series
# par(mfrow = c(1,3))
# acf(residuals(mdl.var4)[,1])
# acf(residuals(mdl.var5)[,1])
# acf(residuals(mdl.var6)[,1])
# 
# plot(mdl.var4)
# plot(mdl.var5)
# plot(mdl.var6)
# 
# plot(predict(mdl.var4, n.ahead = 12))
# plot(predict(mdl.var5, n.ahead = 12))
# plot(predict(mdl.var6, n.ahead = 12))

## Comparison of various models
compare.vars = data.frame(
  Model = c("Mdl.1", "Mdl.2", "Mdl.3", "Mdl.4", "Mdl.5", "Mdl.6"),
  P = c("1","2","3","1","2","3"),
  Lag.Xregs = c("","","","Y","Y","Y"),
  AIC = c(AIC(mdl.var1$varresult$unem_rate_sa), 
          AIC(mdl.var2$varresult$unem_rate_sa), 
          AIC(mdl.var3$varresult$unem_rate_sa), 
          AIC(mdl.var4$varresult$unem_rate_sa), 
          AIC(mdl.var5$varresult$unem_rate_sa), 
          AIC(mdl.var6$varresult$unem_rate_sa)),
  BIC = c(BIC(mdl.var1$varresult$unem_rate_sa), 
          BIC(mdl.var2$varresult$unem_rate_sa), 
          BIC(mdl.var3$varresult$unem_rate_sa), 
          BIC(mdl.var4$varresult$unem_rate_sa), 
          BIC(mdl.var5$varresult$unem_rate_sa), 
          BIC(mdl.var6$varresult$unem_rate_sa)),
  Best = c("Best BIC", "", "", "", "Best AIC", "")
)


#############################################################
## Best Models

compare.best = data.frame(
  Model = c("ARIMA(1,2,1)", "ARIMA(1,2,1)", "VAR(1)", "VAR(2)"),
  Lag.XRegs = c("", "Y", "", "Y"),
  AIC = c(-212.29, -222.45, -226.34, -235.43),
  BIC = c(-201.45, -193.69, -193.79, -181.51),
  Best = c("Best BIC", "", "", "Best AIC")
)

#############################################################
## Forecasting
