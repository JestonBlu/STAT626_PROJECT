rm(list = ls())

library(astsa)
library(forecast)
library(xtable)
library(knitr)
library(tseries)
library(vars)
library(ggplot2)
library(gridExtra)

load("Data_Prep.rda")

old.par <- par()

#par(mfrow=c(2,3))
par(old.par)

par(mfrow=c(2,3))
## Establish stationarity for predictor variables
plot.ts(econ.sa$unem_rate_sa, xlab="", ylab="Unemployment Rate")
plot.ts(econ.sa$industrial_production_sa, xlab="",  ylab="Industrial Production")
plot.ts(econ.sa$manufacturers_new_orders_sa, xlab="",  ylab="New Orders")
plot.ts(econ.sa$house_price_sa, xlab="",  ylab="House Price")
plot.ts(econ.sa$construction_spend_sa, xlab="",  ylab="Construction Spending")
plot.ts(econ.sa$retail_sales_sa, xlab="",  ylab="Retail Sales")

## Establish stationarity for predictor variables
plot.ts(diff(econ.sa$unem_rate_sa, differences = 2), xlab="", ylab="Unemployment Rate, d=2")
plot.ts(diff(econ.sa$industrial_production_sa, differences = 2), xlab="",  ylab="Industrial Production, d=2")
plot.ts(diff(econ.sa$manufacturers_new_orders_sa, differences = 2), xlab="",  ylab="New Orders, d=2")
plot.ts(diff(econ.sa$house_price_sa, differences = 2), xlab="",  ylab="House Price, d=2")
plot.ts(diff(econ.sa$construction_spend_sa, differences = 2), xlab="",  ylab="Construction Spending, d=2")
plot.ts(diff(econ.sa$retail_sales_sa, differences = 2), xlab="",  ylab="Retail Sales, d=2")

par(old.par)

adf.test(diff(econ.sa$unem_rate_sa, differences = 2))
adf.test(diff(econ.sa$industrial_production_sa, differences = 2))
adf.test(diff(econ.sa$manufacturers_new_orders_sa, differences = 2))
adf.test(diff(econ.sa$house_price_sa, differences =2))
adf.test(diff(econ.sa$construction_spend_sa, differences =2))
adf.test(diff(econ.sa$retail_sales_sa, differences =2))

plot.ts(diff(log(econ.sa$house_price_sa), differences = 2), xlab="",  ylab="log(House Price), d=2")


adf.test(diff(log(econ.sa$house_price_sa), differences=2))

econ.sa.st = data.frame(
  unem_rate_sa = diff(econ.sa$unem_rate_sa, differences = 2),
  industrial_production_sa = diff(econ.sa$industrial_production_sa, differences = 2),
  manufacturers_new_orders_sa = diff(econ.sa$manufacturers_new_orders_sa, differences = 2),
  house_price_sa = diff(econ.sa$house_price_sa, differences = 2),
  construction_spend_sa = diff(econ.sa$construction_spend_sa, differences = 2),
  retail_sales_sa = diff(econ.sa$retail_sales_sa, differences = 2),
  recession_ind = diff(econ.sa$recession_ind, differences = 2)
)


Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$industrial_production_sa, lag.max = 12)
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$manufacturers_new_orders_sa, lag.max = 12)
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$house_price_sa, lag.max = 12)
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$construction_spend_sa, lag.max = 12)
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$retail_sales_sa, lag.max = 12)

lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$industrial_production_sa, max.lag = 12)    ## No real lag, setting to 2
lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$manufacturers_new_orders_sa, max.lag = 12) ## Lag 4?
lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$house_price_sa, max.lag = 12)              ## Lag 5?
lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$construction_spend_sa, max.lag = 12)       ## No real lag, setting to 3
lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$retail_sales_sa, max.lag = 12)             ## No real lag, setting to 0

## Not really going in this direction, but does unemployment lead other metrics?
lag2.plot(econ.sa.st$industrial_production_sa, econ.sa.st$unem_rate_sa, max.lag = 12)    ## No
lag2.plot(econ.sa.st$manufacturers_new_orders_sa, econ.sa.st$unem_rate_sa, max.lag = 12) ## No
lag2.plot(econ.sa.st$house_price_sa, econ.sa.st$unem_rate_sa, max.lag = 12)              ## Leading 8,9?
lag2.plot(econ.sa.st$construction_spend_sa, econ.sa.st$unem_rate_sa, max.lag = 12)       ## Leading 9-12?
lag2.plot(econ.sa.st$retail_sales_sa, econ.sa.st$unem_rate_sa, max.lag = 12)             ## Leading 1,6?


econ.sa.lag = ts.intersect(
  unem_rate_sa = econ.sa.st$unem_rate_sa,
  industrial_production_sa = lag(econ.sa.st$industrial_production_sa, -2),
  manufacturers_new_orders_sa = lag(econ.sa.st$manufacturers_new_orders_sa, -4),
  house_price_sa = lag(econ.sa.st$house_price_sa, -5),
  construction_spend_sa = lag(econ.sa.st$construction_spend_sa, -3),
  retail_sales_sa = lag(econ.sa.st$retail_sales_sa, -0),
  dframe = TRUE
)


mdl1 = sarima(xdata = econ.sa.st$unem_rate_sa, p = 1, d = 0, q = 1)                             ## No xRegs
mdl2 = sarima(xdata = econ.sa.st$unem_rate_sa, p = 1, d = 0, q = 1, xreg = econ.sa.st[, 2:7])   ## No lag xRegs
mdl3 = sarima(xdata = econ.sa.lag$unem_rate_sa, p = 1, d = 0, q = 1, xreg = econ.sa.lag[, 2:6]) ## Lagged xRegs

#### Model Comparison
## 
## Model 1: {AIC: -2.617} {BIC: -3.578} *** Best BIC
## Model 2: {AIC: -2.613} {BIC: -3.495}
## Model 3: {AIC: -2.672} {BIC: -3.565} *** Best AIC
##
#### Model 3 Pvalues
##
##                             Estimate     SE  t.value Pvalue
## ar1                          -0.2176 0.0672  -3.2387   .001 ***
## ma1                          -0.8835 0.0411 -21.4938  <.001 ***
## intercept                     0.0001 0.0009   0.1447   .886
## industrial_production_sa     -0.0500 0.0132  -3.7763  <.001 ***
## manufacturers_new_orders_sa  -0.0005 0.0007  -0.6516   .523
## house_price_sa               -0.0413 0.0122  -3.3765  <.001 ***
## construction_spend_sa         0.0120 0.0067   1.7902   .091 
## retail_sales_sa               0.0027 0.0013   2.1645   .044 ***

