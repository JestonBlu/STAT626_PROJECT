rm(list = ls())

library(vars)


load("Data/Data_Prep.rda")


## Fit a VAR on non stationary data
mdl.var1 = VAR(y = econ.sa[, 1:6], type = "both", p = 1)
mdl.var2 = VAR(y = econ.sa[, 1:6], type = "both", p = 2)
mdl.var3 = VAR(y = econ.sa[, 1:6], type = "both", p = 3)

par(mfrow = c(1,3))
acf(residuals(mdl.var1)[,1])
acf(residuals(mdl.var2)[,1])
acf(residuals(mdl.var3)[,1])

plot(mdl.var1)
plot(mdl.var2)
plot(mdl.var3)

plot(predict(mdl.var1, n.ahead = 12))
plot(predict(mdl.var2, n.ahead = 12))
plot(predict(mdl.var3, n.ahead = 12))

#######################################################################################

## Using unique lags from the multivariate script
econ.sa.lag = ts.intersect(
  unem_rate_sa = econ.sa$unem_rate_sa,
  industrial_production_sa = lag(econ.sa$industrial_production_sa, -2),
  manufacturers_new_orders_sa = lag(econ.sa$manufacturers_new_orders_sa, -4),
  house_price_sa = lag(econ.sa$house_price_sa, -5),
  construction_spend_sa = lag(econ.sa$construction_spend_sa, -3),
  retail_sales_sa = lag(econ.sa$retail_sales_sa, -0),
  dframe = TRUE
)

## Fit a VAR on non stationary data
mdl.var4 = VAR(y = econ.sa.lag, type = "both", p = 1)
mdl.var5 = VAR(y = econ.sa.lag, type = "both", p = 2)
mdl.var6 = VAR(y = econ.sa.lag, type = "both", p = 3)

## Residuals of the Unemployment rate series
par(mfrow = c(1,3))
acf(residuals(mdl.var4)[,1])
acf(residuals(mdl.var5)[,1])
acf(residuals(mdl.var6)[,1])

plot(mdl.var4)
plot(mdl.var5)
plot(mdl.var6)

plot(predict(mdl.var4, n.ahead = 12))
plot(predict(mdl.var5, n.ahead = 12))
plot(predict(mdl.var6, n.ahead = 12))


## AIC/BIC Extraction
AIC(mdl.var1$varresult$unem_rate_sa)
AIC(mdl.var2$varresult$unem_rate_sa)
AIC(mdl.var3$varresult$unem_rate_sa)
AIC(mdl.var4$varresult$unem_rate_sa)
AIC(mdl.var5$varresult$unem_rate_sa)
AIC(mdl.var6$varresult$unem_rate_sa)

BIC(mdl.var1$varresult$unem_rate_sa)
BIC(mdl.var2$varresult$unem_rate_sa)
BIC(mdl.var3$varresult$unem_rate_sa)
BIC(mdl.var4$varresult$unem_rate_sa)
BIC(mdl.var5$varresult$unem_rate_sa)
BIC(mdl.var6$varresult$unem_rate_sa)

#######################################################################################

## Detrending the data
# econ.sa.lag = data.frame(
#   unem_rate_sa = econ.sa.lag$unem_rate_sa - fitted(lm(econ.sa.lag$unem_rate_sa ~ time(econ.sa.lag$unem_rate_sa))),
#   industrial_production_sa = econ.sa.lag$industrial_production_sa - fitted(lm(econ.sa.lag$industrial_production_sa ~ time(econ.sa.lag$industrial_production_sa))),
#   manufacturers_new_orders_sa = econ.sa.lag$manufacturers_new_orders_sa - fitted(lm(econ.sa.lag$manufacturers_new_orders_sa ~ time(econ.sa.lag$manufacturers_new_orders_sa))),
#   house_price_sa = econ.sa.lag$house_price_sa - fitted(lm(econ.sa.lag$house_price_sa ~ time(econ.sa.lag$house_price_sa))),
#   construction_spend_sa = econ.sa.lag$construction_spend_sa - fitted(lm(econ.sa.lag$construction_spend_sa ~ time(econ.sa.lag$construction_spend_sa))),
#   retail_sales_sa = econ.sa.lag$retail_sales_sa - fitted(lm(econ.sa.lag$retail_sales_sa ~ time(econ.sa.lag$retail_sales_sa)))
# )

## Make stationary
econ.sa.st = data.frame(
  unem_rate_sa = diff(econ.sa.lag$unem_rate_sa, differences = 2),
  industrial_production_sa = diff(econ.sa.lag$industrial_production_sa, differences = 2),
  manufacturers_new_orders_sa = diff(econ.sa.lag$manufacturers_new_orders_sa, differences = 2),
  house_price_sa = diff(econ.sa.lag$house_price_sa, differences = 2),
  construction_spend_sa = diff(econ.sa.lag$construction_spend_sa, differences = 2),
  retail_sales_sa = diff(econ.sa.lag$retail_sales_sa, differences = 2)
)

## Fit a VAR on non stationary data
mdl.var = VAR(y = econ.sa.st, type = "none", p = 1)
mdl.var2 = VAR(y = econ.sa.st, type = "none", p = 2)
acf(residuals(mdl.var)[,1])
acf(residuals(mdl.var2)[,1])
plot(mdl.var$varresult$unem_rate_sa)
plot(predict(mdl.var, n.ahead = 12))
plot(predict(mdl.var2, n.ahead = 12))


#######################################################################################