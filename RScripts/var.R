rm(list = ls())

library(vars)
library(ggplot2)


load("Data/Data_Prep.rda")

## Fit a VAR on non stationary data
mdl.var1 = VAR(y = econ.sa[, c(1, 3,5,6)], type = "both", p = 1)
mdl.var2 = VAR(y = econ.sa[, c(1, 3,5,6)], type = "both", p = 2)
mdl.var3 = VAR(y = econ.sa[, c(1, 3,5,6)], type = "both", p = 3)

# par(mfrow = c(1,3))
# acf(residuals(mdl.var1)[,1])
# acf(residuals(mdl.var2)[,1])
# acf(residuals(mdl.var3)[,1])
# 
# plot(mdl.var1)
# plot(mdl.var2)
# plot(mdl.var3)
# 
# plot(predict(mdl.var1, n.ahead = 5))
# plot(predict(mdl.var2, n.ahead = 5))
# plot(predict(mdl.var3, n.ahead = 5))

#######################################################################################

## Using unique lags from the multivariate script
econ.sa.lag = ts.intersect(
  unem_rate_sa = econ.sa$unem_rate_sa,
  industrial_production_sa = lag(econ.sa$industrial_production_sa, -2),
  manufacturers_new_orders_sa = lag(econ.sa$manufacturers_new_orders_sa, -4),
  house_price_sa = lag(econ.sa$house_price_sa, -5),
  construction_spend_sa = lag(econ.sa$construction_spend_sa, -3),
  retail_sales_sa = econ.sa$retail_sales_sa,
  dframe = TRUE
)

## Fit a VAR on non stationary data
mdl.var4 = VAR(y = econ.sa.lag[, c(1, 3,5,6)], type = "both", p = 1)
mdl.var5 = VAR(y = econ.sa.lag[, c(1, 3,5,6)], type = "both", p = 2)
mdl.var6 = VAR(y = econ.sa.lag[, c(1, 3,5,6)], type = "both", p = 3)

## Residuals of the Unemployment rate series
# par(mfrow = c(1,3))
# acf(residuals(mdl.var4)[,1], main = "VAR(1): Unemployment Residuals")
# acf(residuals(mdl.var5)[,1], main = "VAR(2): Unemployment Residuals")
# acf(residuals(mdl.var6)[,1], main = "VAR(3): Unemployment Residuals")
# 
# plot(mdl.var4)
# plot(mdl.var5)
# plot(mdl.var6)
# 
# plot(predict(mdl.var4, n.ahead = 5))
# plot(predict(mdl.var5, n.ahead = 5))
# plot(predict(mdl.var6, n.ahead = 36))

predict(mdl.var4, n.head = 5)


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

## Prediction
unem = c(econ.sa.lag$unem_rate_sa, unem.16)
pred = c(rep(NA, 271), predict(mdl.var1, n.head = 5)$fcst$unem_rate_sa[1:5,1])
pred.lwr = c(rep(NA, 271), predict(mdl.var1, n.head = 5)$fcst$unem_rate_sa[1:5,2])
pred.upr = c(rep(NA, 271), predict(mdl.var1, n.head = 5)$fcst$unem_rate_sa[1:5,3])


pred = data.frame(
  unemployment = unem[200:276],
  var.pred = pred[200:276],
  var.pred.lwr = pred.lwr[200:276],
  var.pred.upr =pred.upr[200:276],
  dt = seq.Date(from = as.Date("2010-01-01"), to = as.Date("2016-05-01"), by = "month")
)

ggplot(pred, aes(x = dt)) + 
  geom_line(aes(y = unemployment)) +
  geom_point(aes(y = var.pred), color = "blue") +
  geom_line(aes(y = var.pred.lwr), color = "blue", lty = 2) +
  geom_line(aes(y = var.pred.upr), color = "blue", lty = 2) +
  ggtitle("VAR(2) 5 Month Forecast with 95% CI") +
  scale_y_continuous("Unemployment Rate") +
  scale_x_date("")



## Prediction for best VAR
pred.var.long = c(rep(NA, 271), predict(mdl.var4, n.ahead = 36)$fcst$unem_rate_sa[,1])
pred.var.lwr.long = c(rep(NA, 271), predict(mdl.var4, n.ahead = 36)$fcst$unem_rate_sa[,2])
pred.var.upr.long = c(rep(NA, 271), predict(mdl.var4, n.ahead = 36)$fcst$unem_rate_sa[,3])

pred.long = data.frame(
  unemployment = c(unem, rep(NA, 31)),
  var.pred = pred.var.long,
  var.pred.lwr = pred.var.lwr.long,
  var.pred.upr = pred.var.upr.long,
  dt = seq.Date(from = as.Date("1993-01-01"), length.out = 307, by = "month")
)

par(mfrow = c(1,1))
ggplot(pred.long, aes(x = dt)) + 
  geom_line(aes(y = unemployment)) +
  geom_point(aes(y = var.pred), color = "red") +
  geom_line(aes(y = var.pred.lwr), color = "red", lty = 2) +
  geom_line(aes(y = var.pred.upr), color = "red", lty = 2) +
  ggtitle("3 Year Forecast\nARIMA(1,2,1)(blue), VAR(2)(red)") +
  scale_y_continuous("Unemployment Rate", limits = c(0,NA)) +
  scale_x_date("", limits = c(as.Date("1993-01-01"), NA))



#######################################################################################
## Additional choices I did not use
##
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
# econ.sa.st = data.frame(
#   unem_rate_sa = diff(econ.sa.lag$unem_rate_sa, differences = 2),
#   industrial_production_sa = diff(econ.sa.lag$industrial_production_sa, differences = 2),
#   manufacturers_new_orders_sa = diff(econ.sa.lag$manufacturers_new_orders_sa, differences = 2),
#   house_price_sa = diff(econ.sa.lag$house_price_sa, differences = 2),
#   construction_spend_sa = diff(econ.sa.lag$construction_spend_sa, differences = 2),
#   retail_sales_sa = diff(econ.sa.lag$retail_sales_sa, differences = 2)
# )
# 
# ## Fit a VAR on non stationary data
# mdl.var = VAR(y = econ.sa.st, type = "none", p = 1)
# mdl.var2 = VAR(y = econ.sa.st, type = "none", p = 2)
# acf(residuals(mdl.var)[,1])
# acf(residuals(mdl.var2)[,1])
# plot(mdl.var$varresult$unem_rate_sa)
# plot(predict(mdl.var, n.ahead = 12))
# plot(predict(mdl.var2, n.ahead = 12))


#######################################################################################