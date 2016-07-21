rm(list = ls())

library(vars)

load("Data/Data_Prep.rda")


## Fit a VAR on non stationary data
mdl.var = VAR(y = econ.sa[, 1:6], type = "both", p = 1, lag.max = 1)
acf(residuals(mdl.var))
plot(predict(mdl.var, n.ahead = 50))


## Detrend the data
econ.detrend = econ.sa

econ.detrend$unem_rate_sa = with(econ.detrend, unem_rate_sa - fitted(lm(unem_rate_sa ~ time(unem_rate_sa))))
econ.detrend$industrial_production_sa = with(econ.detrend, industrial_production_sa - fitted(lm(unem_rate_sa ~ time(industrial_production_sa))))
econ.detrend$manufacturers_new_orders_sa = with(econ.detrend, manufacturers_new_orders_sa - fitted(lm(manufacturers_new_orders_sa ~ time(manufacturers_new_orders_sa))))
econ.detrend$house_price_sa = with(econ.detrend, house_price_sa - fitted(lm(house_price_sa ~ time(house_price_sa))))
econ.detrend$construction_spend_sa = with(econ.detrend, construction_spend_sa - fitted(lm(construction_spend_sa ~ time(construction_spend_sa))))
econ.detrend$retail_sales_sa = with(econ.detrend, retail_sales_sa - fitted(lm(retail_sales_sa ~ time(retail_sales_sa))))

plot.ts(econ.detrend)

## Fit a VAR on non stationary, but detrended data
mdl.var = VAR(y = econ.detrend[, 1:6], type = "none", p = 1, lag.max = 1)
acf(residuals(mdl.var))
plot(predict(mdl.var, n.ahead = 50))
