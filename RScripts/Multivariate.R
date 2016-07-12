library(astsa)
library(forecast)

load("Data/Data_Prep.rda")

## Establish stationarity for predictor variables
plot.ts(diff(econ.sa$unem_rate_sa, differences = 2))
plot.ts(diff(econ.sa$industrial_production_sa, differences = 2))
plot.ts(diff(econ.sa$manufacturers_new_orders_sa, differences = 2))
plot.ts(diff(econ.sa$house_price_sa, differences = 2))
plot.ts(diff(econ.sa$construction_spend_sa, differences = 2))
plot.ts(diff(econ.sa$retail_sales_sa, differences = 2))

econ.sa.st = data.frame(
  unem_rate_sa = diff(econ.sa$unem_rate_sa, differences = 2),
  industrial_production_sa = diff(econ.sa$industrial_production_sa, differences = 2),
  manufacturers_new_orders_sa = diff(econ.sa$manufacturers_new_orders_sa, differences = 2),
  house_price_sa = diff(econ.sa$house_price_sa, differences = 2),
  construction_spend_sa = diff(econ.sa$construction_spend_sa, differences = 2),
  retail_sales_sa = diff(econ.sa$retail_sales_sa, differences = 2),
  recession_ind = diff(econ.sa$recession_ind, differences = 2)
)

econ.sa.lag = ts.intersect(
  unem_rate_sa = econ.sa.st$unem_rate_sa,
  industrial_production_sa = lag(econ.sa.st$industrial_production_sa, -2),
  manufacturers_new_orders_sa = lag(econ.sa.st$manufacturers_new_orders_sa, -2),
  house_price_sa = lag(econ.sa.st$house_price_sa, -2),
  construction_spend_sa = lag(econ.sa.st$construction_spend_sa, -2),
  retail_sales_sa = lag(econ.sa.st$retail_sales_sa, -2),
  dframe = TRUE
)


Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$industrial_production_sa, lag.max = 12)
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$manufacturers_new_orders_sa, lag.max = 12)
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$house_price_sa, lag.max = 12)
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$construction_spend_sa, lag.max = 12)
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$retail_sales_sa, lag.max = 12)

mdl1 = sarima(xdata = econ.sa.st$unem_rate_sa, p = 1, d = 0, q = 1)
mdl2 = sarima(xdata = econ.sa.st$unem_rate_sa, p = 1, d = 0, q = 1, xreg = econ.sa.st[, 2:7])

mdl3 = sarima(xdata = econ.sa.lag$unem_rate_sa, p = 1, d = 0, q = 1)
mdl4 = sarima(xdata = econ.sa.lag$unem_rate_sa, p = 1, d = 0, q = 1, xreg = econ.sa.lag[, 2:6], no.constant = T)
