library(fpp)
library(TSA)
library(astsa)
library(forecast)
library(MTS)

## Import original Data
econ = read.csv("Unemployment.csv")

## Format Date as Date
econ$date = as.Date(econ$date)

## Omit missing date, date range kept (Jan 1993 - Dec 2015)
econ = na.omit(econ)

## Scale variable that are measured in dollars
econ$manufacturers_new_orders = scale(econ$manufacturers_new_orders)
econ$construction_spending = scale(econ$construction_spending)
econ$retail_sales = scale(econ$retail_sales)

## Decompose the data so we can remove the seasonal adjustments
unem.decom = decompose(ts(data = econ$unem_rate, start = c(1993,1), frequency = 12), type = "additive")
indprod.decom = decompose(ts(data = econ$industrial_production_index, start = c(1993,1), frequency = 12), type = "additive")
mannew.decom = decompose(ts(data = econ$manufacturers_new_orders, start = c(1993,1), frequency = 12), type = "additive")
constsp.decom = decompose(ts(data = econ$construction_spending, start = c(1993,1), frequency = 12), type = "additive")
retail.decom = decompose(ts(data = econ$retail_sales, start = c(1993,1), frequency = 12), type = "additive")
phouse.decom = decompose(ts(data = econ$purchase_house_price_index, start = c(1993,1), frequency = 12), type = "additive")


## Add seasonally adjusted rate
econ$unem_rate_sa = unem.decom$x - unem.decom$sea
econ$ind_prod_sa = indprod.decom$x - indprod.decom$sea
econ$man_new_sa = mannew.decom$x - mannew.decom$sea
econ$cont_spend_sa = constsp.decom$x - constsp.decom$sea
econ$retail_sa = retail.decom$x - retail.decom$sea
econ$p_house_sa = phouse.decom$x - phouse.decom$sea


adf.test(diff(diff(econ$unem_rate_sa)))
adf.test(diff(econ$ind_prod_sa))
adf.test(diff(econ$man_new_sa))
adf.test(diff(econ$cont_spend_sa))
adf.test(diff(econ$purchase_house_price_index))
adf.test(diff(econ$retail_sa))

st_sa_series = cbind(diff(diff(econ$unem_rate_sa)),diff(diff(econ$ind_prod_sa)),diff(diff(econ$man_new_sa)),
                     diff(diff(econ$cont_spend_sa)),diff(diff(log(econ$purchase_house_price_index))),
                     diff(diff(econ$retail_sa)))

###### for regression model building #########
#lag1.plot(st_sa_series[,1],12)
#lag2.plot(st_sa_series[,2], st_sa_series[,1],12)
#lag2.plot(st_sa_series[,3], st_sa_series[,1],12)
#lag2.plot(st_sa_series[,4], st_sa_series[,1],12)
#lag2.plot(st_sa_series[,5], st_sa_series[,1],12)
#lag2.plot(st_sa_series[,6], st_sa_series[,1],12)

#acf(diff(diff((econ[,8]))))

#lag1.plot(econ[,8],12)
#lag2.plot(econ[,3], econ[,2],12)
#lag2.plot(econ[,7], econ[,2],12)
#lag2.plot(econ[,4], econ[,2],12)
#lag2.plot(econ[,5], econ[,2],12)
#lag2.plot(econ[,6], econ[,2],12)
###############################################

## Season adjusted acf and pacf plots on stationary unemp series
acf2(st_sa_series[,1])

## Season adjusted SARIMA fit and Forecast
sarima(econ$unem_rate_sa,0,2,1,1,0,0,12, xreg=econ[,9:13])
sarima.for(econ$unem_rate_sa,24,0,2,1,1,0,0,12)

## SARIMA (1,2,1,1,1,1,12) without external regressors

plot(forecast(arima(econ[1:250,2],order=c(0,2,1), 
                    seasonal=list(order=c(1,1,1),period=12))))


## SARIMA (1,2,1,1,1,1,12) with external regressors

plot(forecast(arima(econ[1:250,2],order=c(1,2,1), 
                    seasonal=list(order=c(1,1,1),period=12),
                    xreg=econ[1:250,3:7]),
              xreg=econ[1:24,3:7]))

## Season adjusted auto arima fit with forecast

fit=auto.arima(st_sa_series[,1],seasonal=T,xreg=st_sa_series[,2:6])
plot(forecast(fit,xreg=st_sa_series[1:24,2:6]), main="Forecast Non seasonal stationary Unemp_Rate Series")

