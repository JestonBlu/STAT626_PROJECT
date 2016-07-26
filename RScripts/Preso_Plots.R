rm(list = ls())

## Packages
library(ggplot2)
library(plyr)
library(scales)
library(zoo)

## Each of these Chunk sections are independent of the rest of the script
## you shouldnt need to run the sections before each chunk
## This is so you can tweak the setting for each graph without messing up
## the other graphs

############################################################3
## Chunk 1
##
## Plots:
##   - Seasonally Adjusted Unemployment with sitting president backdrop
##   - Original Plot of Seasonally Unadjusted Variables
##   - Lag Plot of Unemployment
##   - Original Data Plot
##   - Scatterplot of unemployment and Predictors

load("Data/Data_Prep.rda")

## Read in data
econ = read.csv("Data/Unemployment.csv")
usa  = read.csv("Data/US Congress and Recession Data.csv")

## Subset data from Jan 1993 to Dec 2015
econ = na.omit(econ)
econ.sa$date = seq.Date(from = as.Date("1993-01-01"), to = as.Date("2015-12-01"), by = "month")
econ.sa$date = as.Date(econ.sa$date)
usa$date = as.Date(usa$date)
dta = join(econ.sa, usa, by = "date")


## Custom ggplot theme
theme_stat = function() {
  theme(
    plot.title          = element_text(size = 20, face = "bold"),
    panel.background    = element_rect(fill = NA),
    panel.grid.major    = element_line(color = "gray", size = .2),
    panel.grid.minor    = element_line(color = "gray", size = .1),
    panel.border        = element_rect(color = "gray", fill = NA, size = .2),
    axis.ticks          = element_line(size = 0),
    axis.text           = element_text(size = 18, color = "black"),
    axis.title.x        = element_text(size = 20, color = "black"),
    axis.title.y        = element_text(size = 20, color = "black"),
    legend.text         = element_text(size = 18, color = "black"), 
    legend.title        = element_text(size = 20, color = "black")
  )
}

## Base plot
g0 = ggplot(dta)

## Summarise dates of office 
pres = ddply(dta, .(pres), summarise, enter = min(date), exit = max(date))

## Plot presidential terms
g1 = g0 + geom_rect(aes(xmin = enter, xmax = exit, ymin = 0, ymax = .12, fill = pres), 
                    alpha = .2, data = pres)

## Plot recession dates
g2 = g1 + 
  geom_rect(aes(xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-01"), 
                ymin = 0, ymax = .12), color = NA, fill = "gray", alpha = .2) +
  geom_rect(aes(xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-01"), 
                ymin = 0, ymax = .12), color = NA, fill = "gray", alpha = .2)

## Overlay unemployment rate
g3 = g2 + 
  geom_line(aes(x = date, y = unem_rate/100)) +
  scale_x_date("Year", expand = c(0,0)) +
  scale_y_continuous("Unemployment Rate", labels = percent, expand=c(0,0)) +
  scale_fill_discrete("President") +
  ggtitle("Unemployment Rate\n(Jan 93' - Dec 15')") +
  theme_stat()

## Remove seasonal component, plot trend
unem.decom = decompose(ts(data = dta$unem_rate, start = c(1993,1), frequency = 12), type = "additive")

g4 = g2 + 
  geom_line(aes(x = date, y = unem_rate_sa/100)) +
  scale_x_date("", expand = c(0,0)) +
  scale_y_continuous("Unemployment Rate", labels = percent, expand=c(0,0)) +
  scale_fill_discrete("President") +
  ggtitle("") +
  theme_stat()

## Presidential Plot
g4

## Lag Plot
## Borrowed from StackOverflow: http://stackoverflow.com/questions/21524600/
lag.plot1 = function(data1, max.lag = 1, corr = TRUE, smooth = FALSE) { 
  name1 = paste("t-", sep = "")
  data1 = as.ts(data1)
  max.lag = as.integer(max.lag)
  prow = ceiling(sqrt(max.lag))
  pcol = ceiling(max.lag / prow)
  a = acf(data1, max.lag, plot = FALSE)$acf[-1]
  par(mfrow = c(prow, pcol), mar = c(2.5, 4, 2.5, 1), cex.main = 4, font.main = 1)
  for(h in 1:max.lag) {                       
    plot(lag(data1, -h), data1, xy.labels = FALSE, main = paste(name1, h, "",sep = ""), 
         ylab = "", xlab = "", cex = 2.5, cex.axis = 2, cex.main = 2.5) 
    if (smooth == TRUE) 
      lines(lowess(ts.intersect(lag(data1, -h), data1)[, 1], ts.intersect(lag(data1, -h), data1)[, 2],
                   f = 1), col = "red")
    if (corr == TRUE)
      legend("topright", legend = round(a[h], digits = 2), text.col = "blue", bg = "white", cex=1.5, x.intersp = 0, y.intersp = 0)
  }
}

with(econ.sa, lag.plot1(unem_rate_sa, 12, smooth = TRUE))

## Predictor Variable Plots
econ2 = ts(econ.sa[, 1:6], start = c(1993, 1), frequency = 12)


## Seasonally Adjusted Plots
plot.zoo(econ2, main = "", 
         ylab = c("Unemployment", "Industrial Production", "Man. New Orders", 
                  "House Price Index", "Constr. Spend", "Retail Sales"),
         cex.main = 2,
         xlab = "")

econ = ts(econ[, 2:7], start = c(1993, 1), frequency = 12)

## Original Data, no seasonal adjustment
plot.zoo(econ, ylab = c("Unemployment", "Industrial Production", "Man. New Orders", 
                        "House Price Index", "Constr. Spend", "Retail Sales"),
         cex.main = 3, main = "",
         xlab = "")

## Scatterplot
plot(econ.sa[,1:6], c("Unemployment", "Industrial Production", "Man. New Orders", 
                      "House Price Index", "Constr. Spend", "Retail Sales"), cex.labels=1.5, gap=.5, 
                    xaxt='n', yaxt='n')



############################################################3
## Chunk 2
##
## Plots:
##    - Unemployment differencing plots
##    - Predictor variable differencing plots
##    - House Price Index differencing
##    - ACF PACF unemployment
##    - ACF PACF unemployment 2nd differenc

rm(list = ls())

## Packages
library(ggplot2)
library(plyr)
library(scales)
library(forecast)

load("Data/Data_Prep.rda")

## Unemployment Plot Differencing
par(mfrow = c(1,2))
plot(diff(econ.sa$unem_rate_sa, differences = 1), main = "1st Difference", cex.main = 1.5, ylab = "", xlab = "")
plot(diff(econ.sa$unem_rate_sa, differences = 2), main = "2nd Difference", cex.main = 1.5, ylab = "", xlab = "")

## Predictor Variable differencing
par(mfrow = c(2,3))
plot(diff(econ.sa$industrial_production_sa, differences = 2), main = "Ind. Production", cex.main = 1.5, ylab = "", xlab = "")
plot(diff(econ.sa$manufacturers_new_orders_sa, differences = 2), main = "Man. New Orders", cex.main = 1.5, ylab = "", xlab = "")
plot(diff(econ.sa$house_price_sa, differences = 2), main = "House Price Index", cex.main = 1.5, ylab = "", xlab = "")
plot(diff(econ.sa$construction_spend_sa, differences = 2), main = "Constr. Spend", cex.main = 1.5, ylab = "", xlab = "")
plot(diff(econ.sa$retail_sales_sa, differences = 2), main = "Retail Sales", cex.main = 1.5, ylab = "", xlab = "")

## House Price Differecing
par(mfrow = c(1,1))
plot(diff(log(econ.sa$house_price_sa), differences = 2), main = "House Price Index", cex.main = 1.5, ylab = "", xlab = "")

## ACF/PACF Unemployment
par(mfrow = c(1,2))
Acf(econ.sa$unem_rate_sa, main = "ACF")
Pacf(econ.sa$unem_rate_sa, main = "PACF")

## ACF/PACF Unemployment, 2nd difference
par(mfrow = c(1,2))
Acf(diff(econ.sa$unem_rate_sa, differences = 2), main = "ACF")
Pacf(diff(econ.sa$unem_rate_sa, differences = 2), main = "PACF")


rm(list = ls())

############################################################3
## Chunk 3
##
## Plots:
##    - Model 1:  ARIMA(1,2,1) no regressors
##    - Model 7:  ARIMA(1,2,1) lagged regressors
##    - VAR Mdl3: VAR(1) 
##    - 5 month forecast plots
##    - 3 year forecast plots
##    - Residual plots for VAR

library(astsa)
library(xtable)
library(knitr)
library(tseries)
library(forecast)
library(vars)
library(ggplot2)
library(gridExtra)

load("Data/Data_Prep.rda")

###########################################################333
## ARIMA Models

## Models with no regressors
mdl.1 = sarima(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1)

econ.sa.lag = ts.intersect(
  unem_rate_sa = econ.sa$unem_rate_sa,
  industrial_production_sa = lag(econ.sa$industrial_production_sa, -2),
  manufacturers_new_orders_sa = lag(econ.sa$manufacturers_new_orders_sa, -4),
  house_price_sa = lag(econ.sa$house_price_sa, -5),
  construction_spend_sa = lag(econ.sa$construction_spend_sa, -3),
  retail_sales_sa = lag(econ.sa$retail_sales_sa, -0),
  dframe = TRUE
)

econ.sa.lag$recession_ind = econ.sa$recession_ind[6:276]

## Models with lagged Regressors
mdl.7 = sarima(econ.sa.lag$unem_rate_sa, p = 1, d = 2, q = 1, xreg = econ.sa.lag[, 2:6])

## Var Model
mdl.var3 = VAR(y = econ.sa[, c(1,5,6,7)], type = "both", p = 1)


## Prediction
unem = c(econ.sa.lag$unem_rate_sa, unem.16)

## Prediction for best ARIMA
pred.arima = sarima.for(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1, n.ahead = 5)$pred
pred.arima.lwr = pred.arima - (1.96 * sarima.for(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1, n.ahead = 5)$se)
pred.arima.upr = pred.arima + (1.96 * sarima.for(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1, n.ahead = 5)$se)

pred.arima = c(rep(NA, 271), pred.arima)
pred.arima.lwr = c(rep(NA, 271), pred.arima.lwr)
pred.arima.upr = c(rep(NA, 271), pred.arima.upr)

## Prediction for best VAR
pred.var = c(rep(NA, 271), predict(mdl.var3, n.ahead = 5)$fcst$unem_rate_sa[,1])
pred.var.lwr = c(rep(NA, 271), predict(mdl.var3, n.ahead = 5)$fcst$unem_rate_sa[,2])
pred.var.upr = c(rep(NA, 271), predict(mdl.var3, n.ahead = 5)$fcst$unem_rate_sa[,3])


pred = data.frame(
  unemployment = unem[200:276],
  arima.pred = pred.arima[200:276],
  arima.pred.lwr = pred.arima.lwr[200:276],
  arima.pred.upr = pred.arima.upr[200:276],
  var.pred = pred.var[200:276],
  var.pred.lwr = pred.var.lwr[200:276],
  var.pred.upr = pred.var.upr[200:276],
  dt = seq.Date(from = as.Date("2010-01-01"), to = as.Date("2016-05-01"), by = "month")
)

## Best ARIMA Forecast
ggplot(pred, aes(x = dt)) + 
  geom_line(aes(y = unemployment)) +
  geom_point(aes(y = arima.pred), color = "blue") +
  geom_line(aes(y = arima.pred.lwr), color = "blue", lty = 2) +
  geom_line(aes(y = arima.pred.upr), color = "blue", lty = 2) +
  ggtitle("") +
  scale_y_continuous("Unemployment Rate") +
  scale_x_date("")+
  theme_stat()

## Best VAR Forecast
ggplot(pred, aes(x = dt)) + 
  geom_line(aes(y = unemployment)) +
  geom_point(aes(y = var.pred), color = "red") +
  geom_line(aes(y = var.pred.lwr), color = "red", lty = 2) +
  geom_line(aes(y = var.pred.upr), color = "red", lty = 2) +
  ggtitle("") +
  scale_y_continuous("Unemployment Rate") +
  scale_x_date("")+
  theme_stat()

## Shorten the plotted time range and plot both series together
## Best ARIMA Forecast

ggplot(pred, aes(x = dt)) + 
  geom_line(aes(y = unemployment)) +
  geom_point(aes(y = arima.pred), color = "blue") +
  geom_line(aes(y = arima.pred.lwr), color = "blue", lty = 2) +
  geom_line(aes(y = arima.pred.upr), color = "blue", lty = 2) +
  geom_point(aes(y = var.pred), color = "red") +
  geom_line(aes(y = var.pred.lwr), color = "red", lty = 2) +
  geom_line(aes(y = var.pred.upr), color = "red", lty = 2) +
  ggtitle("") +
  scale_y_continuous("Unemployment Rate", limits = c(4, 7)) +
  scale_x_date("", limits = c(as.Date("2014-01-01"), NA)) +
  theme_stat()



####################################################
## Long Term Forecasts

## Prediction for best ARIMA
pred.arima.long = sarima.for(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1, n.ahead = 36)$pred
pred.arima.lwr.long = pred.arima.long - (1.96 * sarima.for(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1, n.ahead = 36)$se)
pred.arima.upr.long = pred.arima.long + (1.96 * sarima.for(econ.sa$unem_rate_sa, p = 1, d = 2, q = 1, n.ahead = 36)$se)

pred.arima.long = c(rep(NA, 271), pred.arima.long)
pred.arima.lwr.long = c(rep(NA, 271), pred.arima.lwr.long)
pred.arima.upr.long = c(rep(NA, 271), pred.arima.upr.long)

## Prediction for best VAR
pred.var.long = c(rep(NA, 271), predict(mdl.var3, n.ahead = 36)$fcst$unem_rate_sa[,1])
pred.var.lwr.long = c(rep(NA, 271), predict(mdl.var3, n.ahead = 36)$fcst$unem_rate_sa[,2])
pred.var.upr.long = c(rep(NA, 271), predict(mdl.var3, n.ahead = 36)$fcst$unem_rate_sa[,3])

pred.long = data.frame(
  unemployment = c(unem, rep(NA, 31)),
  arima.pred = pred.arima.long,
  arima.pred.lwr = pred.arima.lwr.long,
  arima.pred.upr = pred.arima.upr.long,
  var.pred = pred.var.long,
  var.pred.lwr = pred.var.lwr.long,
  var.pred.upr = pred.var.upr.long,
  dt = seq.Date(from = as.Date("1993-01-01"), length.out = 307, by = "month")
)

par(mfrow = c(1,1))
ggplot(pred.long, aes(x = dt)) + 
  geom_line(aes(y = unemployment)) +
  geom_point(aes(y = arima.pred), color = "blue") +
  geom_line(aes(y = arima.pred.lwr), color = "blue", lty = 2) +
  geom_line(aes(y = arima.pred.upr), color = "blue", lty = 2) +
  geom_point(aes(y = var.pred), color = "red") +
  geom_line(aes(y = var.pred.lwr), color = "red", lty = 2) +
  geom_line(aes(y = var.pred.upr), color = "red", lty = 2) +
  ggtitle("") +
  scale_y_continuous("Unemployment Rate", limits = c(0,NA)) +
  scale_x_date("", limits = c(as.Date("1993-01-01"), NA)) +
  theme_stat()


#######################################################
## VAR Residual Plot

par(mfrow = c(2,2))
Acf(residuals(mdl.var3$varresult$unem_rate_sa), main = "Unemployment", cex.main=2)
Ccf(residuals(mdl.var3$varresult$unem_rate_sa), residuals(mdl.var3$varresult$construction_spend_sa), main = "Unemployment vs Construction")
Ccf(residuals(mdl.var3$varresult$unem_rate_sa), residuals(mdl.var3$varresult$retail_sales_sa), main = "Unemployment vs Retail")
Ccf(residuals(mdl.var3$varresult$unem_rate_sa), residuals(mdl.var3$varresult$recession_ind), main = "Unemployment vs Recession")


############################################################3
## Chunk 4
##
## Plots:
##    - Model 1:  ARIMA(1,2,1) no regressors
##    - Model 7:  ARIMA(1,2,1) lagged regressors
##    - VAR Mdl3: VAR(1) 
##    - 5 month forecast plots
##    - 3 year forecast plots
##    - Residual plots for VAR

rm(list = ls())

load("Data/Data_Prep.rda")

econ.sa.st = data.frame(
  unem_rate_sa = diff(econ.sa$unem_rate_sa, differences = 2),
  industrial_production_sa = diff(econ.sa$industrial_production_sa, differences = 2),
  manufacturers_new_orders_sa = diff(econ.sa$manufacturers_new_orders_sa, differences = 2),
  house_price_sa = diff(econ.sa$house_price_sa, differences = 2),
  construction_spend_sa = diff(econ.sa$construction_spend_sa, differences = 2),
  retail_sales_sa = diff(econ.sa$retail_sales_sa, differences = 2),
  recession_ind = diff(econ.sa$recession_ind, differences = 2)
)

par(mfrow = c(2,3))
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$industrial_production_sa, lag.max = 12, main = "Ind. Production")
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$manufacturers_new_orders_sa, lag.max = 12, main = "Man. New Orders")
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$house_price_sa, lag.max = 12, main = "House Price Index")
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$construction_spend_sa, lag.max = 12, main = "Constr. Spend")
Ccf(x = econ.sa.st$unem_rate_sa, y = econ.sa.st$retail_sales_sa, lag.max = 12, main = "Retail Sales")

## Individual Lag Plots (not saved)
par(mfrow = c(2,3))
lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$industrial_production_sa, max.lag = 12)    ## No real lag, setting to 2
lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$manufacturers_new_orders_sa, max.lag = 12) ## Lag 4?
lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$house_price_sa, max.lag = 12)              ## Lag 5?
lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$construction_spend_sa, max.lag = 12)       ## No real lag, setting to 3
lag2.plot(econ.sa.st$unem_rate_sa, econ.sa.st$retail_sales_sa, max.lag = 12)             ## No real lag, setting to 0

## VAR Forcast

par(mfrow = c(1,1))
ggplot(pred.long, aes(x = dt)) + 
  geom_line(aes(y = unemployment)) +
  geom_point(aes(y = var.pred), color = "blue") +
  geom_line(aes(y = var.pred.lwr), color = "blue", lty = 2) +
  geom_line(aes(y = var.pred.upr), color = "blue", lty = 2) +
  ggtitle("") +
  scale_y_continuous("Unemployment Rate", limits = c(0,NA)) +
  scale_x_date("", limits = c(as.Date("2015-01-01"), as.Date("2017-01-01"))) +
  theme_stat()

