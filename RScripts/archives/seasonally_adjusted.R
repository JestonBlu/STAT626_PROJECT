library(astsa)

load("Data/Data_Prep.rda")

## Seasonally adjusted unemployment rate
unem = econ.sa$unem_rate_sa

par(mfrow = c(2, 2))
plot.ts(unem)
plot.ts(diff(unem, differences = 1))
plot.ts(diff(unem, differences = 2))
plot.ts(diff(unem, differences = 3))

par(mfrow = c(1, 2))
acf(diff(unem, differences = 2))
pacf(diff(unem, differences = 2))

mdl1 = sarima(xdata = unem, p = 0, d = 2, q = 1)
mdl2 = sarima(xdata = unem, p = 0, d = 2, q = 2)
mdl3 = sarima(xdata = unem, p = 0, d = 2, q = 3)
mdl4 = sarima(xdata = unem, p = 1, d = 2, q = 1)
mdl5 = sarima(xdata = unem, p = 1, d = 2, q = 2)
mdl6 = sarima(xdata = unem, p = 1, d = 2, q = 3)
mdl7 = sarima(xdata = unem, p = 2, d = 2, q = 1)
mdl8 = sarima(xdata = unem, p = 2, d = 2, q = 2)
mdl9 = sarima(xdata = unem, p = 2, d = 2, q = 3)

mdl1$AIC
mdl2$AIC
mdl3$AIC
mdl4$AIC ## Very Close 2nd
mdl5$AIC
mdl6$AIC
mdl7$AIC
mdl8$AIC
mdl9$AIC ## Winner

mdl1$BIC
mdl2$BIC
mdl3$BIC
mdl4$BIC ## Winner
mdl5$BIC
mdl6$BIC
mdl7$BIC
mdl8$BIC
mdl9$BIC

## Final model from the bunch would be model 4
## Model4 AIC is very close to Model9 and a simpler model would be preferred


