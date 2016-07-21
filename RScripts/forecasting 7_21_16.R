STAT626_PROJECT/Data/Data_Prep.rda

library(xlsx)
library(astsa)
library(TSA)
library(vars)
library(forecast)

# Load the seasonally adjusted data
load("C:\\Users\\LILLEYT\\Downloads\\Data_Prep (7).rda")
names(econ.sa) <- c("unem", "ipi", "orders", "house", "constr", "retail", "recession")
attach(econ.sa)

# From all the models discussed thus far, I vote for ARIMA(1, 2, 1)
# It had the lowest AIC, best diagnostics, and greatest parsimony.
sarima(unem, p = 1, d = 2, q = 1)

# I then checked if our extra x regressors would help basic ARIMA(1, 2, 1)
# by using forward selection.

# None of these regressors are significant
sarima(unem, p = 1, d = 2, q = 1, xreg = ipi) 
sarima(unem, p = 1, d = 2, q = 1, xreg = orders)
sarima(unem, p = 1, d = 2, q = 1, xreg = house)
sarima(unem, p = 1, d = 2, q = 1, xreg = constr)
sarima(unem, p = 1, d = 2, q = 1, xreg = recession)

# When retail is added, it is significant, and the AIC is
# decreased, so I put it in the model.
sarima(unem, p = 1, d = 2, q = 1, xreg = retail)

# Continued with forward selection by adding additional 
# regressors along with retail. None were significant. 
sarima(unem, p = 1, d = 2, q = 1, xreg = c(retail, ipi))
sarima(unem, p = 1, d = 2, q = 1, xreg = c(retail, orders))
sarima(unem, p = 1, d = 2, q = 1, xreg = c(retail, house))
sarima(unem, p = 1, d = 2, q = 1, xreg = c(retail, constr))
sarima(unem, p = 1, d = 2, q = 1, xreg = c(retail, recession))

# Even though ipi is not significant, its presence does
# decrease the AIC. So I figured I would keep it around just in case.

# Adding anything beyond retail and ipi doesn't improve the model.
# Retail is always significant whenever it's in the model, and it
# decreases the AIC, so it's probably the most important predictor.
# However, its coefficient, while significant, is very small (0.0030).

# Someone can do backward selection if they want. I couldn't figure out how to
# do any selection with the stepAIC( ) or stepwise( ) functions, so I just did it manually.

# To summarize, after looking at regressors, I think we have three potential models:
# ARIMA(1, 2, 1) with no regressors
# ARIMA(1, 2, 1) with retail as a regressor
# ARIMA(1, 2, 1) with retail and ipi as regressors

# Personally, I like the ARIMA(1, 2, 1) with no regressors. These are my reasons.
# It's a valid model with great diagnostics.
# It's the most parsimonious.
# While retail is statistically significant, I argue that it isn't practially significant,
# because its coefficient is so small.
# The forecasted values for unem from the basic ARIMA(1, 2, 1) are very close to the actual ones
# observed in 2016. (See below).

# Here is the forecasting for all three ARIMA(1, 2, 1) models discussed before.

# Read in 2016 data
econ <- read.xlsx("C:\\Users\\LILLEYT\\Desktop\\Unemployment.xlsx", sheetName = "UNRATENSA")

# Convert retail to billions
econ$retail_sales <- econ$retail_sales / 1000

# Note about seasonal adjusting: 
# Using Joseph's code, the decompose( ) function doesn't like to 
# do seasonal decomposition unless you have complete cycles of data.
# We only have 3 retail numbers for 2016 (not the whole year),
# and only 4 ipi numbers for 2016, so the decompose( ) function did strange things. 
# I don't know how to seasonally adjust only the 2016 values for 
# retail and ipi. If someone could figure out how to seasonally adjust them
# that would help. I don't think it's a huge deal though.

# Here is the forecasting for the ARIMA(1, 2, 1) with no regressors
# I did five forecasts ahead, because we have those actual unem numbers.
# Then I did forecasts for one year, and two years. I think going
# beyond two years might be too far. 
sarima.for(unem, p = 1, d = 2, q = 1, n.ahead = 5)
sarima.for(unem, p = 1, d = 2, q = 1, n.ahead = 12)
sarima.for(unem, p = 1, d = 2, q = 1, n.ahead = 24)

# Compare the predictions with the actual values.
Jan 2016: actual 5.3 , predicted = 5.0
Feb 2016: actual 5.2 , predicted = 5.0	
Mar 2016: actual 5.1 , predicted = 4.9
Apr 2016: actual 4.7 , predicted = 4.9
May 2016: actual 4.5 , predicted = 5.9

# I would say, overall, that is a very good model.

# Next I did forecasting with the models that had predictors.
# Since sarima.for( ) doesn't take a xreg argument, I had
# to build the model with Arima( ) and then use the forecast( )
# function from the {forecast} package. 

# Here is forecast for ARIMA(1, 2, 1) with retail as predictor
fit.for_retail <- Arima(unem, order = c(1, 2, 1), xreg = retail)

# The forecast( ) function can only make forecasts when you have
# values for all the predictors. Thus, with this model, we can
# only predict the first three months of 2016. I think this is another
# reason to go with regular ARIMA(1, 2, 1). We could potentially
# come up with a model for the predictors and then forecast their
# own times, but this seems like a lot of work and would introduce
# a lot more variability in our modeling.
forecast(fit.for_retail, h = 3, xreg = econ$retail_sales[313:315])
plot(forecast(fit.for_retail, h = 3, xreg = econ$retail_sales[313:315]))

# Here are the actual and predicted unem values:
Jan 2016: actual 5.3 , predicted = 4.8
Feb 2016: actual 5.2 , predicted = 4.8	
Mar 2016: actual 5.1 , predicted = 4.9

# I think the predictors from the regular ARIMA(1, 2, 1) are better.
# Again, how are we going to forecast more than three months if
# we don't know the retail values for more than three months?

# Here is the forecasting for ARIMA(1, 2, 1) with both retail and ipi as regressors.
fit.for_retail_ipi <- Arima(unem, order = c(1, 2, 1), xreg = cbind(retail, ipi))
forecast(fit.for_retail_ipi, h = 5, xreg = cbind(econ$retail_sales[313:315], econ$industrial_production_index[313:315]))
plot(forecast(fit.for_retail_ipi, h = 5, xreg = cbind(econ$retail_sales[313:315], econ$industrial_production_index[313:315])))

# Here are the actual and predicted unem values for this model:
Jan 2016: actual 5.3 , predicted = 4.8
Feb 2016: actual 5.2 , predicted = 4.8	
Mar 2016: actual 5.1 , predicted = 4.9

# This model doesn't really do any better than the one with only retail as a predictor.

# Overall, I think that we should use the basic ARIMA(1, 2, 1) without any regressors.
# Dr. P mentions parsimony constantly, so I think that this a relatively simple,
# but very useful and valid model.



