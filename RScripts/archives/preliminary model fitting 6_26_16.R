library(astsa)
library(TSA)


load("Data/Data_Prep.rda")

data <- ts(econ)

# Is raw data stationary?
# No
adf.test(data[,2])

# Try first difference
# Seems stationary
adf.test(diff(data[,2]))

# Look at acf and pacf of differenced data
acf(data[,2], main = "Unemployment")
pacf(data[,2], main = "Unemployment")

# Try ARIMA p = 1, q = 0, on differenced data
fit1 <- sarima(diff(data[,2]), 1, 0, 0)

# Try ARIMA p = 0, q = 1, on differenced data
fit2 <- sarima(diff(data[,2]), 0, 0, 1)

# Try ARIMA p = 1, q = 1, on differenced data
fit3 <- sarima(diff(data[,2]), 1, 0, 1)

# Try ARIMA p = 2, q = 1, on differenced data
fit4 <- sarima(diff(data[,2]), 2, 0 ,1)

# Try ARIMA p = 1, q = 2, on differenced data
fit5 <- sarima(diff(data[,2]), 1, 0 , 2)

# Try ARIMA p = q = 2, on differenced data
fit6 <- sarima(diff(data[,2]), 2, 0, 2)

# Display AICc of all modelts
fit1$AICc
fit2$AICc
fit3$AICc
fit4$AICc
fit5$AICc
fit6$AICc

# Seems that ARMA with p = q = 2 is best
acf(diff(log(econ$unem_rate)))
pacf(diff(log(econ$unem_rate)))


# ADF on seasonally adjusted data
adf.test(na.omit(econ$unem_rate_sa))
adf.test(diff(na.omit(econ$unem_rate_sa), lag = 1, differences = 2))

# Differencing Plots
par(mfrow = c(2,2))
plot.ts(diff(na.omit(econ$unem_rate), lag = 1, differences = 1), 
        main = "Unem 1st Order Differencing", ylab = "")
plot.ts(diff(na.omit(econ$unem_rate_sa), lag = 1, differences = 1), 
        main = "Unem SA 1st Order Differencing", ylab = "")
plot.ts(diff(na.omit(econ$unem_rate_sa), lag = 1, differences = 2), 
        main = "Unem SA 2nd Order Differencing", ylab = "")
plot.ts(diff(na.omit(econ$unem_rate_sa), lag = 1, differences = 3), 
        main = "Unem SA 3rd Order Differencing", ylab = "")

econ = na.omit(econ)
Acf(diff(na.omit(econ$unem_rate_sa), differences = 2))
Acf(diff(na.omit(econ$unem_rate_sa), differences = 3))
Pacf(diff(na.omit(econ$unem_rate_sa), differences = 2))
Pacf(diff(na.omit(econ$unem_rate_sa), differences = 3))

par(mfrow = c(1,1))