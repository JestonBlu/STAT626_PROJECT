# 
library(astsa)
library(tsa)
library(xlsx)
data <- read.xlsx("C:\\Users\\Travis\\Desktop\\STAT 626\\Group Project\\Unemployment.xlsx", 
	           sheetName = "unem")[1:276, ]

data_2016 <- read.xlsx("C:\\Users\\Travis\\Desktop\\STAT 626\\Group Project\\Unemployment.xlsx", 
	           sheetName = "unem")[277, 281, ]

data <- ts(data)

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


