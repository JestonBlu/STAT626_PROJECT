library(xlsx)
data <- read.xlsx("C:\\Users\\Travis\\Desktop\\STAT 626\\Group Project\\Unemployment.xlsx", 
	           sheetName = "unem")[1:276, ]

data_2016 <- read.xlsx("C:\\Users\\Travis\\Desktop\\STAT 626\\Group Project\\Unemployment.xlsx", 
	           sheetName = "unem")[277, 281, ]

data <- ts(data)

pairs(data[, -1])
ts.plot(data[, 2])
