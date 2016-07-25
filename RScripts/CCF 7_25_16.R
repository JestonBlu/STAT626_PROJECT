library(xlsx)
library(astsa)
library(TSA)
library(vars)
library(forecast)

# Load the seasonally adjusted data
load("C:\\Users\\LILLEYT\\Downloads\\Data_Prep (7).rda")
names(econ.sa) <- c("unem", "ipi", "orders", "house", "constr", "retail", "recession")
attach(econ.sa)

fit <- VAR(cbind(unem, constr, retail, recession), type = "both")

un.res <- residuals(fit)[, 1]
con.res <- residuals(fit)[, 2]
ret.res <- residuals(fit)[, 3]
rec.res <- residuals(fit)[, 4]

par(mfrow = c(2, 2))

acf(un.res, main="Unemployment")
ccf(un.res, con.res, main="Unemployment & Construction", ylab="CCF")
ccf(un.res, ret.res, main="Unemployment & Retail", ylab="CCF")
ccf(un.res, rec.res, main="Unemployment & Recession", ylab="CCF")

