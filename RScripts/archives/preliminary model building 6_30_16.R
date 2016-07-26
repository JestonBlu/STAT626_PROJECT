# Some more models

unem <- data[,2]
n <- length(unem)
crit_value <- 1.96/sqrt(n)

# Looks like taking the second difference, then the season s = 12 difference
# produces stationary data

ts.plot(diff(diff(diff(unem)), 12))
adf.test(diff(diff(diff(unem)), 12))

# Look at ACF and PACF at intervals of 12
acf(diff(diff(diff(unem)), 12), 72)[seq(12, 72, 12)]
pacf(diff(diff(diff(unem)), 12), 72)[seq(12, 72, 12)]

# Seems that then the ACF trails off, and PACF cuts off after one year
# This would suggest
# (i) P = 1, D = 1, Q = 0, S = 12

# Look at ACF and PACF within first 12
# ACF cuts off after 1
# PACF declines slowly
# Suggests p = 0, d = 2, q = 1
acf(diff(diff(diff(unem)), 12), 72)[1:12]
pacf(diff(diff(diff(unem)), 12), 72)[1:12]

# Apply model to original data p = 0, d = 2, q = 1, P = 1, D = 1, Q = 0, S = 12
# Diagnostics look reasonable
# p-values for Ljung-Box are not excellent, but all (except one) stay
# above the line for the first 23, which may be good enough
model <- sarima(unem, p = 0, d = 2, q = 1, P = 1, D = 1, Q = 0, S = 12)

# With operator notation, our model becomes
(1-PHI*B^12)(1-B^12)((1-B)^2)xt = (1+theta*B)wt



