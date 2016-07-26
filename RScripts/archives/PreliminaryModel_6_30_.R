load("Data_Prep.rda")
library(astsa)

unem<-econ$unem_rate
ts.plot(unem)

unem.diff2<-diff(diff(diff(unem)), 12)
unem.diff3<-diff(diff(diff(diff(unem))), 12)

par(mfrow=c(3,1))
ts.plot(unem)
ts.plot(unem.diff2)
ts.plot(unem.diff3)

par(mfrow=c(2,1))
acf(unem.diff2, 72)
pacf(unem.diff2, 72)

## First, we can look at the seasonal pattern. The ACF seems to tail off
# The PACF cuts off at either 1 or 3. Together, the plots suggest
# AR(1) or AR (3)

## We then can inspect the plots at the within season lags, h=1,..,11
# One perspective is that the ACF cuts off after 1 and the PACF tails off,
# it indicates MA(1)

# Another perspective is that the ACF cuts off after 1 and the PACF cuts off
# after 4. In this situation, the book suggests to build at SARMA of orders 
# p = 4 and q = 1. However, Professor points out this is a bad reasoning. It is
# tempting to try this model since I don't feel that the PACF tails off. We right now
# don't know how to handel the siation where both ACF and PACF cuts off at a certain lag.
# So I tried this model. In diagnositic procedures, it works better at some criterion. 

# Together, I proposed two additonal models and compare them to the one proposed by Travis.

model1 <- sarima(unem, p = 0, d = 2, q = 1, P = 1, D = 1, Q = 0, S = 12)
model2 <- sarima(unem, p = 0, d = 2, q = 1, P = 3, D = 1, Q = 0, S = 12)
model3 <- sarima(unem, p = 4, d = 2, q = 1, P = 3, D = 1, Q = 0, S = 12)


model1$AIC; model1$BIC

model2$AIC; model2$BIC

model3$AIC; model3$BIC

# > model1$AIC; model1$BIC
# [1] -2.283108
# [1] -3.244063
# > 
#   > model2$AIC; model2$BIC
# [1] -2.444084
# [1] -3.379008
# > 
#   > model3$AIC; model3$BIC
# [1] -2.44496
# [1] -3.327824

## From looking at AIC and BIC values, Models 2 and 3 perform quite similarly, which both show some evidece of performing
# better than Model1. 
# Then we could compare the three models based on diagnostic plots.
# The standardized residuals of all models show some evidece of non-white-noise.
# ARMA models do not model variablity. We will have a few lectures on this topic.
# There is not much we can do now in this regard.

# In the ACF of residuals of Model 1 shows spike at lag 24. The other two models
# do not show such a spike.

# The normal plots from the three models are fairly similar.

# The Q-statistic or Ljung-Box statistic 
# Models 1 and 2 have similar results. Model 1 seems to perform better
# at the first few lags and Model 2 does better after lag 15. 
# Model 3 clearly perform better thant the two models on the Q-statistic. 

