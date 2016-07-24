## Packages
library(ggplot2)
library(plyr)
library(scales)


## Read in data
econ = read.csv("~/OneDrive/TAMU/STAT 626/Group Presentation/STAT626_PROJECT/RScripts/Unemployment.csv")
usa  = read.csv("~/OneDrive/TAMU/STAT 626/Group Presentation/STAT626_PROJECT/RScripts/US Congress and Recession Data.csv")

#Unemployment before all the modifications
unem.sa <- read.csv("~/OneDrive/TAMU/STAT 626/Group Presentation/STAT626_PROJECT/RScripts/UNRATE.csv")$UNRATE
unem.nsa <- read.csv("~/OneDrive/TAMU/STAT 626/Group Presentation/STAT626_PROJECT/RScripts/UNRATENSA.csv")$UNRATENSA
unem.sa <- ts(unem.sa, start=c(1948, 1), frequency = 12)
unem.nsa <- ts(unem.sa, start=c(1948, 1), frequency = 12)

par.oldpar <- par()

#Plot seasonally adjusted and not seasonaly adjusted Unemployment rates
par(par.oldpar)
plot(unem.nsa, main="Monthly US Unemployment Rate, Not Seasonally Adusted", xlab="Year", ylab="Rate")
plot(unem.sa)


## Subset data from Jan 1993 to Dec 2015
econ = na.omit(econ)
dta = na.omit(join(econ, usa, by = "date"))
dta$date = as.Date(dta$date)



## Custom ggplot theme
theme_stat = function() {
  theme(
    plot.title          = element_text(size = 14, face = "bold"),
    panel.background    = element_rect(fill = NA),
    panel.grid.major    = element_line(color = "gray", size = .2),
    panel.border        = element_rect(color = "gray", fill = NA, size = .2),
    axis.ticks          = element_line(size = 0),
    axis.text           = element_text(color = "black"),
    axis.title.x        = element_text(size = 12, color = "black"),
    axis.title.y        = element_text(size = 12, color = "black")
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
  geom_line(aes(x = date, y = unem.decom$trend/100)) +
  scale_x_date("Year", expand = c(0,0)) +
  scale_y_continuous("Unemployment Rate", labels = percent, expand=c(0,0)) +
  scale_fill_discrete("President") +
  ggtitle("Seasonally Adjusted Unemployment Rate\n(Jan 93' - Dec 15')") +
  theme_stat()

g4

## Lag Plot
## Borrowed from StackOverflow: http://stackoverflow.com/questions/21524600/
lag.plot1 = function(data1, max.lag = 1, corr = TRUE, smooth = FALSE) { 
  name1 = paste(deparse(substitute(data1)), "(t-", sep = "")
  name2 = paste(deparse(substitute(data1)), "(t)", sep = "")
  data1 = as.ts(data1)
  max.lag = as.integer(max.lag)
  prow = ceiling(sqrt(max.lag))
  pcol = ceiling(max.lag / prow)
  a = acf(data1, max.lag, plot = FALSE)$acf[-1]
  par(mfrow = c(prow, pcol), mar = c(2.5, 4, 2.5, 1), cex.main = 1.1, font.main = 1)
  for(h in 1:max.lag) {                       
    plot(lag(data1, -h), data1, xy.labels = FALSE, main = paste(name1, h, ")",sep = ""), 
         ylab = name2, xlab = "") 
    if (smooth == TRUE) 
      lines(lowess(ts.intersect(lag(data1, -h), data1)[, 1], ts.intersect(lag(data1, -h), data1)[, 2],
                   f = 1), col = "red")
    if (corr == TRUE)
      legend("topright", legend = round(a[h], digits = 2), text.col = "blue", bg = "white", x.intersp = 0)
  }
}

with(econ, lag.plot1(unem_rate, 12, smooth = TRUE))

## Predictor Variable Plots
econ2 = ts(econ[, 2:7], start = c(1993, 1), frequency = 12)
plot.ts(econ2, main = "Unemployment Rate with Predictor Variables")

## Scatterplot
plot(econ[,2:7])



