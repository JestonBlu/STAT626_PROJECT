## Packages
library(ggplot2)
library(plyr)
library(scales)

## Read in data
econ = read.csv("Data/Unemployment.csv")
usa  = read.csv("Data/US Congress and Recession Data.csv")

## Subset data from Jan 1993 to Dec 2015
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

g0 = ggplot(dta)

pres = ddply(dta, .(pres), summarise, enter = min(date), exit = max(date))

g1 = g0 + geom_rect(aes(xmin = enter, xmax = exit, ymin = 0, ymax = .12, fill = pres), 
            alpha = .2, data = pres)

g2 = g1 + 
  geom_rect(aes(xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-01"), 
                ymin = 0, ymax = .12), color = NA, fill = "gray", alpha = .2) +
  geom_rect(aes(xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-01"), 
                ymin = 0, ymax = .12), color = NA, fill = "gray", alpha = .2)
g3 = g2 + 
geom_line(aes(x = date, y = unem_rate/100)) +
  scale_x_date("Year", expand = c(0,0)) +
  scale_y_continuous("Unemployment Rate", labels = percent, expand=c(0,0)) +
  scale_fill_discrete("President") +
  ggtitle("Unemployment Rate\n(Jan 93' - Dec 15')") +
  theme_stat()
