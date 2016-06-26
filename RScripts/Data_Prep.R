## Import original Data
econ = read.csv("Data/Unemployment.csv")

## Format Date as Date
econ$date = as.Date(econ$date)

## Omit missing date, date range kept (Jan 1993 - Dec 2015)
econ = na.omit(econ)

## Scale variable that are measured in dollars
econ$manufacturers_new_orders = scale(econ$manufacturers_new_orders)
econ$construction_spending = scale(econ$construction_spending)
econ$retail_sales = scale(econ$retail_sales)

## Decompose the data so we can remove the seasonal adjustments
unem.decom = decompose(ts(data = econ$unem_rate, start = c(1993,1), frequency = 12), type = "additive")

## Add seasonally adjusted rate
econ$unem_rate_sa = unem.decom$trend

## Export data to be used for modeling
save(econ, file = "Data/Data_Prep.rda")
