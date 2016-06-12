## Import original Data
econ = read.csv("Data/Unemployment.csv")

## Format Date as Date
econ$date = as.Date(econ$date)

## Omit missing date, date range kept (Jan 1993 - Oct 2015)
econ = na.omit(econ)

## Scale variable that are measured in dollars
econ$manufacturers_new_orders = scale(econ$manufacturers_new_orders)
econ$construction_spending = scale(econ$construction_spending)
econ$retail_sales = scale(econ$retail_sales)

## Export data to be used for modeling
save(list = ls(), file = "Data/Data_Prep.rda")
