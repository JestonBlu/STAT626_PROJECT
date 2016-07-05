## Import original Data
econ = read.csv("Data/Unemployment.csv")

## Format Date as Date
econ$date = as.Date(econ$date)

## Omit missing date, date range kept (Jan 1993 - Dec 2015)
econ = na.omit(econ)
econ = subset(econ, date <= '2015-12-01')

row.names(econ) = econ$date
econ = econ[, -1]


## Change the measurements to billions
econ$manufacturers_new_orders = econ$manufacturers_new_orders / 1000 
econ$construction_spending = econ$construction_spending / 1000
econ$retail_sales = econ$retail_sales / 1000

## Decompose the data so we can remove the seasonal adjustments
decom.unem = decompose(ts(data = econ$unem_rate, 
                          start = c(1993,1), frequency = 12), type = "additive")
decom.ind = decompose(ts(data = econ$industrial_production_index, 
                         start = c(1993,1), frequency = 12), type = "additive")
decom.mno = decompose(ts(data = econ$manufacturers_new_orders, 
                         start = c(1993,1), frequency = 12), type = "additive")
decom.hpi = decompose(ts(data = econ$purchase_house_price_index, 
                         start = c(1993,1), frequency = 12), type = "additive")
decom.con = decompose(ts(data = econ$construction_spending, 
                         start = c(1993,1), frequency = 12), type = "additive")
decom.rts = decompose(ts(data = econ$retail_sales, 
                         start = c(1993,1), frequency = 12), type = "additive")

## Add seasonally adjusted rate
econ.sa = data.frame(
  row.names = row.names(econ),
  unem_rate_sa = econ$unem_rate - decom.unem$sea,
  industrial_production_sa = econ$industrial_production_index - decom.ind$sea,
  manufacturers_new_orders_sa = econ$manufacturers_new_orders - decom.mno$sea,
  house_price_sa = econ$purchase_house_price_index - decom.hpi$sea,
  construction_spend_sa = econ$construction_spending - decom.con$sea,
  retail_sales_sa = econ$retail_sales - decom.rts$sea
)


## Export data to be used for modeling
save(list = c("econ", "econ.sa"), file = "Data/Data_Prep.rda")