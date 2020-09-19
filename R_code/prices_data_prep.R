library(xts)

### Data Preparation ###
prices <- read.csv('tios-quant-homework-data-2020/electricity-price.csv')
prices_ts <- xts(x = prices['Price'], 
                 order.by = strptime(prices[['Time']], format = "%Y-%m-%d %H:%M:%S"))
yearpoints = endpoints(prices_ts, on = 'year')
p_ts_2017 = prices_ts[0:yearpoints[2],]
p_ts_2018 = prices_ts[(yearpoints[2] + 1):yearpoints[3],]
p_ts_2019 = prices_ts[(yearpoints[3] + 1):yearpoints[4],]
p_ts_2020 = prices_ts[(yearpoints[4] + 1):yearpoints[5],]