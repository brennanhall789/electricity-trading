### Required Packages ###
library(lubridate)
library(ggplot2)
#########################
filename = 'prices_data_prep.R'
source(filename)

#### additional data prep ####
roll_day_2017 = rollmean(p_ts_2017, k = 24)
roll_wk_2017 = rollmean(p_ts_2017, k = 169)
roll_day_2018 = rollmean(p_ts_2018, k = 24)
roll_wk_2018 = rollmean(p_ts_2018, k = 169)
roll_day_2019 = rollmean(p_ts_2019, k = 24)
roll_wk_2019 = rollmean(p_ts_2019, k = 169)
roll_day_2020 = rollmean(p_ts_2020, k = 24)
roll_wk_2020 = rollmean(p_ts_2020, k = 169)

#### summary stats ####
summary(prices); sd(prices_ts)
summary(coredata(p_ts_2017)); sd(p_ts_2017)
summary(coredata(p_ts_2018)); sd(p_ts_2018)
summary(coredata(p_ts_2019)); sd(p_ts_2019)
summary(coredata(p_ts_2020)); sd(p_ts_2020)


#### Price Plots ####

p <- ggplot(prices_ts, aes(x=as.Date(index(prices_ts)),y=coredata(prices_ts))) +
  geom_line() + xlab('') + ylab('$') + 
  coord_cartesian(ylim=c(-5,5000))
p +title('Price process') 

p2017 <- ggplot(p_ts_2017, aes(x=as.Date(index(p_ts_2017)), y=coredata(p_ts_2017))) + 
  geom_line() + xlab('') + ylab('$') +
  coord_cartesian(ylim=c(-5,1000))
  p2017 + 
  geom_line(data = roll_day_2017, 
            aes(x=as.Date(index(roll_day_2017)), y=coredata(roll_day_2017)), color='red') +
  annotate(geom="text", x=as.Date('2017-02-18'), y=750, label="24-hour Rolling Average",col='red') +
  geom_line(data = roll_wk_2017, 
            aes(x=as.Date(index(roll_wk_2017)), y=coredata(roll_wk_2017)), color='green') + 
  annotate(geom="text", x=as.Date('2017-02-19'), y=700, label="Weekly Rolling Average",col='green')


p2018 <- ggplot(p_ts_2018, aes(x=as.Date(index(p_ts_2018)), y=coredata(p_ts_2018))) +
  geom_line() + xlab('') + ylab('$') + 
  coord_cartesian(ylim=c(-5,1000))
p2018 +
  geom_line(data = roll_day_2018, 
            aes(x=as.Date(index(roll_day_2018)), y=coredata(roll_day_2018)), color='red') +
  annotate(geom="text", x=as.Date('2018-04-12'), y=750, label="24-hour Rolling Average",col='red') +
  geom_line(data = roll_wk_2018, 
            aes(x=as.Date(index(roll_wk_2018)), y=coredata(roll_wk_2018)), color='green') + 
  annotate(geom="text", x=as.Date('2018-04-12'), y=700, label="Weekly Rolling Average",col='green')


p2019 <- ggplot(p_ts_2019, aes(x=as.Date(index(p_ts_2019)), y=coredata(p_ts_2019))) + 
  geom_line() + xlab('') + ylab('$') + 
  coord_cartesian(ylim=c(5,1000))
p2019 + 
  geom_line(data = roll_day_2019, 
            aes(x=as.Date(index(roll_day_2019)), y=coredata(roll_day_2019)), color='red') +
  annotate(geom="text", x=as.Date('2019-04-18'), y=4500, label="24-hour Rolling Average",col='red') +
  geom_line(data = roll_wk_2019, 
            aes(x=as.Date(index(roll_wk_2019)), y=coredata(roll_wk_2019)), color='green') + 
  annotate(geom="text", x=as.Date('2019-04-19'), y=4300, label="Weekly Rolling Average",col='green')

p2020 <- ggplot(p_ts_2020, aes(x=as.Date(index(p_ts_2020)), y=coredata(p_ts_2020))) + 
  geom_line() + xlab('') + ylab('$') + 
  coord_cartesian(ylim=c(-5,1000))
p2020 +
  geom_line(data = roll_day_2020, 
            aes(x=as.Date(index(roll_day_2020)), y=coredata(roll_day_2020)), color='red') +
  annotate(geom="text", x=as.Date('2020-02-18'), y=750, label="24-hour Rolling Average",col='red') +
  geom_line(data = roll_wk_2020, 
            aes(x=as.Date(index(roll_wk_2020)), y=coredata(roll_wk_2020)), color='green') + 
  annotate(geom="text", x=as.Date('2020-02-19'), y=700, label="Weekly Rolling Average",col='green')


ggpubr::ggarrange(p2017, p2018, p2019, nrow = 4,
                  ggpubr::ggarrange(p2020, ncol=2))


#### ACF/PACF ####
par(mfrow=c(2,2))
acf(coredata(p_ts_2017),lag.max = 200, main='2017 Autocorrelation')
pacf(coredata(p_ts_2017),lag.max = 200, main='2017 partial Autocorrelation')
acf(coredata(p_ts_2018),lag.max = 200, main='2018 Autocorrelation')
pacf(coredata(p_ts_2018),lag.max = 200, main='2018 partial Autocorrelation')
acf(coredata(p_ts_2019),lag.max = 200, main='2019 Autocorrelation')
pacf(coredata(p_ts_2019),lag.max = 200, main='2019 partial Autocorrelation')
acf(coredata(p_ts_2020),lag.max = 200, main='2020 Autocorrelation')
pacf(coredata(p_ts_2020),lag.max = 200, main='2020 partial Autocorrelation')
# indicates AR(1) process with 24-hour seasonality during normal periods
par(mfrow=c(1,1))


#### Volatility ####
daily_sd = data.table(prices)
split = stringr::str_split_fixed(daily_sd[['Time']],' ', 2)
daily_sd[,Date:=split[,1]]
daily_sd[,ix:=.GRP, by = 'Date']
daily_sd[,d_sd:=sd(Price), by = 'ix']
daily_sd = unique(daily_sd, by='ix')
summary(daily_sd$d_sd)
sd(daily_sd$d_sd)

sp <- ggplot(data=daily_sd, aes(x=as.Date(Date))) +
  geom_line(aes(y=d_sd)) + 
  labs(x='Year', y='') +
  coord_cartesian(ylim=c(-5,350))

#### 2017 ####
daily_sd = data.table(Date=as.Date(index(p_ts_2017)), coredata(p_ts_2017))
daily_sd[,ix:=.GRP, by = 'Date']
daily_sd[,d_sd:=sd(Price), by = 'ix']
daily_sd = unique(daily_sd, by='ix')
summary(daily_sd$d_sd)
sd(daily_sd$d_sd)

sp2017 <- ggplot(data=daily_sd, aes(x=as.Date(Date))) +
  geom_line(aes(y=d_sd)) + 
  labs(x='2017', y='Std. Dev.') +
  coord_cartesian(ylim=c(-5,350))

#### 2018 ####
daily_sd = data.table(Date=as.Date(index(p_ts_2018)), coredata(p_ts_2018))
daily_sd[,ix:=.GRP, by = 'Date']
daily_sd[,d_sd:=sd(Price), by = 'ix']
daily_sd = unique(daily_sd, by='ix')
summary(daily_sd$d_sd)
sd(daily_sd$d_sd)

sp2018 <- ggplot(data=daily_sd, aes(x=as.Date(Date))) +
  geom_line(aes(y=d_sd)) + 
  labs(x='2018', y='Std. Dev.') +
  coord_cartesian(ylim=c(-5,350))

#### 2019 ####
daily_sd = data.table(Date=as.Date(index(p_ts_2019)), coredata(p_ts_2019))
daily_sd[,ix:=.GRP, by = 'Date']
daily_sd[,d_sd:=sd(Price), by = 'ix']
daily_sd = unique(daily_sd, by='ix')
summary(daily_sd$d_sd)
sd(daily_sd$d_sd)

sp2019 <- ggplot(data=daily_sd, aes(x=as.Date(Date))) +
  geom_line(aes(y=d_sd)) + 
  labs(x='2019', y='Std. Dev.') +
  coord_cartesian(ylim=c(-5,350))

#### 2020 ####
daily_sd = data.table(Date=as.Date(index(p_ts_2020)), coredata(p_ts_2020))
daily_sd[,ix:=.GRP, by = 'Date']
daily_sd[,d_sd:=sd(Price), by = 'ix']
daily_sd = unique(daily_sd, by='ix')
summary(daily_sd$d_sd)
sd(daily_sd$d_sd)

sp2020 <- ggplot(data=daily_sd, aes(x=as.Date(Date))) +
  geom_line(aes(y=d_sd)) + 
  labs(x='2020', y='Std. Dev.') +
  coord_cartesian(ylim=c(-5,350))

ggpubr::ggarrange(sp2017,sp2018,sp2019,
                  ggpubr::ggarrange(sp2020, sp, ncol=2),nrow = 4)

##### Abnormalities #####
spikes_ind <- which(prices_ts$Price > 150)
spikes = prices_ts[spikes_ind,]
afternoon_hrs = c(11,12,13,14,15,16,17)
aft_count = 0
for(i in 1:length(spikes_ind)){
  if(hour(index(spikes[i])) %in% afternoon_hrs){
    aft_count = aft_count + 1
  }
}
jump_prob = length(spikes_ind)/length(prices_ts)
aft_jump = aft_count/length(spikes_ind)


## Negative prices
neg_price_ind <- which(prices_ts$Price < 0)
neg_price = prices_ts[neg_price_ind,]
morning_hrs = c(23,0,1,2,3,4,5)
morn_count = 0
for(i in 1:length(neg_price_ind)){
  if(hour(index(neg_price[i])) %in% morning_hrs){
    morn_count = morn_count + 1
  }
}
neg_prob = length(neg_price_ind)/length(prices_ts)
morn_prob = morn_count/length(neg_price_ind)


#### 2017 ####
spikes_ind <- which(p_ts_2017$Price > 150)
spikes = p_ts_2017[spikes_ind,]
afternoon_hrs = c(11,12,13,14,15,16,17)
aft_count = 0
for(i in 1:length(spikes_ind)){
  if(hour(index(spikes[i])) %in% afternoon_hrs){
    aft_count = aft_count + 1
  }
}
jump_prob = length(spikes_ind)/length(p_ts_2017)
aft_jump = aft_count/length(spikes_ind)

## Negative prices
neg_price_ind <- which(p_ts_2017$Price < 0)
neg_price = p_ts_2017[neg_price_ind,]
morning_hrs = c(23,0,1,2,3,4,5)
morn_count = 0
for(i in 1:length(neg_price_ind)){
  if(hour(index(neg_price[i])) %in% morning_hrs){
    morn_count = morn_count + 1
  }
}
neg_prob = length(neg_price_ind)/length(p_ts_2017)
morn_prob = morn_count/length(neg_price_ind)


#### 2018 ####
spikes_ind <- which(p_ts_2018$Price > 150)
spikes = p_ts_2018[spikes_ind,]
afternoon_hrs = c(11,12,13,14,15,16,17)
aft_count = 0
for(i in 1:length(spikes_ind)){
  if(hour(index(spikes[i])) %in% afternoon_hrs){
    aft_count = aft_count + 1
  }
}
jump_prob = length(spikes_ind)/length(p_ts_2018)
aft_jump = aft_count/length(spikes_ind)

## Negative prices
neg_price_ind <- which(p_ts_2018$Price < 0)
neg_price = p_ts_2018[neg_price_ind,]
morning_hrs = c(23,0,1,2,3,4,5)
morn_count = 0
for(i in 1:length(neg_price_ind)){
  if(hour(index(neg_price[i])) %in% morning_hrs){
    morn_count = morn_count + 1
  }
}
neg_prob = length(neg_price_ind)/length(p_ts_2018)
morn_prob = morn_count/length(neg_price_ind)

#### 2019 ####
spikes_ind <- which(p_ts_2019$Price > 150)
spikes = p_ts_2019[spikes_ind,]
afternoon_hrs = c(11,12,13,14,15,16,17)
aft_count = 0
for(i in 1:length(spikes_ind)){
  if(hour(index(spikes[i])) %in% afternoon_hrs){
    aft_count = aft_count + 1
  }
}
jump_prob = length(spikes_ind)/length(p_ts_2019)
aft_jump = aft_count/length(spikes_ind)

## Negative prices
neg_price_ind <- which(p_ts_2019$Price < 0)
neg_price = prices_ts[neg_price_ind,]
morning_hrs = c(23,0,1,2,3,4,5)
morn_count = 0
for(i in 1:length(neg_price_ind)){
  if(hour(index(neg_price[i])) %in% morning_hrs){
    morn_count = morn_count + 1
  }
}
neg_prob = length(neg_price_ind)/length(p_ts_2019)
morn_prob = morn_count/length(neg_price_ind)


#### 2020 ####
spikes_ind <- which(p_ts_2020$Price > 150)
spikes = p_ts_2020[spikes_ind,]
afternoon_hrs = c(11,12,13,14,15,16,17)
aft_count = 0
for(i in 1:length(spikes_ind)){
  if(hour(index(spikes[i])) %in% afternoon_hrs){
    aft_count = aft_count + 1
  }
}
jump_prob = length(spikes_ind)/length(p_ts_2020)
aft_jump = aft_count/length(spikes_ind)

## Negative prices
neg_price_ind <- which(p_ts_2020$Price < 0)
neg_price = p_ts_2020[neg_price_ind,]
morning_hrs = c(23,0,1,2,3,4,5)
morn_count = 0
for(i in 1:length(neg_price_ind)){
  if(hour(index(neg_price[i])) %in% morning_hrs){
    morn_count = morn_count + 1
  }
}
neg_prob = length(neg_price_ind)/length(p_ts_2020)
morn_prob = morn_count/length(neg_price_ind)


################
