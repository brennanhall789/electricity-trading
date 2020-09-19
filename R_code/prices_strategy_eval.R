library(portfolioBacktest)
library(data.table)
library(ggplot2)
source('data_prep.R')

#### Additional data prep ####
trading = as.data.table(read.csv('tios-quant-homework-data-2020/electricity-trading-history.csv'))
trading_offers = trading[1:30644,]
trading_bids = trading[30645:nrow(trading),]

trading_offers$Cum.Offers = cumsum(trading_offers$Clear.Volume)
trading_bids$Cum.Bids = cumsum(trading_bids$Clear.Volume)
trading_offers$Cum.Return = cumsum(trading_offers$Return)
trading_bids$Cum.Return = cumsum(trading_bids$Return)
Net.Offer = trading_bids$Cum.Bids - trading_offers$Cum.Offers

returns <- data.table(Time = trading_offers$Time,
                     Cumulative.Returns = trading_offers$Cum.Return + trading_bids$Cum.Return,
                     Returns = trading_offers$Return + trading_bids$Return,
                     Net.Offer = Net.Offer,
                     Trade.Vol = trading_offers$Clear.Volume + trading_bids$Clear.Volume,
                     Net.Trade = Net.Offer * trading_offers$Clear.Price
                     )

summary(returns)

daily_rets = returns[,.(Time, Returns, Trade.Vol)]
split = stringr::str_split_fixed(daily_rets[['Time']],' ', 2)
daily_rets[,Date:=split[,1]]#; daily_rets[,'Hour':=split[,2]]
daily_rets[,ix:=.GRP, by = 'Date']
daily_rets[,daily_ret:=sum(Returns), by = 'ix']
daily_rets[,daily_sd:=sd(Returns), by = 'ix']
daily_rets[,daily_volume:=sum(Trade.Vol), by = 'ix']
daily_rets=unique(daily_rets, by='ix')

daily=daily_rets[,.(Date,daily_ret, daily_sd, daily_volume)]
summary(daily)

#### Plots ####

# plot of net bids/offers
position_plot <- ggplot(data = returns, aes(x=as.Date(Time),y=Net.Offer))+
  geom_line() + xlab('Time') + ylab('Net Trades (bids - offers)')
position_plot 
## Note the strategy is selling far more than buying

# plot of daily trade volume
daily_volume_plot <- ggplot(data = daily_rets, aes(x=as.Date(Time),y=daily_volume))+
  geom_line() + xlab('Time') + ylab('Daily Trade Volume')
daily_volume_plot 

# plot of daily returns
daily_plot <- ggplot(data = daily_rets, aes(x=as.Date(Date),y=daily_ret))+
  geom_line(color='blue') + xlab('Date') + ylab('Daily Return')
daily_plot #+ coord_cartesian(ylim=c(-200,200))

ggpubr::ggarrange(daily_plot,daily_volume_plot,nrow=2)

# plot of cumulative returns
total_rets_plot <- ggplot(data = returns, aes(x=as.Date(Time)))+
  geom_line(aes(y=Cumulative.Returns),color='blue') + xlab('Time') + ylab('Cumulative Return ($)') 
total_rets_plot 

# plot of daily return std. dev.
daily_sd_plot <- ggplot(data = daily_rets, aes(x=as.Date(Time)))+
  geom_line(aes(y=daily_sd),color='red') + xlab('Time') + ylab('Daily Return SD') 
daily_sd_plot + geom_abline(slope=0,intercept = outlier,
                            color='blue',size=.6)

ggpubr::ggarrange(total_rets_plot, daily_sd_plot,nrow=2)


#### Performance Measures ####
# total net profit
sum(returns$Returns)
daily_returns_plot

# percent profitable trades = winning trades/total trades
total_trades = length(which(returns$Trade.Vol != 0))
wins = length(which(returns$Returns > 0))
profit_trades = wins/total_trades

# trade volatility
sd(daily$daily_ret)
outlier = 1.5*(quantile(daily$daily_ret,.75)-quantile(daily$daily_ret,.25))
length(which(daily$daily_ret>outlier))/nrow(daily)

ggpubr::ggarrange(daily_plot, daily_sd_plot, daily_volume_plot, nrow=3)

