#kobe shooting percentage time series analysis
#intialise libraries
library(dplyr)
library(forecast)
library(tseries)

#read data
setwd("C:/Users/bjkwok/Documents/Personal - Not Backed Up - Aucune sauvegarde/References/Kobe")
data = read.table("data.csv",sep=",",header=TRUE)

#data definition adjustments
data$game_date = as.Date(data$game_date)
data$game_month = as.numeric(format(data$game_date,'%m'))
data$game_year = as.numeric(format(data$game_date,'%Y'))
data$playoffs = factor(data$playoffs)

#separate training and testing data
train = data[!is.na(data$shot_made_flag),]
test = data[is.na(data$shot_made_flag),]

#summarise shots taken/missed by game_id in regular season
grp = group_by(train,game_id)
shots = data.frame(summarise(grp,sum(shot_made_flag)),tally(grp)[,2])
shots$pct = ts(shots$sum.shot_made_flag./shots$n)

#plot shooting percentage time series
ts.plot(shots$pct,fitted(Arima(shots$pct,order=c(1,0,1))),
        gpars=list(col=c("black","red")),
	  main="Kobe Monthly FG%")
legend(x="topleft",legend=c("Actual", "Fitted"),xpd=TRUE,
       col=c("black","red"),lty=c(1,1),inset=c(0,-0.2))

#test for stationarity
PP.test(shots$pct)

#check autocorrelation of shooting percentage between games
par(mfrow=c(2,1))
acf(shots$pct)
pacf(shots$pct)
par(mfrow=c(1,1))

#check autocorrelation of first-differenced shooting percentage between games
par(mfrow=c(2,1))
acf(diff(shots$pct,d=1))
pacf(diff(shots$pct,d=1))
par(mfrow=c(1,1))

#fit optimal ARMA(p,q)
fit = auto.arima(shots$pct)
fit

#analyse residuals
plot.ts(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals)
pacf(fit$residuals)
par(mfrow=c(1,1))

qqnorm(fit$residuals)
qqline(fit$residuals)
jarque.bera.test(fit$residuals)
