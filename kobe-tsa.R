# Kaggle Kobe shooting percentage time series analysis

# Load libraries
library(dplyr)
library(ggplot2)

# Read data
setwd("C:/Users/bjkwok/Documents/Personal - Not Backed Up - Aucune sauvegarde/References/Kobe")
data = read.csv("data.csv", sep = ",", header = T, stringsAsFactors = F)

# Seperate train and test data
train = data[!is.na(data$shot_made_flag),]
test = data[is.na(data$shot_made_flag),]

# Summarise shots taken/missed by game_id
grp = group_by(train, game_id)
shots = summarise(grp, y = sum(shot_made_flag), n = n())
shots$pct = shots$y / shots$n

# Plot shooting percentage time series
ts.plot(shots$pct, fitted(Arima(shots$pct, order=c(1,0,1))),
        gpars = list(col = c("black","red")),
	  main = "Kobe FG% by game")
legend(x = "topleft", legend = c("Actual","Fitted"), xpd = T,
       col = c("black","red"), lty = c(1,1), inset = c(0,-0.15))

# Test for stationarity (Phillips-Perron unit root test)
PP.test(shots$pct)

# Check autocorrelation of shooting percentage between games
par(mfrow=c(2,1))
acf(shots$pct)
pacf(shots$pct)
par(mfrow=c(1,1))

# Check autocorrelation of first-differenced shooting percentage between games
# Very clear ARIMA(0,1,1) pattern in acf and pacf graphs
par(mfrow=c(2,1))
acf(diff(shots$pct, d = 1))
pacf(diff(shots$pct, d = 1))
par(mfrow=c(1,1))

# Fit optimal ARIMA(p,d,q) where we expect ARIMA(0,1,1)
fit = auto.arima(shots$pct)
fit

# Analyse autocorrelation of residuals
plot.ts(fit$residuals)
par(mfrow=c(2,1))
acf(fit$residuals)
pacf(fit$residuals)
par(mfrow=c(1,1))

# Test normality of residuals
qqnorm(fit$residuals)
qqline(fit$residuals)
jarque.bera.test(fit$residuals)
