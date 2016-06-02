#initialize libraries
library(MASS)
library(tseries)

#read data
setwd("C:/Users/bjkwok/Documents/Personal - Not Backed Up - Aucune sauvegarde/References/Kobe")
data = read.table("data.csv",sep=",",header=TRUE)

train = data[!is.na(data$shot_made_flag),]
test = data[is.na(data$shot_made_flag),]

#test for independence ie shots follow poisson process
#does not follow proper assumptions of poisson distribution
shots_made_ind = which(train$shot_made_flag==1)
lags = (shots_made_ind-c(0,shots_made_ind)[-length(shots_made_ind)+1])[-1]

par(mfrow=c(1,2))
lambda.hat = fitdistr(lags,"exponential")$estimate[[1]] 
hist(lags,prob=TRUE)
curve(dexp(x,lambda.hat),add=TRUE)

qqplot(x=qexp(ppoints(1000),lambda.hat),y=lags)
abline(a=0,b=1)
par(mfrow=c(1,1))

#runs test for independence
runs.test(factor(train$shot_made_flag),alternative="two.sided")

#data visualization