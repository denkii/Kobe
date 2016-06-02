#Kobe Random Forest classifier
#load libraries
library(dplyr)
library(randomForest)

#read data
setwd("C:/Users/bjkwok/Documents/Personal - Not Backed Up - Aucune sauvegarde/References/Kobe")
data = read.table("data.csv",sep=",",header=TRUE)

#data definition adjustments
data$game_date = as.Date(data$game_date)
data$game_month = as.numeric(format(data$game_date,'%m'))
data$game_year = as.numeric(format(data$game_date,'%Y'))
data$playoffs = factor(data$playoffs)

#create home variable
data$home = factor(as.numeric(regexpr('@',data$matchup)==-1))

#separate training and testing data
train = data[!is.na(data$shot_made_flag),]
test = data[is.na(data$shot_made_flag),]

#summarise shots taken/missed by game_id in regular season
grp = group_by(train,game_id)
game = data.frame(summarise(grp,sum(shot_made_flag)),tally(grp)[,2])
game$pct = ts(shots$sum.shot_made_flag./shots$n)

#set data for logistic regression
train = train %>% 
	  group_by(combined_shot_type,shot_zone_basic,home,season) %>%
	  summarise(y=sum(shot_made_flag),m=n())

#fit logistic regression 
lr = glm(cbind(y,m)~combined_shot_type +  
			  shot_zone_basic +
                    home + season,
			  family=binomial,data=train)
summary(lr)

#check residual plot
plot(lr$fitted.values,residuals.glm(lr,"deviance"),ylim=c(-5,5))
abline(h=2); abline(h=-2)

#predict on logistic regression
pred.lr = predict(lr,newdata=test,type="response")
output = data.frame(shot_id=test$shot_id,shot_made_flag=pred.lr)

#output to csv
#write.csv(output,file="kobe-lr1.csv",row.names=F)