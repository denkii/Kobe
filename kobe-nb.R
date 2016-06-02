#Kobe Naive Bayes implementation with ARIMA prior
#Manual implementation to use ARIMA prior probability

#load libraries
library(dplyr)

#read data
setwd("C:/Users/bjkwok/Documents/Personal - Not Backed Up - Aucune sauvegarde/References/Kobe")
data = read.table("data.csv",sep=",",header=TRUE)

#data definition adjustments
data$game_date = as.Date(data$game_date)
data$game_month = as.numeric(format(data$game_date,'%m'))
data$game_year = as.numeric(format(data$game_date,'%Y'))
data$playoffs = factor(data$playoffs)

#create home variable
data$home = as.numeric(regexpr('@',data$matchup)==-1)

#separate training and testing data
train = data[!is.na(data$shot_made_flag),]
test = data[is.na(data$shot_made_flag),]

#summarise shots taken/missed by game_id in regular season
grp = group_by(train,game_id)
game = data.frame(summarise(grp,sum(shot_made_flag)),tally(grp)[,2])
game$pct = ts(shots$sum.shot_made_flag./shots$n)

#fit arima model to all data (leakage)
#sufficient data pts required to fit ARIMA model on prior probability
arima.fit = auto.arima(game$pct)

#1-ahead prior prediction for each game
game$prior = NA
game[1:2,"prior"]=as.numeric(prop.table(table(train$shot_made_flag))
for (i in 3:nrow(game)){
	game.sub = subset(game,game_id<game[i,"game_id"])
	fit.sub = arima(game.sub$pct,order=c(0,1,1),fixed=arima.fit$coef)
	game[i,"prior"] = as.numeric(predict(fit.sub,n_ahead=1)$pred)
}

#apply naive bayes from all train data (leakage)
#use predictors combined_shot_type,shot_zone_basic
llh.shot = prop.table(table(train$combined_shot_type,train$shot_made_flag),2)
llh.shot = data.frame(combined_shot_type=row.names(llh.shot),
			    type0=as.numeric(llh.shot[,1]),
			    type1=as.numeric(llh.shot[,2]))
llh.zone = prop.table(table(train$shot_zone_basic,train$shot_made_flag),2)
llh.zone = data.frame(shot_zone_basic=row.names(llh.zone),
			    zone0=as.numeric(llh.zone[,1]),
			    zone1=as.numeric(llh.zone[,2]))
llh.home = prop.table(table(train$home,train$shot_made_flag),2)
llh.home = data.frame(home=row.names(llh.home),
			    home0=as.numeric(llh.home[,1]),
			    home1=as.numeric(llh.home[,2]))


#merge relevant probabilities into test df
test = merge(test,game[,c("game_id","prior")],all.x=T,by="game_id")
test = merge(test,llh.shot,all.x=T,by="combined_shot_type")
test = merge(test,llh.zone,all.x=T,by="shot_zone_basic")
test = merge(test,llh.home,all.x=T,by="home")
test = arrange(test,shot_id)
test$prior[is.na(test$prior)]=as.numeric(prop.table(table(train$shot_made_flag))[2])

#calculate 
test$shot_made_flag = (test$prior*test$type1*test$zone1*test$home1)/
			    ((test$prior*test$type1*test$zone1*test$home1)+
                       ((1-test$prior)*test$type0*test$zone0*test$home0))

#write to csv
output = test[,c("shot_id","shot_made_flag")]
write.csv(output,file="kobe-nb1.csv",row.names=F)
