#Kobe caret xgb classifier
#load libraries
library(plyr)
library(dplyr)
library(boot)
library(glmnet)
library(caret)
library(gbm)
library(e1071)
library(xgboost)

#read data
setwd("C:/Users/bjkwok/Documents/Personal - Not Backed Up - Aucune sauvegarde/References/Kobe")
data = read.table("data.csv",sep=",",header=TRUE)

#data definition adjustments
data$game_date = as.Date(data$game_date)
data$game_day = as.factor(weekdays(data$game_date))
data$game_month = as.factor(format(data$game_date,'%m'))
data$game_year = as.factor(format(data$game_date,'%Y'))
data$playoffs = factor(ifelse(data$playoffs==1,"Yes","No"))
data$time_remaining = data$minutes_remaining*60 + data$seconds_remaining
data$shot_distance[data$shot_distance>=30] = 45
data$overtime = factor(ifelse(data$period > 4,"Yes","No"))
data$home = factor(ifelse(regexpr('@',data$matchup)==-1,"Yes","No"))
data$period = factor(paste("Period",data$period))

#combine low n action_types and replace with combined_shot_type
rare_shots = names(table(data$action_type))[table(data$action_type)<10]
data$action_type = as.character(data$action_type)
data$combined_shot_type = as.character(data$combined_shot_type)
data$action_type[which(data$action_type %in% rare_shots)] = data$combined_shot_type[which(data$action_type %in% rare_shots)]
data$action_type = factor(data$action_type)
data$combined_shot_type = factor(data$combined_shot_type)

#extract test shot_id
shot_id = subset(data,is.na(shot_made_flag))[,"shot_id"]

#remove unnecessary variables
removeNames = c("game_event_id","game_id","lat","lon",
		    "minutes_remaining","seconds_remaining","team_id",
		    "team_name","matchup","shot_id","game_date")
data = data[,!(names(data) %in% removeNames)]

#separate training and testing data
train = data[!is.na(data$shot_made_flag),]
test = data[is.na(data$shot_made_flag),]

#set predictor and outcome matrix
outcomeName = "shot_made_flag"
predictorsNames = names(train)[names(train)!=outcomeName]
y = train[,outcomeName]
x = data.matrix(train[,predictorsNames],rownames.force=NA)

#remove shot_made_flag from data
train$shot_made_flag = NULL
test$shot_made_flag = NULL

#create DMatrix
Dtrain = xgb.DMatrix(data=x,label=y,missing=NaN)

watchlist = list(x=Dtrain)

set.seed(123)

#xgboost fitting with optimal param
param = list(
	objective = "binary:logistic",
	booster = "gbtree",
	eval_metric = "logloss",
	eta = 0.4,
	max_depth = 3,
	subsample = 0.40,
	colsample_bytree = 0.40
)

#fit model with params above
xgb = xgb.cv(
	data = x,
	label = y,
	params = param,
	nrounds = 1000,
	watchlist = watchlist,
	verbose = F,
	maximize = F,
	nfold = 10,
	print.every.n = 1,
	early.stop.round = 10
)

#identify optimal nrounds
bestRound = which.min(as.matrix(xgb)[,3])

#train xgboost model
xgb = xgb.train(
	data = Dtrain,
	params = param,
	nrounds = bestRound,
	watchlist = watchlist,
	verbose = 1,
	maximize = F
)

#set test df as matrix
test = data.matrix(test,rownames.force=NA)

#predict and output to csv
shot_made_flag = predict(xgb,test)
output = data.frame(shot_id,shot_made_flag)
write.csv(output,"kobe-xgb1.csv",row.names=F)