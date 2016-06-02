#Kobe caret gbm and glmnet classifier
#load libraries
library(plyr)
library(dplyr)
library(boot)
library(glmnet)
library(caret)
library(gbm)
library(e1071)

#read data
setwd("C:/Users/bjkwok/Documents/Personal - Not Backed Up - Aucune sauvegarde/References/Kobe")
data = read.table("data.csv",sep=",",header=TRUE)

#data definition adjustments
data$game_date = as.Date(data$game_date)
data$game_month = as.factor(format(data$game_date,'%m'))
data$game_year = as.factor(format(data$game_date,'%Y'))
data$playoffs = factor(ifelse(data$playoffs==1,"Yes","No"))
data$time_remaining = data$minutes_remaining*60 + data$seconds_remaining
data$shot_distance[data$shot_distance>=30] = 45
data$overtime = factor(ifelse(data$period > 4,"Yes","No"))
data$home = factor(ifelse(regexpr('@',data$matchup)==-1,"Yes","No"))
data$period = factor(paste("Period",data$period))

#combine low n action_types and replace with combined_shot_type
rare_shots = names(table(train$action_type)[table(train$action_type)<10])
data$action_type = as.character(data$action_type)
data$combined_shot_type = as.character(data$combined_shot_type)
data$action_type[which(data$action_type %in% rare_shots)] = data$combined_shot_type[which(data$action_type %in% rare_shots)]
data$action_type = factor(data$action_type)
data$combined_shot_type = factor(data$combined_shot_type)

#extract test shot_id
shot_id = subset(data,is.na(shot_made_flag))[,"shot_id"]

#remove unnecessary variables
removeNames = c("game_event_id","game_id","lat","loc_x","loc_y","lon",
		    "minutes_remaining","seconds_remaining","team_id",
		    "team_name","matchup","shot_id","game_date")
data = data[,!(names(data) %in% removeNames)]

#separate training and testing data
train = data[!is.na(data$shot_made_flag),]
train$shot_made_flag = factor(ifelse(train$shot_made_flag == 1,"Yes","No"))
test = data[is.na(data$shot_made_flag),]

#set predictor and outcome df
outcomeName = "shot_made_flag"
predictorsNames = names(train)[names(train)!=outcomeName]
y = train[,outcomeName]
x = train[,predictorsNames]

#set resampling of data
objControl = trainControl(method='cv',number=10,returnResamp='none',
				  summaryFunction=multiClassSummary,classProbs=TRUE)

#run gbm model
objModel = train(x,y,method="gbm",trControl=objControl,
		     metric="logLoss",preProc=c("center","scale"),verbose=F)
summary(objModel)
expLL = mean(objModel$results[,"logLoss"])
expLL

#predict probabilities for test set
predNames = as.character(summary(objModel)$var)
shot_made_flag = predict(objModel,newdata=test[,predNames],type="prob")["Yes"][,1]
output = data.frame(shot_id,shot_made_flag)

#write csv
#write.csv(output,"kobe-gbm-new.csv",row.names=F)