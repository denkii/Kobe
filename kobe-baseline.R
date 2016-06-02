#long run average baseline prediction for kobe
#load libraries
library(dplyr)
library(ggplot2)

#load data
setwd("C:/Users/bjkwok/Documents/Personal - Not Backed Up - Aucune sauvegarde/References/Kobe")
data = read.table("data.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)

#separate test and train
train = data[!is.na(data$shot_made_flag),]
test = data[is.na(data$shot_made_flag),]

#proportion table
baseline = prop.table(table(train$combined_shot_type,train$shot_made_flag),1)[,1]
baseline = as.data.frame(baseline)
pred = merge(test,baseline,by.x="combined_shot_type",by.y="row.names",all.x=T)
pred = pred[,c("shot_id","baseline")]
pred = arrange(pred,shot_id)
names(pred) = c("shot_id","shot_made_flag")
write.csv(pred,file="baseline.csv",row.names=F)