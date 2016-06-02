#load libraries
library(dplyr)
library(ggplot2)

setwd("C:/Users/bjkwok/Documents/Personal - Not Backed Up - Aucune sauvegarde/References/Kobe")
data = read.table("data.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)

train = data[!is.na(data$shot_made_flag),]
test = data[is.na(data$shot_made_flag),]

train$shot_made_flag = factor(train$shot_made_flag,levels=c("1","0"))
train$playoffs = factor(train$playoffs)

pplot = function(feat){
	  feat = substitute(feat)
	  ggplot(data=train,aes_q(x=feat)) +
	  scale_y_continuous(breaks=seq(0,1,0.05)) + 
	  geom_bar(aes(fill=shot_made_flag),stat="count",position="fill") +
	  scale_fill_brewer(palette="Set1",direction=-1) +
	  ggtitle(paste("accuracy by",feat))
}
 
courtplot = function(feat){
		feat = substitute(feat)
		train %>%
		ggplot(aes(x=lon,y=lat)) + 
		geom_point(aes_q(color=feat),alpha=0.7,size=3) +
		ylim(c(33.7,34.0883)) + 
		scale_color_brewer(palette="Set1")+
		theme_void() +
		ggtitle(paste(feat))
}

courtplot(combined_shot_type)

ggplot() +
	geom_point(data=filter(train,combined_shot_type=="Jump Shot"),
	aes(x=lon,y=lat),color="grey",alpha=0.3,size=2) +
	geom_point(data=filter(train,combined_shot_type!="Jump Shot"),
	aes(x=lon,y=lat,color=combined_shot_type),alpha=0.7,size=2) +
	ylim(c(33.7,34.0883)) + 
	scale_color_brewer(palette="Set1") +		
	theme_void() +
	ggtitle("Shot Types")	

pplot(combined_shot_type)
pplot(shot_type)
pplot(shot_zone_area)
pplot(shot_zone_basic)
pplot(shot_distance)
pplot(opponent)
pplot(seconds_remaining)
pplot(shot_zone_range)
pplot(playoffs)
pplot(period)