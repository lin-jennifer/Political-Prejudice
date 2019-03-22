#Political Party of State - Generic maps
#Jennifer Lin
#CCES 2016 - https://cces.gov.harvard.edu/

#Load R Data
#cces = read.table("/Users/JenniferLin/Desktop/Working/CCES/CCES2016.tab", header = TRUE)
load("/Users/JenniferLin/Desktop/Working/CCES/CCES16_Common_OUTPUT_Feb2018_VV.RData")

#Set Working Directory
setwd("~/Desktop/Working/Political-Prejudice/Map")

#Load Packages
library(zipcode)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(car)
library(mapproj)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(car)

#Look At Zip Code Variable
str(x$lookupzip)
#rename variable
x$zip = x$lookupzip

#Look at Respondent Party
str(x$pid3)

#Get USA MAp
usa <- map_data("state")
usa
ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3)+
  guides(fill=FALSE)

#Generate longitude and latitude from CCES data points
data(zipcode)
xcode = merge(x, zipcode, by.x='zip', by.y='zip')
xcode$zip = clean.zipcodes(xcode$zip)
xcode$zip

#Remove Missing from Party ID
xcode$pid3
xcode<-xcode[!(xcode$pid3=="Not sure"),]
xcode<-xcode[!(xcode$pid3=="Other"),]
xcode<-xcode[!(xcode$pid3=="NA"),]

#Remove Alaska and Hawaii (Sorry)
str(xcode$state)
table(xcode$state)
xcode<-xcode[!(xcode$state=="AK"),]
xcode<-xcode[!(xcode$state=="HI"),]

#Collapse data by State
attach(xcode)
aggdata <-aggregate(xcode, by=list(inputstate, CurrentGovParty), 
                    FUN=mean, na.rm=TRUE)
print(aggdata)
detach(xcode)

#Collapse Zipcode by state
attach(zipcode)
aggzip <-aggregate(zipcode, by=list(state), 
                    FUN=mean, na.rm=TRUE)
print(aggzip)
detach(zipcode)


#Ditch the Axes
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

#Jitter Points
jitter <- position_jitter(width = 0.15, height = 0.15)

#Declare the merged variables as facotrs
aggdata$Group.2 <- as.factor(aggdata$Group.2)
aggdata$Group.1 <- as.factor(aggdata$Group.1)

aggzip$Group.1 <- as.factor(aggzip$Group.1)

#Merge Aggdata with USA file
aggdata$state = aggdata$Group.1
aggzip$state = aggzip$Group.1
usagov = merge(aggdata, aggzip, by.x='state', by.y='state')

usagov$govpid = usagov$Group.2

#Create the map

ggplot(usagov, aes(longitude.x, latitude.y)) +
  geom_polygon(data = usa, mapping = aes(x = long, y = lat, group = group), color = "black", fill = "grey94", size = .25)+
  geom_point(aes(color = Group.2), na.rm = TRUE)+
  coord_fixed(1.3)+ theme_bw()  + scale_color_manual("Party ID", values=c("Republican" = "red", "Democratic" = "blue", "Independent" = "plum1"))

#Use the Left Join command
aggdata$state = tolower(aggdata$Group.1)
usa$state = usa$region
state_gov = left_join(usa, aggdata)  

#Remove NA Rows
state_gov<-state_gov[!(state_gov$Group.2=="NA"),]

#Healey's Way https://socviz.co/maps.html

library(tidyverse)
p0 <- ggplot(data = state_gov,
             mapping = aes(x = long, y = lat,
                           group = group, fill = Group.2), na.rm = TRUE)
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
  theme_bw()+coord_fixed(1.3)
p2 <- p1 + scale_fill_manual("Governor Party", values = c("Republican" = "red", "Democratic" = "blue"))  +
  theme(text = element_text(size = 18, colour="black"),
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black")) + guides(color = guide_legend(override.aes = list(size=5)))
  #labs(title = "State Governor Party after 2016", fill = NULL)
p2 +ditch_the_axes

ls(state_gov)


