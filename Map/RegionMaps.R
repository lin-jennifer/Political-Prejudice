#Create Map of Individuals and Party by US Regions
#Jennifer Lin
#CCES 2016

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

#Import County Data
counties <- map_data("county")
counties

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
jitter <- position_jitter(width = 0.1, height = 0.1)

########################################
#Northeast
northeast = subset(xcode, state == c('MA', 'ME', 'NH', 'VT', 'CT', 'RI', 'NY', 'PA', 'NJ'))
table(northeast$state)
neastmap = subset(usa, region %in% c("massachusetts", "new york", "new jersey", "pennsylvania", "rhode island", "connecticut", "vermont", "new hampshire", "maine") )
table(neastmap$region)

ggplot(northeast, aes(longitude, latitude)) + 
  geom_polygon(data = neastmap, mapping = aes(x = long, y = lat, group = group), color = "black", fill = "grey94", size = .25) +
  geom_point(aes(color = pid3), size = .2, na.rm = TRUE, position = jitter)+
  coord_fixed(1.3)+ theme_bw() + 
  theme(text = element_text(size = 18, colour="black"),
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black")) + guides(color = guide_legend(override.aes = list(size=5)))+
  ditch_the_axes + scale_color_manual("Party ID", values=c("Republican" = "red", "Democrat" = "blue", "Independent" = "plum1"))

