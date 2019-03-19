#Create Map of Individuals and Party in US
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
library(fiftystater)

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
xcode

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

#Create US Plot
usplot = ggplot(data = usa) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group), color = "black", fill = "grey") +
  geom_point(data=xcode, aes(x=longitude, y=latitude, colour=pid3), na.rm = TRUE)+
  coord_fixed(1.3)+ theme_bw() +
  ditch_the_axes

pid3 = c("Republican" = "red", "Democrat" = "blue", "Independent" = "white", "NA" = "NA")
usplot + scale_colour_manual(values=pid3, 
                    name="Party ID",
                  breaks=c("Democrat", "Republican", "Independent", "NA"),
                    labels=c("Democrat", "Republican", "Independent", "NA"))

usplot

#Shrink the Map to Just Florida
#Only include Florida
florida = subset(xcode, state == "FL")
floridamap = subset(usa, region == "florida")

#Get Florida map
ggplot(data = floridamap) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3)+
  guides(fill=FALSE)

ggplot(data = floridamap) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group), color = "black", fill = "grey") +
  geom_point(data=florida, aes(x = longitude, y = latitude, colour=pid3))+
  coord_fixed(1.3) + theme_bw() +
  ditch_the_axes
