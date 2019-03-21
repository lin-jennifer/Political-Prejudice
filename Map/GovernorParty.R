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


#Current Governor Party
table(xcode$CurrentGovParty)
xcode$inputstate

library(forcats)
xcode$inputstate <- fct_explicit_na(xcode$inputstate, na_level = "(Missing)")


statefill = group_by(xcode, CurrentGovParty, inputstate)

ggplot() + 
  geom_polygon(data = usa, mapping = aes(x = long, y = lat, group = group, fill = region),  size = .25) +
  geom_polygon(data = xcode, mapping = aes(x = longitude, y = latitude, fill = CurrentGovParty))
  coord_fixed(1.3)+ theme_bw() +
  theme(text = element_text(size = 18, colour="black"),
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black")) + guides(color = guide_legend(override.aes = list(size=5)))+
  ditch_the_axes 

  
  