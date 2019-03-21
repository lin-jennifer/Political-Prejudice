#Analysis of Feeling Thermometers 
#Jennifer Lin
#ANES Time Series Cumulative 1948-2016
#https://electionstudies.org/data-center/ 

#Load in data
anescdf = read.csv("~/Desktop/Working/ANES/anes.csv", header = TRUE)

#Set Working Directory
setwd("~/Desktop/Working/Political-Prejudice/Feeling")

#Load packages
library(car)
library(ggplot2) #Graphs
library(ggpubr)
library(gmodels) #Cross Tab
library(dplyr)

#Analyze Party ID Variable
str(anescdf$VCF0303)
table(anescdf$VCF0303)

#Recode Party ID variable to omit missing data
anescdf$pid3 <- car::recode(anescdf$VCF0303, "0 = 'NA'")
na.omit(anescdf$pid3)
CrossTable(anescdf$VCF0303, anescdf$pid3)
table(anescdf$pid3)

#Year of Study
table(anescdf$VCF0004)
anescdf$year <- anescdf$VCF0004

#Clean Feeling Thermometers

#VCF0201 - Feeling Towards Democrats
table(anescdf$VCF0201)
anescdf$demfeel <- car::recode(anescdf$VCF0201, "98:99 = 'NA'")
CrossTable(anescdf$demfeel, anescdf$VCF0201)
table(anescdf$demfeel)

#VCF0202 - Feeling Towards Republicans
table(anescdf$VCF0202)
anescdf$repfeel <- car::recode(anescdf$VCF0202, "98:99 = 'NA'")
CrossTable(anescdf$repfeel, anescdf$VCF0202)
table(anescdf$repfeel)

#Change to Numeric
anescdf$demfeel <- as.numeric(as.character(anescdf$demfeel), na.rm=TRUE)
summary(anescdf$demfeel)

anescdf$repfeel <- as.numeric(as.character(anescdf$repfeel), na.rm = TRUE)
summary(anescdf$repfeel)

#Feeling Thermometer by Respondent Party and Year
na.omit(anescdf$demfeel, anescdf$repfeel)

anescdf$year<-as.factor(anescdf$year)
anescdf$pid3<-as.factor(anescdf$pid3)

str(anescdf$pid3)
str(anescdf$year)

library(forcats)
anescdf$pid3.complete <- fct_explicit_na(anescdf$pid3, na_level = "(Missing)")


#Feelings towards Democrats

demagg <- aggregate(demfeel ~ pid3 + year, anescdf, mean, na.rm = TRUE)

ggplot(data=demagg, aes(x=year, y=demfeel, group=pid3, na.rm = TRUE)) +
  geom_line(aes(linetype=pid3, color = pid3))+
  geom_point(aes(shape=pid3, color = pid3))

#Feelings Towards Republicans

repagg <- aggregate(repfeel ~ pid3 + year, anescdf, mean, na.rm = TRUE)

ggplot(data=repagg, aes(x=year, y=repfeel, group=pid3, na.rm = TRUE)) +
  geom_line(aes(linetype=pid3, color = pid3))+
  geom_point(aes(shape=pid3, color = pid3))

############################
#Party Thermometer ends at 1982. Take same logic to create feeling towards liberal and conservatives

#VCF0212 - Feelings towards conservatives
table(anescdf$VCF0212)
anescdf$consfeel <- car::recode(anescdf$VCF0212, "98:99 = 'NA'")

#VCF0211 - Feelings Towards Liberals
table(anescdf$VCF0211)
anescdf$libfeel <- car::recode(anescdf$VCF0211, "98:99 = 'NA'")

#Declare variables as numeric
anescdf$libfeel <- as.numeric(as.character(anescdf$libfeel), na.rm=TRUE)
summary(anescdf$demfeel)

anescdf$consfeel <- as.numeric(as.character(anescdf$consfeel), na.rm = TRUE)
summary(anescdf$repfeel)

#Graph of feelings towards liberals
libagg <- aggregate(libfeel ~ pid3 + year, anescdf, mean, na.rm = TRUE)

#Remove NA Party Rows
libagg<-libagg[!(libagg$pid3=="NA"),]

#Make Graph
ggplot(data=libagg, aes(x=year, y=libfeel, group=pid3, na.rm = TRUE)) +
  geom_line(aes(linetype=pid3, color = pid3))+
  geom_point(aes(shape=pid3, color = pid3))

#Graph of Feeling towards Conservatives
consagg <- aggregate(consfeel ~ pid3 + year, anescdf, mean, na.rm = TRUE)

#Remove NA Party Rows
consagg<-consagg[!(consagg$pid3=="NA"),]

#Make Graph
ggplot(data=consagg, aes(x=year, y=consfeel, group=pid3, na.rm = TRUE)) +
  geom_line(aes(linetype=pid3, color = pid3))+
  geom_point(aes(shape=pid3, color = pid3))
