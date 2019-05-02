########## General Information #################
#File: Participation and Knowledge Multiple Regression Analysis
#Author: Jennifer Lin
#Requirements: "RMProjData.csv" - Refer to `Management.R` for the construction of this file
#Root File: ANES 2016 Time Series (https://electionstudies.org/project/2016-time-series-study/)
#Purpose: Analysis of the Interaction between knowledge and participation on feelings towards political others
###############################################

#Load in the Data - CHANGE FILE PATH IF STORED IN DIFFERENT PLACE!
getwd()
data <- read.csv("~/Desktop/Working/Political-Prejudice/RM-Project/RMProjData.csv", header = TRUE, na.strings=c(""," ","NA"))

#Set Working Directory - tells R where to save the output graphs from this script
setwd("~/Desktop/Working/Political-Prejudice/RM-Project")

#Load packages
#Install packages by un-commenting the following line and relacing the package in parentheses with package needed
#install.packages("PACKAGE")
library(ggplot2) #For graphics
library(car) #For general statistics
library(ggpubr) #For publishing quality graphs
library(lsr) #contrasts for ANOVA
library(dplyr) #Create summary graphs
library(coefplot) #Coefficient plot

#Create Political Knowledge variable
data$knowledge <- rowSums(data[,c('biden', 'ryan', 'merkel', 'putin', 'roberts')], na.rm=TRUE)

#Create Political Participation Variable
data$participation <- rowSums(data[, c('talk', 'meeting', 'wear', 'work', 'campaigndon', 'partydon', 'attend', 'petition', 'post', 'contact', 'vote16')], na.rm = TRUE)

#Remove "No Party", "other" and NAs from party
data<-data[!(data$party=="NA"),]
data<-data[!(data$party=="No Party"),]   
data<-data[!(data$party=="Other"),]
table(data$party)
str(data$party)

#Clean Ideology Variables - Use 7 category since more people respond to this
str(data$ideo7)
table(data$ideo7)
data$ideo7 <- recode(data$ideo7, "(99) 99. Haven't thought much about this" = "NA")
table(data$ideo7) #Check to make sure the recode was done correctly

#Group Ideology to 3 cateogry
data$ideo3re <- recode(data$ideo7, "(01) 1. Extremely liberal" = "Liberal", "(02) 2. Liberal" = "Liberal", "(03) 3. Slightly liberal" = "Liberal", "(04) 4. Moderate/ middle of the road" = "Moderate", "(05) 5. Slightly conservative" = "Conservative", "(06) 6. Conservative" = "Conservative", "(07) 7. Extremely conservative" = "Conservative")
table(data$ideo3re)

#Rid implicit NAs for the ideology variable
library(forcats)
data$ideo3re<- fct_explicit_na(data$ideo3re, na_level = "NA")
table(data$ideo3re)

#Rid NAs from the data for ideology
data<-data[!(data$ideo3re=="NA"),]

#Condense Knowledge and Participation Variables to low/medium/high concept
#Political Knowledge
#1 = knows little, 2 = knows a lot
table(data$knowledge)
data<-data[!(data$knowledge=="NA"),]
data$know <- car::recode(data$knowledge, "0:2 = 1")
data$know <- car::recode(data$know, "3:5 = 2")
table(data$know)

#Political participation
#Divided odly to capture the population based on data, rather than scale by the possible values
boxplot(data$participation) #Used to make the decision based on where the 25, 50 and 75 percent were
#1 = participates little, 2 = participates moderately, 3 = participates a lot
table(data$participation)
data<-data[!(data$participation=="NA"),]
data$part <- car::recode(data$participation, "0:1 = 1")
data$part <- car::recode(data$part, "2:4 = 2")
data$part <- car::recode(data$part, "5:11 = 3")
table(data$part)

#Subsection data by political ideology identification
lib.sub <- data[data$ideo3re=="Liberal",]
con.sub <- data[data$ideo3re=="Conservative",]
mod.sub <- data[data$ideo3re=="Moderate",]

#Subsection data by political party identification
dem.sub <- data[data$party=="Democrat",]
rep.sub <- data[data$party=="Republican",]
ind.sub <- data[data$party=="Independent",]

############### Multiple Regression for entire model ############

#Feelings towards Democrat
demfeel <- lm(feeldem ~ participation + knowledge + party, data=data)
summary(demfeel)
coefplot(demfeel) #Coeficient Plot
plot(demfeel) #Residual plot

#Feelings towards Republicans
repfeel <- lm(feelrep ~ participation + knowledge + party, data=data)
summary(repfeel)
coefplot(repfeel) #Coeficient Plot
plot(repfeel) #Residual plot

#Feelings towards Liberals
libfeel <- lm(feellib ~ participation + knowledge + ideo3re, data = data)
summary(libfeel)
coefplot(libfeel)

#Feelings towards Conservatives
consfeel <- lm(feelcons ~ participation + knowledge + ideo3re, data = data)
summary(consfeel)
coefplot(consfeel)
