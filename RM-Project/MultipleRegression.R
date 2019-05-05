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

#Use 7-category Party ID as numeric
#1 = Strong Democrat, 7 = Strong Republican
str(data$pid7)
table(data$pid7)
data$pid7 <- as.numeric(as.factor(data$pid7))
table(data$pid7)

#Clean Ideology Variables - Use 7 category since more people respond to this
#1 = Extremely Liberal, 7 = Extremely Conservative
str(data$ideo7)
table(data$ideo7)
data$ideo7 <- recode(data$ideo7, "(99) 99. Haven't thought much about this" = "NA")
data$ideo7 <- as.numeric(as.factor(data$ideo7))
table(data$ideo7) #Check to make sure the recode was done correctly


############### Multiple Regression for entire model ############

#Feelings towards Democrat
demfeel <- lm(feeldem ~ participation + knowledge + pid7, data=data)
summary(demfeel)
coefplot(demfeel) #Coeficient Plot

#Feelings towards Republicans
repfeel <- lm(feelrep ~ participation + knowledge + pid7, data=data)
summary(repfeel)
coefplot(repfeel) #Coeficient Plot

#Feelings towards Liberals
libfeel <- lm(feellib ~ participation + knowledge + ideo7, data = data)
summary(libfeel)
coefplot(libfeel)

#Feelings towards Conservatives
consfeel <- lm(feelcons ~ participation + knowledge + ideo7, data = data)
summary(consfeel)
coefplot(consfeel)

########## Testing for Interactions ################

#Feelings towards Democrat
demfeel <- lm(feeldem ~ (participation + knowledge + pid7)^2, data=data)
anova(demfeel)
summary(demfeel)
coefplot(demfeel) #Coeficient Plot

#Feelings towards Republicans
repfeel <- lm(feelrep ~ (participation + knowledge + pid7)^2, data=data)
anova(repfeel)
summary(repfeel)
coefplot(repfeel) #Coeficient Plot

#Feelings towards Liberals
libfeel <- lm(feellib ~ (participation + knowledge + ideo7)^2, data = data)
anova(libfeel)
summary(libfeel)
coefplot(libfeel)

#Feelings towards Conservatives
consfeel <- lm(feelcons ~ (participation + knowledge + ideo7)^2, data = data)
anova(consfeel)
summary(consfeel)
coefplot(consfeel)
