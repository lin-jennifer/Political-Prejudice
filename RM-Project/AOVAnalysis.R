################################################
#File: Participation and Knowledge ANOVA Analysis
#Author: Jennifer Lin
#Requirements: "RMProjData.csv" - Refer to `Management.R` for the construction of this file
#Root File: ANES 2016 Time Series (https://electionstudies.org/project/2016-time-series-study/)
#Purpose: Analysis of the Interaction between knowledge and participation on feelings towards political others
###############################################

#Load in the Data - CHANGE FILE PATH IF STORED IN DIFFERENT PLACE!
getwd()
data <- read.csv("~/Desktop/Working/Political-Prejudice/RM-Project/RMProjData.csv", header = TRUE)

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

#Create Political Knowledge variable
data$knowledge <- rowSums(data[,c('biden', 'ryan', 'merkel', 'putin', 'roberts')], na.rm=TRUE)

#Create Political Participation Variable
data$participation <- rowSums(data[, c('talk', 'meeting', 'wear', 'work', 'campaigndon', 'partydon', 'attend', 'petition', 'post', 'contact', 'vote16')], na.rm = TRUE)



####################################################
#Two way Anota for participation and knowledge on feelings

#Feelings towards Democrats
dem <- aov(feeldem ~ as.factor(knowledge)*as.factor(participation), data = data)
summary(dem)

#Comupte Cell means for feelings towards dems
groups <- group_by(data, knowledge, participation)
dem.feel <- summarise(groups,
                      mean = mean(feeldem, na.rm=TRUE),
                      sd = sd(feeldem, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
dem.feel
ggplot(dem.feel, aes(x=participation, y=mean, fill = knowledge )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Feelings towards Democrats") +
  xlab("Political Participation") +
  ylab("Feelings towards Democrats") + theme_bw()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual("Political Knowledge")