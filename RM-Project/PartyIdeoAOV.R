########## General Information #################
#File: Participation and Knowledge ANOVA Analysis
#Author: Jennifer Lin
#Requirements: "RMProjData.csv" - Refer to `Management.R` for the construction of this file
#Root File: ANES 2016 Time Series (https://electionstudies.org/project/2016-time-series-study/)
#Purpose: ANOVA for Party/Ideology and Knowledge/Participation on feelings towards parties/ideologies
#         - AOV Party x Ideology on Feelings towards Dems
#         - AOV Party x Ideology on Feelings towards Reps
#         - AOV Party x Ideology on Feelings towards Dems
#         - AOV Party x Ideology on Feelings towards Reps
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
library(phia) #Contrasts
library(ggplot2) #Graphics
library(stargazer) #Exporting Plots

################## Clean Data #######################
#Remove "No Party", "other" and NAs from party
data<-data[!(data$party=="NA"),]
data<-data[!(data$party=="No Party"),]   
data<-data[!(data$party=="Other"),]
table(data$party)

#Recode Party to numeric
table(data$party)
data$partynum <- recode(data$party, "Democrat" = '1')
data$partynum <- recode(data$partynum, "Independent" = '2')
data$partynum <- recode(data$partynum, "Republican" = '3')
data$partynum <- recode(data$partynum, "No Party" = 'NA')
data$partynum <- recode(data$partynum, "Other" = 'NA')
table(data$partynum)

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

#Recode Ideology Variable to numeric
table(data$ideo3re)
data$ideo3num <- recode(data$ideo3re, "Liberal" = '1')
data$ideo3num <- recode(data$ideo3num, "Moderate" = '2')
data$ideo3num <- recode(data$ideo3num, "Conservative" = '3')
data$ideo3num <- recode(data$ideo3num, "NA" = 'NA')
table(data$ideo3num)

#Declare variables as factors for graphing purposes
data$partyfactor <- factor(data$partynum, 
                           levels = c(1, 2, 3),
                           labels = c("Democrat", "Independent", "Republican"))

data$ideofactor <- factor(data$ideo3num, 
                          levels = c(1, 2, 3),
                          labels = c("Liberal", "Moderate", "Conservative"))
