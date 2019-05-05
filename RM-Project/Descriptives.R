########## General Information #################
#File: Descriptive Statistics
#Author: Jennifer Lin
#Requirements: "RMProjData.csv" - Refer to `Management.R` for the construction of this file
#Root File: ANES 2016 Time Series (https://electionstudies.org/project/2016-time-series-study/)
#Purpose: Run Descriptive Statistics
###############################################

############# Interview Characteristics #################
#Load in data and change the name
getwd()
data <- read.csv("~/Desktop/Working/Political-Prejudice/RM-Project/RMProjData.csv", header = TRUE, na.strings=c(""," ","NA"))

#Set the Working Directory
getwd()
setwd("~/Desktop/Working/Political-Prejudice/RM-Project")

#Library
library(car)
library(psych)

#Party ID
table(data$pid3)

$Ideology
table(data$ideo3)
table(data$ideo7)
