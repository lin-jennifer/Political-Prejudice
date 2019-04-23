########## General Information #################
#File: Convert Data
#Author: Jennifer Lin
#Purpose: Code to convert CSV to SPSS, Stata and SAS files - USE AS NEEDED
#Root data: ANES 2016 Time Series ( https://electionstudies.org/project/2016-time-series-study/)
#Git Repository: https://github.com/lin-jennifer/Political-Prejudice.git
###################################################

#How to Use this File:
# 1. Load in the data and packages in the "Load Data" section
# 2. Use the section finder at the bottom of this script grid (middle of the screen to the left) to find the platform you need to export to
# 3. Execute that specific command


########### Load Data #############
#Load in the Data - CHANGE FILE PATH IF STORED IN DIFFERENT PLACE!
getwd()
data <- read.csv("~/Desktop/Working/Political-Prejudice/RM-Project/RMProjData.csv", header = TRUE, na.strings=c(""," ","NA"))

#Set Working Directory - tells R where to save the output graphs from this script
#CHANGE IT TO WHERE YOU WANT THE OUTPUT FILE TO GO
setwd("~/Desktop/Working/Political-Prejudice/RM-Project")

#Load packages
#Install packages by un-commenting the following line and relacing the package in parentheses with package needed
#install.packages("PACKAGE")
install.packages("rio")
library(rio) #Convert data
library(haven) #Working with SAV and DTA files

########## Stata #########
export(data, "RMProjData.dta")

########## SPSS ##########
export(data, "RMProjData.sav")

########## SAS ###########
export(data, "RMProjData.sas7bdat")

####### For More Information ############
# For More Information on Exporting Data
# https://cran.r-project.org/web/packages/rio/vignettes/rio.html




