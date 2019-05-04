########## General Information #################
#Purpose: Clean data for Project
#Author: Jennifer Lin
#Requires: ANES 2016 Time Series Data 
#Access: https://electionstudies.org/project/2016-time-series-study/
###############################################

#Load in data and change the name
load("~/Desktop/Data/ANES/DS0001/anes2016.rda")
anes2016 = da36824.0001

#Set the Working Directory
getwd()
setwd("~/Desktop/Working/Political-Prejudice/AS-Project")

#Load Libraries
library(car)
library(dplyr)

############## Clean Variables ##############

#Feelings towards Democrat Presidential Candidate
table(anes2016$V161086)
anes2016$clinton <- car::recode(anes2016$V161086, "-88:-99 = 'NA'")
table(anes2016$clinton)

#Feelings towards Republican Presidential Candidate
table(anes2016$V161087)
anes2016$trump <- car::recode(anes2016$V161087, "-88:-99 = 'NA'")
table(anes2016$trump)

#Participant Party - 7-category
str(anes2016$V161158X)
table(anes2016$V161158X)
anes2016$pid7 = car::recode(anes2016$V161158X, "-8:-9 = 'NA'")
anes2016$pid7 <- as.numeric(as.factor(anes2016$pid7))
table(anes2016$pid7)

#V161155 - Party ID 3-point
str(anes2016$V161155)
table(anes2016$V161155)
anes2016$pid3 = car::recode(anes2016$V161155, "-8:-9 = 'NA'")
table(anes2016$pid3)

#Participant Ideology - 7-point
str(anes2016$V162171)
table(anes2016$V162171)
anes2016$ideo7 = car::recode(anes2016$V162171, "-6:-9 = 'NA'")
anes2016$ideo7 <- recode(anes2016$ideo7, "(99) 99. Haven't thought much about this" = "NA")
anes2016$ideo7 <- as.numeric(as.factor(anes2016$ideo7))
anes2016$ideo7 <- car::recode(anes2016$ideo7, "8 = 'NA'")
table(anes2016$ideo7)

#Participant Education
table(anes2016$V161270)
str(anes2016$V161270)
anes2016$education <- car::recode(anes2016$V161270, "-9 = 'NA'")
anes2016$education <- recode(anes2016$education, "(90) 90. Other specify given as: high school graduate" = "NA")
anes2016$education <- recode(anes2016$education, "(95) 95. Other SPECIFY" = "NA")
anes2016$education <- as.numeric(as.factor(anes2016$education))
table(anes2016$education)

#Participant Income
table(anes2016$V161361X)
str(anes2016$V161361X)
anes2016$income <- car::recode(anes2016$V161361X, "-5:-9 = 'NA'")
anes2016$income <- as.numeric(as.factor(anes2016$income))
table(anes2016$income)

#Feeling towards the Democrats
str(anes2016$V161095)
table(anes2016$V161095)
anes2016$feeldem = car::recode(anes2016$V161095, "-88:-99 = 'NA'")
table(anes2016$feeldem)
psych::describe(anes2016$feeldem, na.rm = TRUE)

#Feelings towards the Republicans
str(anes2016$V161096)
table(anes2016$V161096)
anes2016$feelrep = car::recode(anes2016$V161096, "-88:-99 = 'NA'")
table(anes2016$feelrep)
psych::describe(anes2016$feelrep, na.rm = TRUE)

############### Export data ####################
#Pull out the variables
which( colnames(anes2016)=="clinton" ) #1197
which( colnames(anes2016)=="feelrep" ) #1205

#Create data frame
composite <- anes2016[,c(2, 1197:1205)]

#Export data
library(rio)
export(composite, "anes16.csv")
export(composite, "anes16.sav")

