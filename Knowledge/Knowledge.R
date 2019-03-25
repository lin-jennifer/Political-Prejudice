#Political Knowledge
#Jennifer Lin
#ANES 2016 Time Series 

#Load in data and change the name
load("/Users/JenniferLin/Desktop/Data/ANES/DS0001/anes2016.rda")
anes2016 = da36824.0001

#Set the Working Directory
getwd()
setwd("~/Desktop/Working/Political-Prejudice/Knowledge")

#Load Libraries
library(car)
library(dplyr)

#V161155 - Party ID 3-point
str(anes2016$V161155)
table(anes2016$V161155)
anes2016$pid3 = car::recode(anes2016$V161155, "-8:-9 = 'NA'")
anes2016$pid3

#Rid implicit NAs for the party id variable
library(forcats)
anes2016$pid3<- fct_explicit_na(anes2016$pid3, na_level = "(Missing)")

#Recode PID3
anes2016$party = recode(anes2016$pid3, "(0) 0. No preference (FTF ONLY)" = "No Party", "(1) 1. Democrat" = "Democrat", "(2) 2. Republican" = "Republican", "(3) 3. Independent" = "Independent", "(5) 5. Other party SPECIFY" = "Other", "(Missing)" = "NA" )
table(anes2016$party)

#Office Recall - Delli Carpini and Keeter Model

#V162072 - Biden
str(anes2016$V162072)
table(anes2016$V162072) #Check original distribution
anes2016$biden = car::recode(anes2016$V162072, "-6:-7 = 'NA'")
anes2016$biden = as.numeric(as.factor(anes2016$biden))
str(anes2016$biden)
table(anes2016$biden) #Check before recoding values
anes2016$biden = car::recode(anes2016$biden, "1 = 0")
anes2016$biden = car::recode(anes2016$biden, "2 = 1")
table(anes2016$biden) #Check after and match to original distribution

#V162073A - Ryan

#V162074A - Merkel

#V162075A - Putin

#V162076B - Roberts