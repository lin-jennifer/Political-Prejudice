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
str(anes2016$V162073A)
table(anes2016$V162073A) #Check original distribution
anes2016$ryan = car::recode(anes2016$V162073A, "-6:-7 = 'NA'")
anes2016$ryan = as.numeric(as.factor(anes2016$ryan))
str(anes2016$ryan)
table(anes2016$ryan) #Check before recoding values
anes2016$ryan = car::recode(anes2016$ryan, "1 = 0")
anes2016$ryan = car::recode(anes2016$ryan, "2 = 1")
table(anes2016$ryan) #Check after and match to original distribution

#V162074B - Merkel
str(anes2016$V162074B)
table(anes2016$V162074B) #Check original distribution
anes2016$merkel = car::recode(anes2016$V162074B, "-6:-7 = 'NA'")
anes2016$merkel = as.numeric(as.factor(anes2016$merkel))
str(anes2016$merkel)
table(anes2016$merkel) #Check before recoding values
anes2016$merkel = car::recode(anes2016$merkel, "1 = 0")
anes2016$merkel = car::recode(anes2016$merkel, "2 = 1")
table(anes2016$merkel) #Check after and match to original distribution

#V162075A - Putin
str(anes2016$V162075B)
table(anes2016$V162075B) #Check original distribution
anes2016$putin = car::recode(anes2016$V162075B, "-6:-7 = 'NA'")
anes2016$putin = as.numeric(as.factor(anes2016$putin))
str(anes2016$putin)
table(anes2016$putin) #Check before recoding values
anes2016$putin = car::recode(anes2016$putin, "1 = 0")
anes2016$putin = car::recode(anes2016$putin, "2 = 1")
table(anes2016$putin) #Check after and match to original distribution

#V162076B - Roberts
str(anes2016$V162076B)
table(anes2016$V162076B) #Check original distribution
anes2016$roberts = car::recode(anes2016$V162076B, "-6:-7 = 'NA'")
anes2016$roberts = as.numeric(as.factor(anes2016$roberts))
str(anes2016$roberts)
table(anes2016$roberts) #Check before recoding values
anes2016$roberts = car::recode(anes2016$roberts, "1 = 0")
anes2016$roberts = car::recode(anes2016$roberts, "2 = 1")
table(anes2016$roberts) #Check after and match to original distribution
