#Feeling Thermometer Variables 
#Jennifer Lin

#Load in data and change the name
load("/Users/JenniferLin/Desktop/Data/ANES/DS0001/anes2016.rda")
anes2016 = da36824.0001

#Set the Working Directory
getwd()
setwd("~/Desktop/Working/Political-Prejudice/Knowledge")

#Load Libraries
library(car)
library(dplyr)
library(psych)

#Feeling Thermometer towards the Parties

#V161095 - Feeling towards the democrats
str(anes2016$V161095)
table(anes2016$V161095)
anes2016$feeldem = car::recode(anes2016$V161095, "-88:-99 = 'NA'")
table(anes2016$feeldem)
psych::describe(anes2016$feeldem, na.rm = TRUE)

#V161096 - Feelings towards the republicans
str(anes2016$V161096)
table(anes2016$V161096)
anes2016$feelrep = car::recode(anes2016$V161096, "-88:-99 = 'NA'")
table(anes2016$feelrep)
psych::describe(anes2016$feelrep, na.rm = TRUE)

#Feeling Thermometers towards the Ideologies

#V162097 - Feelings towards liberals
str(anes2016$V162097)
table(anes2016$V162097)
anes2016$feellib = car::recode(anes2016$V162097, "-1:-99 = 'NA'")
table(anes2016$feellib)
psych::describe(anes2016$feellib, na.rm = TRUE)

#V162101 - feelings towards conservatives
str(anes2016$V162101)
table(anes2016$V162101)
anes2016$feelcons = car::recode(anes2016$V162101, "-1:-99 = 'NA'")
table(anes2016$feelcons)
psych::describe(anes2016$feelcons, na.rm = TRUE)

#V161158X - Party ID 7-category
str(anes2016$V161158X)
table(anes2016$V161158X)
anes2016$pid7 = car::recode(anes2016$V161158X, "-8:-9 = 'NA'")
table(anes2016$pid7)

#V161155 - Party ID 3-point
str(anes2016$V161155)
table(anes2016$V161155)
anes2016$pid3 = car::recode(anes2016$V161155, "-8:-9 = 'NA'")
table(anes2016$pid3)

#V162171 - Ideology 7-point
str(anes2016$V162171)
table(anes2016$V162171)
anes2016$ideo7 = car::recode(anes2016$V162171, "-6:-9 = 'NA'")
anes2016$ideo7 = car::recode(anes2016$ideo7, "99 = 'NA'")
table(anes2016$ideo7)

#V162171A - Ideology 3-point
str(anes2016$V162171A)
table(anes2016$V162171A)
anes2016$ideo3 = car::recode(anes2016$V162171A, "-1:-9 = 'NA'")
table(anes2016$ideo3)
