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
