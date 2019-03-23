#Political Participation
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

#Clean Political Participation Variables
#Talk to others about voting - V162010
str(anes2016$V162010)
table(anes2016$V162010)
anes2016$talk = car::recode(anes2016$V162010, "-6:-9 = 'NA'")
anes2016$talk <- as.numeric(as.factor(anes2016$talk), na.rm = TRUE)
str(anes2016$talk)

#Go to political meeting - V162011
str(anes2016$V162011)
table(anes2016$V162011)
anes2016$meeting = car::recode(anes2016$V162011, "-6:-9 = 'NA'" )
anes2016$meeting = as.numeric(as.factor(anes2016$meeting), na.rm = TRUE)
str(anes2016$meeting)

#Wear Campaign Button - V162012
str(anes2016$V162012)
table(anes2016$V162012)
anes2016$wear = car::recode(anes2016$V162012, "-6:-9 = 'NA'")
anes2016$wear = as.numeric(as.factor(anes2016$wear), na.rm = TRUE)
str(anes2016$wear)

#Do any other work for party - V162013
str(anes2016$V162013)
table(anes2016$V162013)

#Donate to campaign - V162014
str(anes2016$V162014)
table(anes2016$V162014)

#Donate to Party - V162016
str(anes2016$V162016)
table(anes2016$V162016)

#Attend Rally/Protest - V162018A
str(anes2016$V162018A)
table(anes2016$V162018A)

#Signed Petition - V162018B
str(anes2016$V162018B)
table(anes2016$V162018B)

#Posted on Social Media about politics - V162018E
str(anes2016$V162018E)
table(anes2016$V162018E)

#Contact US Representative or Senator - V162019
str(anes2016$V162019)
table(anes2016$V162019)

#Voted in 2016 - V162031X
str(anes2016$V162031X)
table(anes2016$V162031X)
