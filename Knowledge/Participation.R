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
anes2016$talk = car::recode(anes2016$talk, "2 = 0")
table(anes2016$talk)

#Go to political meeting - V162011
str(anes2016$V162011)
table(anes2016$V162011)
anes2016$meeting = car::recode(anes2016$V162011, "-6:-9 = 'NA'" )
anes2016$meeting = as.numeric(as.factor(anes2016$meeting), na.rm = TRUE)
str(anes2016$meeting)
anes2016$meeting = car::recode(anes2016$meeting, "2 = 0")
table(anes2016$meeting)

#Wear Campaign Button - V162012
str(anes2016$V162012)
table(anes2016$V162012)
anes2016$wear = car::recode(anes2016$V162012, "-6:-9 = 'NA'")
anes2016$wear = as.numeric(as.factor(anes2016$wear), na.rm = TRUE)
str(anes2016$wear)
anes2016$wear = car::recode(anes2016$wear, "2 = 0")
table(anes2016$wear)

#Do any other work for party - V162013
str(anes2016$V162013)
table(anes2016$V162013)
anes2016$work = car::recode(anes2016$V162013, "-6:-9 = 'NA'")
anes2016$work = as.numeric(as.factor(anes2016$work), na.rm = TRUE)
str(anes2016$work)
anes2016$work = car::recode(anes2016$work, "2 = 0")
table(anes2016$work)

#Donate to campaign - V162014
str(anes2016$V162014)
table(anes2016$V162014)
anes2016$campaigndon = car::recode(anes2016$V162014, "-6:-9 = 'NA'")
anes2016$campaigndon = as.numeric(as.factor(anes2016$campaigndon), na.rm = TRUE)
str(anes2016$campaigndon)
anes2016$campaigndon = car::recode(anes2016$campaigndon, "2 = 0")
table(anes2016$campaigndon)

#Donate to Party - V162016
str(anes2016$V162016)
table(anes2016$V162016)
anes2016$partydon = car::recode(anes2016$V162016, "-6:-9 = 'NA'")
anes2016$partydon = as.numeric(as.factor(anes2016$partydon), na.rm = TRUE)
str(anes2016$partydon)
anes2016$partydon = car::recode(anes2016$partydon, "2 = 0")
table(anes2016$partydon)

#Attend Rally/Protest - V162018A
str(anes2016$V162018A)
table(anes2016$V162018A)
anes2016$attend = car::recode(anes2016$V162018A, "-6:-9 = 'NA'")
anes2016$attend = as.numeric(as.factor(anes2016$attend), na.rm = TRUE)
str(anes2016$attend)
anes2016$attend = car::recode(anes2016$attend, "2 = 0")
table(anes2016$attend)

#Signed Petition - V162018B
str(anes2016$V162018B)
table(anes2016$V162018B)
anes2016$petition = car::recode(anes2016$V162018B, "-6:-9 = 'NA'")
anes2016$petition = as.numeric(as.factor(anes2016$petition), na.rm = TRUE)
str(anes2016$petition)
anes2016$petition = car::recode(anes2016$petition, "2 = 0")
table(anes2016$petition)

#Posted on Social Media about politics - V162018E
str(anes2016$V162018E)
table(anes2016$V162018E)
anes2016$post = car::recode(anes2016$V162018E, "-6:-9 = 'NA'")
anes2016$post = as.numeric(as.factor(anes2016$post), na.rm = TRUE)
str(anes2016$post)
anes2016$post = car::recode(anes2016$post, "2 = 0")
table(anes2016$post)

#Contact US Representative or Senator - V162019
str(anes2016$V162019)
table(anes2016$V162019)
anes2016$contact = car::recode(anes2016$V162019, "-6:-9 = 'NA'")
anes2016$contact = as.numeric(as.factor(anes2016$contact), na.rm = TRUE)
str(anes2016$contact)
anes2016$contact = car::recode(anes2016$contact, "2 = 0")
table(anes2016$contact)

#Voted in 2016 - V162031X
str(anes2016$V162031X)
table(anes2016$V162031X)
anes2016$vote16 = car::recode(anes2016$V162031X, "-1:-9 = 'NA'")
anes2016$vote16 = as.numeric(as.factor(anes2016$vote16), na.rm = TRUE)
str(anes2016$vote16)
anes2016$vote16 = car::recode(anes2016$vote16, "2 = 0")
table(anes2016$vote16)

#V161155 - Party ID 3-point
str(anes2016$V161155)
table(anes2016$V161155)
anes2016$pid3 = car::recode(anes2016$V161155, "-8:-9 = 'NA'")
table(anes2016$pid3)

#Rid implicit NAs for the party id variable
library(forcats)
anes2016$pid3<- fct_explicit_na(anes2016$pid3, na_level = "(Missing)")

#Recode PID3
anes2016$party = recode(anes2016$pid3, "(0) 0. No preference (FTF ONLY)" = "No Party", "(1) 1. Democrat" = "Democrat", "(2) 2. Republican" = "Republican", "(3) 3. Independent" = "Independent", "(5) 5. Other party SPECIFY" = "Other", "(Missing)" = "NA" )
table(anes2016$party)

#Generate Political Behavior Variable - out of 11 possible activities
which( colnames(anes2016)=="talk" ) #1197
anes2016$participation = anes2016[,1197]+anes2016[,1198]+anes2016[,1199]+anes2016[,1200]+anes2016[,1201]+anes2016[,1202]+anes2016[,1203]+anes2016[,1204]+anes2016[,1205]+anes2016[,1206]+anes2016[,1207]
anes2016$participation
summary(anes2016$participation)
hist(anes2016$participation)

#Decalre Participation variable as categorical
anes2016$participationcat = as.factor(as.numeric(anes2016$participation))
str(anes2016$participationcat)
table(anes2016$participationcat)

library(forcats)
anes2016$participationcat<- fct_explicit_na(anes2016$participationcat, na_level = "(Missing)")
anes2016$participationcat = recode(anes2016$participationcat, "(Missing)" = "NA")
table(anes2016$participationcat)

#Descriptive Statistics for participation by party
library(dplyr)
groups <- group_by(anes2016, party, participationcat)
participation <- summarise(groups,
                       mean = mean(participation, na.rm=TRUE),
                       sd = sd(participation, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975,df=n-1)*se)

participation

#Remove the No Parties and Third Parties from chart 
participation<-participation[!(participation$party=="NA"),]
participation<-participation[!(participation$party=="No Party"),]
participation<-participation[!(participation$party=="Other"),]

#Remove NAs from Total Participation acts
participation<-participation[!(participation$participationcat=="NA"),]

#Make the Graph
library(ggplot2)
library(ggpubr)
ggplot(participation, aes(x=participationcat, y=n, fill = party)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  scale_fill_manual("Party ID", values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  ggtitle("Political Participation by Party")+xlab("Total Participation Acts")+ylab("Number of People")+
  theme(text = element_text(size = 18, colour="black"),
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        plot.title = element_text(hjust = 0.5))

