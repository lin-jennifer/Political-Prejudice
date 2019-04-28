########## General Information #################
#File: Participation and Knowledge ANOVA Analysis
#Author: Jennifer Lin
#Requirements: "RMProjData.csv" - Refer to `Management.R` for the construction of this file
#Root File: ANES 2016 Time Series (https://electionstudies.org/project/2016-time-series-study/)
#Purpose: Analysis of the Interaction between knowledge and participation on feelings towards political others
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

#Create Political Knowledge variable
data$knowledge <- rowSums(data[,c('biden', 'ryan', 'merkel', 'putin', 'roberts')], na.rm=TRUE)

#Create Political Participation Variable
data$participation <- rowSums(data[, c('talk', 'meeting', 'wear', 'work', 'campaigndon', 'partydon', 'attend', 'petition', 'post', 'contact', 'vote16')], na.rm = TRUE)

#Remove "No Party", "other" and NAs from party
data<-data[!(data$party=="NA"),]
data<-data[!(data$party=="No Party"),]   
data<-data[!(data$party=="Other"),]
table(data$party)

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

#Condense Knowledge and Participation Variables to low/medium/high concept
#Political Knowledge
#1 = knows little, 2 = knows a lot
table(data$knowledge)
data<-data[!(data$knowledge=="NA"),]
data$know <- car::recode(data$knowledge, "0:2 = 1")
data$know <- car::recode(data$know, "3:5 = 2")
table(data$know)

#Political participation
#Divided odly to capture the population based on data, rather than scale by the possible values
boxplot(data$participation) #Used to make the decision based on where the 25, 50 and 75 percent were
#1 = participates little, 2 = participates moderately, 3 = participates a lot
table(data$participation)
data<-data[!(data$participation=="NA"),]
data$part <- car::recode(data$participation, "0:1 = 1")
data$part <- car::recode(data$part, "2:4 = 2")
data$part <- car::recode(data$part, "5:11 = 3")
table(data$part)

#Declare variables as factors for graphing purposes
data$knowfactor <- factor(data$know, 
                                 levels = c(1, 2),
                                 labels = c("Less", "More"))

data$partfactor <- factor(data$part, 
                             levels = c(1, 2, 3),
                             labels = c("Less", "Moderate", "More"))

#Subsection data by political ideology identification
lib.sub <- data[data$ideo3re=="Liberal",]
con.sub <- data[data$ideo3re=="Conservative",]
mod.sub <- data[data$ideo3re=="Moderate",]

#Subsection data by political party identification
dem.sub <- data[data$party=="Democrat",]
rep.sub <- data[data$party=="Republican",]
ind.sub <- data[data$party=="Independent",]

#Declare variables as factors for graphing purposes
lib.sub$knowfactor <- factor(lib.sub$know, 
                          levels = c(1, 2),
                          labels = c("Less", "More"))

lib.sub$partfactor <- factor(lib.sub$part, 
                          levels = c(1, 2, 3),
                          labels = c("Less", "Moderate", "More"))

con.sub$knowfactor <- factor(con.sub$know, 
                             levels = c(1, 2),
                             labels = c("Less", "More"))

con.sub$partfactor <- factor(con.sub$part, 
                             levels = c(1, 2, 3),
                             labels = c("Less", "Moderate", "More"))

dem.sub$knowfactor <- factor(dem.sub$know, 
                             levels = c(1, 2),
                             labels = c("Less", "More"))

dem.sub$partfactor <- factor(dem.sub$part, 
                             levels = c(1, 2, 3),
                             labels = c("Less", "Moderate", "More"))

rep.sub$knowfactor <- factor(rep.sub$know, 
                             levels = c(1, 2),
                             labels = c("Less", "More"))

rep.sub$partfactor <- factor(rep.sub$part, 
                             levels = c(1, 2, 3),
                             labels = c("Less", "Moderate", "More"))

############### Participation x Feeling ANOVA (overall) #####################################

#Feelings towards Democrats
dem <- aov(feeldem ~ knowfactor*partfactor, data = data)
summary(dem)
etaSquared(dem, anova = TRUE)

#Comupte Cell means for feelings towards dems
groups <- group_by(data, knowfactor, partfactor)
dem.feel <- summarise(groups,
                      mean = mean(feeldem, na.rm=TRUE),
                      sd = sd(feeldem, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
dem.feel
ggplot(dem.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation") +
  xlab("Participation") +
  ylab("Feelings towards Democrats") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "blue", "More" = "blue4"))

#Feelings towards Republicans
rep <- aov(feelrep ~ knowfactor*partfactor, data = data)
summary(rep)
etaSquared(rep, anova = TRUE)

#Comupte Cell means for feelings towards Republicans
groups <- group_by(data, knowfactor, partfactor)
rep.feel <- summarise(groups,
                      mean = mean(feelrep, na.rm=TRUE),
                      sd = sd(feelrep, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
rep.feel
ggplot(rep.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation") +
  xlab("Participation") +
  ylab("Feelings towards Republicans") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "red", "More" = "red3"))

#Feelings towards Liberals
lib <- aov(feellib ~ knowfactor*partfactor, data = data)
summary(lib)
etaSquared(lib, anova = TRUE)

#Comupte Cell means for feelings towards Liberals
groups <- group_by(data, knowfactor, partfactor)
lib.feel <- summarise(groups,
                      mean = mean(feellib, na.rm=TRUE),
                      sd = sd(feellib, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
lib.feel
ggplot(lib.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation") +
  xlab("Participation") +ylim(0, 100)+
  ylab("Feelings towards Liberals") + theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "royalblue1", "More" = "royalblue3"))

#Feelings towards Conservatives
cons <- aov(feelcons ~ knowfactor*partfactor, data = data)
summary(cons)
etaSquared(cons, anova = TRUE)

#Comupte Cell means for feelings towards Conservatives
groups <- group_by(data, knowfactor, partfactor)
cons.feel <- summarise(groups,
                      mean = mean(feelcons, na.rm=TRUE),
                      sd = sd(feelcons, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
cons.feel
ggplot(cons.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation") +
  xlab("Participation") +ylim(0, 100)+
  ylab("Feelings towards Conservatives") + theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "firebrick1", "More" = "firebrick3"))

################## Participation x Knowledge by Democrats #####################
#This section contains feelings towards Democrats and Liberals sort by party (D or R) or ideology (L or C)

#Feelings towards Democrats among Democrats
dem <- aov(feeldem ~ knowfactor*partfactor, data = dem.sub)
summary(dem)
etaSquared(dem, anova = TRUE)

#Comupte Cell means for feelings towards Democrats
groups <- group_by(dem.sub, knowfactor, partfactor)
dem.feel <- summarise(groups,
                      mean = mean(feeldem, na.rm=TRUE),
                      sd = sd(feeldem, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
dem.feel
ggplot(dem.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation among Democrats") +
  xlab("Participation") +
  ylab("Feelings towards Democrats") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "blue", "More" = "blue4"))

#Feelings towards Democrats among Republicans
rep <- aov(feeldem ~ knowfactor*partfactor, data = rep.sub)
summary(rep)
etaSquared(rep, anova = TRUE)

#Comupte Cell means for feelings towards Democrats
groups <- group_by(rep.sub, knowfactor, partfactor)
rep.feel <- summarise(groups,
                      mean = mean(feeldem, na.rm=TRUE),
                      sd = sd(feeldem, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
rep.feel
ggplot(rep.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation among Republicans") +
  xlab("Participation") +
  ylab("Feelings towards Democrats") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "red", "More" = "red3"))

#Feelings towards Liberals among Liberals
lib <- aov(feellib ~ knowfactor*partfactor, data = lib.sub)
summary(lib)
etaSquared(lib, anova = TRUE)

#Comupte Cell means for feelings towards Liberals
groups <- group_by(lib.sub, knowfactor, partfactor)
lib.feel <- summarise(groups,
                      mean = mean(feellib, na.rm=TRUE),
                      sd = sd(feellib, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
lib.feel
ggplot(lib.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation among Liberals") +
  xlab("Participation") +ylim(0, 100)+
  ylab("Feelings towards Liberals") + theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "royalblue1", "More" = "royalblue3"))

#Feelings towards Liberals among Conservatives
cons <- aov(feellib ~ knowfactor*partfactor, data = con.sub)
summary(cons)
etaSquared(cons, anova = TRUE)

#Comupte Cell means for feelings towards Conservatives
groups <- group_by(con.sub, knowfactor, partfactor)
cons.feel <- summarise(groups,
                       mean = mean(feellib, na.rm=TRUE),
                       sd = sd(feellib, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975,df=n-1)*se)
cons.feel

ggplot(cons.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation among Conservatives") +
  xlab("Participation") +ylim(0, 100)+
  ylab("Feelings towards Liberals") + theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "firebrick1", "More" = "firebrick3"))

################### Knowledge x Participation for Republicans #####################
#This section contains feelings towards Republicans and Conservatives sort by party (D or R) or ideology (L or C)

#Feelings towards Republicans among Democrats
dem <- aov(feelrep ~ knowfactor*partfactor, data = dem.sub)
summary(dem)
etaSquared(dem, anova = TRUE)

#Comupte Cell means for feelings towards Democrats
groups <- group_by(dem.sub, knowfactor, partfactor)
dem.feel <- summarise(groups,
                      mean = mean(feelrep, na.rm=TRUE),
                      sd = sd(feelrep, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
dem.feel
ggplot(dem.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation among Democrats") +
  xlab("Participation") +
  ylab("Feelings towards Republicans") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "blue", "More" = "blue4"))

#Feelings towards Democrats among Republicans
rep <- aov(feelrep ~ knowfactor*partfactor, data = rep.sub)
summary(rep)
etaSquared(rep, anova = TRUE)

#Comupte Cell means for feelings towards Democrats
groups <- group_by(rep.sub, knowfactor, partfactor)
rep.feel <- summarise(groups,
                      mean = mean(feelrep, na.rm=TRUE),
                      sd = sd(feelrep, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
rep.feel
ggplot(rep.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation among Republicans") +
  xlab("Participation") +
  ylab("Feelings towards Republicans") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "red", "More" = "red3"))

#Feelings towards Liberals among Liberals
lib <- aov(feelcons ~ knowfactor*partfactor, data = lib.sub)
summary(lib)
etaSquared(lib, anova = TRUE)

#Comupte Cell means for feelings towards Liberals
groups <- group_by(lib.sub, knowfactor, partfactor)
lib.feel <- summarise(groups,
                      mean = mean(feelcons, na.rm=TRUE),
                      sd = sd(feelcons, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
lib.feel
ggplot(lib.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation among Liberals") +
  xlab("Participation") +ylim(0, 100)+
  ylab("Feelings towards Conservatives") + theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "royalblue1", "More" = "royalblue3"))

#Feelings towards Liberals among Conservatives
cons <- aov(feelcons ~ knowfactor*partfactor, data = con.sub)
summary(cons)
etaSquared(cons, anova = TRUE)

#Comupte Cell means for feelings towards Conservatives
groups <- group_by(con.sub, knowfactor, partfactor)
cons.feel <- summarise(groups,
                       mean = mean(feelcons, na.rm=TRUE),
                       sd = sd(feelcons, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975,df=n-1)*se)
cons.feel
ggplot(cons.feel, aes(x=partfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Knowledge and Participation among Conservatives") +
  xlab("Participation") +ylim(0, 100)+
  ylab("Feelings towards Conservatives") + theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "firebrick1", "More" = "firebrick3"))

