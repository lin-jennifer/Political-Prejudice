########## General Information #################
#File: Party by Ideology ANOVA Analysis
#Author: Jennifer Lin
#Requirements: "RMProjData.csv" - Refer to `Management.R` for the construction of this file
#Root File: ANES 2016 Time Series (https://electionstudies.org/project/2016-time-series-study/)
#Purpose: ANOVA for Party/Ideology and Knowledge/Participation on feelings towards parties/ideologies
#         - AOV Party x Ideology on Feelings towards Dems
#         - AOV Party x Ideology on Feelings towards Reps
#         - AOV Party x Ideology on Feelings towards Dems
#         - AOV Party x Ideology on Feelings towards Reps
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
library(phia) #Contrasts
library(ggplot2) #Graphics
library(stargazer) #Exporting Plots

################## Clean Data #######################
#Remove "No Party", "other" and NAs from party
data<-data[!(data$party=="NA"),]
data<-data[!(data$party=="No Party"),]   
data<-data[!(data$party=="Other"),]
table(data$party)

#Recode Party to numeric
table(data$party)
data$partynum <- recode(data$party, "Democrat" = '1')
data$partynum <- recode(data$partynum, "Independent" = '2')
data$partynum <- recode(data$partynum, "Republican" = '3')
data$partynum <- recode(data$partynum, "No Party" = 'NA')
data$partynum <- recode(data$partynum, "Other" = 'NA')
table(data$partynum)

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

#Recode Ideology Variable to numeric
table(data$ideo3re)
data$ideo3num <- recode(data$ideo3re, "Liberal" = '1')
data$ideo3num <- recode(data$ideo3num, "Moderate" = '2')
data$ideo3num <- recode(data$ideo3num, "Conservative" = '3')
data$ideo3num <- recode(data$ideo3num, "NA" = 'NA')
table(data$ideo3num)

#Declare variables as factors for graphing purposes
data$partyfactor <- factor(data$partynum, 
                           levels = c(1, 2, 3),
                           labels = c("Democrat", "Independent", "Republican"))

data$ideofactor <- factor(data$ideo3num, 
                          levels = c(1, 2, 3),
                          labels = c("Liberal", "Moderate", "Conservative"))

############# Party - Ideology on Party Feelings ANOVA #####################

# 3(Party ID: Republican, Democrat, Independent) x 3(Ideology: Liberal, Moderate Conservative)

#Feelings towards Democratic Party
demparty <- aov(feeldem ~ ideofactor*partyfactor, data = data)
summary(demparty)
etaSquared(demparty, anova = TRUE)

#Comupte Cell means for feelings towards democats
groups <- group_by(data, ideofactor, partyfactor)
dem.party <- summarise(groups,
                      mean = mean(feeldem, na.rm=TRUE),
                      sd = sd(feeldem, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
dem.party
ggplot(dem.party, aes(x=ideofactor, y=mean, fill = partyfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Party and Ideology") +
  xlab("Political Ideology") +
  ylab("Feelings towards Democrats") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Political Party", values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))

#Feelings towards Republican party
repparty <- aov(feelrep ~ ideofactor*partyfactor, data = data)
summary(repparty)
etaSquared(repparty, anova = TRUE)

#Comupte Cell means for feelings towards republicans
groups <- group_by(data, ideofactor, partyfactor)
rep.party <- summarise(groups,
                      mean = mean(feelrep, na.rm=TRUE),
                      sd = sd(feelrep, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
rep.party
ggplot(rep.party, aes(x=ideofactor, y=mean, fill = partyfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Party and Ideology") +
  xlab("Political Ideology") +
  ylab("Feelings towards Republicans") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Political Party", values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))

########### Party - Ideology on Ideology Feelings ANOVA ###########

# 3(Party ID: Republican, Democrat, Independent) x 3(Ideology: Liberal, Moderate Conservative)

#Feelings towards Liberals
LibIdeo <- aov(feellib ~ ideofactor*partyfactor, data = data)
summary(LibIdeo)
etaSquared(LibIdeo, anova = TRUE)

#Comupte Cell means for feelings towards liberals
groups <- group_by(data, partyfactor, ideofactor)
liberal <- summarise(groups,
                         mean = mean(feellib, na.rm=TRUE),
                         sd = sd(feellib, na.rm=TRUE),
                         n = n(),
                         se=sd/sqrt(n),
                         ci = qt(0.975,df=n-1)*se)
liberal
ggplot(liberal, aes(x=ideofactor, y=mean, fill = partyfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Party and Ideology") +
  xlab("Political Ideology") +
  ylab("Feelings towards Liberals") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Political Party", values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))

#Feelings towards Conservatives
ConsIdeo <- aov(feelcons ~ ideofactor*partyfactor, data = data)
summary(ConsIdeo)
etaSquared(ConsIdeo, anova = TRUE)

#Comupte Cell means for feelings towards liberals
groups <- group_by(data, parytfactor, ideofactor)
conserve <- summarise(groups,
                          mean = mean(feelcons, na.rm=TRUE),
                          sd = sd(feelcons, na.rm=TRUE),
                          n = n(),
                          se=sd/sqrt(n),
                          ci = qt(0.975,df=n-1)*se)
conserve
ggplot(conserve, aes(x=ideofactor, y=mean, fill = partyfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Party and Ideology") +
  xlab("Political Ideology") +
  ylab("Feelings towards Conservatives") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Political Party", values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))


