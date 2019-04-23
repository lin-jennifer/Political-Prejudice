########## General Information #################
#File: Participation and Knowledge ANOVA Analysis
#Author: Jennifer Lin
#Requirements: "RMProjData.csv" - Refer to `Management.R` for the construction of this file
#Root File: ANES 2016 Time Series (https://electionstudies.org/project/2016-time-series-study/)
#Purpose: ANOVA for Party/Ideology and Knowledge/Participation on feelings towards parties/ideologies
#         - AOV Party x Knolwedge on Feelings towards Dems
#         - AOV Party x Knolwedge on Feelings towards Reps
#         - AOV Party x Participation on Feelings towards Dems
#         - AOV Party x Participation on Feelings towards Reps
#         - AOV Ideology x Knolwedge on Feelings towards Liberals
#         - AOV Ideology x Knolwedge on Feelings towards Conservatives
#         - AOV Ideology x Participation on Feelings towards Liberals
#         - AOV Ideology x Participation on Feelings towards Conservatives
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
#Create Political Knowledge variable
data$knowledge <- rowSums(data[,c('biden', 'ryan', 'merkel', 'putin', 'roberts')], na.rm=TRUE)

#Create Political Participation Variable
data$participation <- rowSums(data[, c('talk', 'meeting', 'wear', 'work', 'campaigndon', 'partydon', 'attend', 'petition', 'post', 'contact', 'vote16')], na.rm = TRUE)

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

data$partyfactor <- factor(data$partynum, 
                           levels = c(1, 2, 3),
                           labels = c("Democrat", "Independent", "Republican"))

data$ideofactor <- factor(data$ideo3num, 
                          levels = c(1, 2, 3),
                          labels = c("Liberal", "Moderate", "Conservative"))


############# Party - Knowledge on Party Feelings ANOVA ############

# 3(Party ID: Republican, Democrat, Independent) x 2(Knowledge: More or Less)

#Feelings towards Democratic Party
dem.party <- aov(feeldem ~ knowfactor*partyfactor, data = data)
summary(dem.party)
etaSquared(dem.party, anova = TRUE)

#Comupte Cell means for feelings towards democats
groups <- group_by(data, knowfactor, partyfactor)
dem.feel <- summarise(groups,
                      mean = mean(feeldem, na.rm=TRUE),
                      sd = sd(feeldem, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
dem.feel
ggplot(dem.feel, aes(x=partyfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Party and Knowledge") +
  xlab("Political Party") +
  ylab("Feelings towards Democrats") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "blue", "More" = "blue4"))

#Feelings towards Republican party
rep.party <- aov(feelrep ~ knowfactor*partyfactor, data = data)
summary(rep.party)
etaSquared(rep.party, anova = TRUE)

#Comupte Cell means for feelings towards republicans
groups <- group_by(data, knowfactor, partyfactor)
rep.feel <- summarise(groups,
                      mean = mean(feelrep, na.rm=TRUE),
                      sd = sd(feelrep, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
rep.feel
ggplot(rep.feel, aes(x=partyfactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Party and Knowledge") +
  xlab("Political Party") +
  ylab("Feelings towards Republicans") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "red", "More" = "red3"))

########### Ideology - Knowledge on Ideology Feelings ANOVA ###########

# 3(Ideology: Liberal, Moderate, Conservative) x 2(Knowledge: More or Less)

#Feelings towards Liberals
lib.ideo <- aov(feellib ~ knowfactor*ideofactor, data = data)
summary(lib.ideo)
etaSquared(lib.ideo, anova = TRUE)

#Comupte Cell means for feelings towards liberals
groups <- group_by(data, knowfactor, ideofactor)
lib.ideo <- summarise(groups,
                      mean = mean(feellib, na.rm=TRUE),
                      sd = sd(feellib, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
lib.ideo
ggplot(lib.ideo, aes(x=ideofactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Ideology and Knowledge") +
  xlab("Political Ideology") +
  ylab("Feelings towards Liberals") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "royalblue1", "More" = "royalblue3"))

#Feelings towards Conservatives
cons.ideo <- aov(feelcons ~ knowfactor*ideofactor, data = data)
summary(cons.ideo)
etaSquared(cons.ideo, anova = TRUE)

#Comupte Cell means for feelings towards liberals
groups <- group_by(data, knowfactor, ideofactor)
cons.ideo <- summarise(groups,
                      mean = mean(feelcons, na.rm=TRUE),
                      sd = sd(feelcons, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
cons.ideo
ggplot(cons.ideo, aes(x=ideofactor, y=mean, fill = knowfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Ideology and Knowledge") +
  xlab("Political Ideology") +
  ylab("Feelings towards Conservatives") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Knowledge", values = c("Less" = "firebrick1", "More" = "firebrick3"))

############# Party - Participation on Party Feelings ANOVA #####################

# 3(Party ID: Republican, Democrat, Independent) x 3(Participation: Low, Medium, High)

#Feelings towards Democratic Party
part.dem <- aov(feeldem ~ partfactor*partyfactor, data = data)
summary(part.dem)
etaSquared(part.dem, anova = TRUE)

#Comupte Cell means for feelings towards democats
groups <- group_by(data, partfactor, partyfactor)
dem.part <- summarise(groups,
                      mean = mean(feeldem, na.rm=TRUE),
                      sd = sd(feeldem, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
dem.part
ggplot(dem.part, aes(x=partyfactor, y=mean, fill = partfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Party and Participation") +
  xlab("Political Party") +
  ylab("Feelings towards Democrats") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Participation", values = c("Less" = "blue1", "Moderate" = "blue2", "More" = "blue3"))

#Feelings towards Republican party
part.rep <- aov(feelrep ~ partfactor*partyfactor, data = data)
summary(part.rep)
etaSquared(part.rep, anova = TRUE)

#Comupte Cell means for feelings towards republicans
groups <- group_by(data, partfactor, partyfactor)
rep.part <- summarise(groups,
                      mean = mean(feelrep, na.rm=TRUE),
                      sd = sd(feelrep, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
rep.part
ggplot(rep.part, aes(x=partyfactor, y=mean, fill = partfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Party and Participation") +
  xlab("Political Party") +
  ylab("Feelings towards Republicans") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Participation", values = c("Less" = "red1", "Moderate" = "red2", "More" = "red3"))

########### Ideology - Participation on Ideology Feelings ANOVA ###########

# 3(Ideology: Liberal, Moderate, Conservative) x 3(Participation: Low, Moderate, High)

#Feelings towards Liberals
part.lib <- aov(feellib ~ partfactor*ideofactor, data = data)
summary(part.lib)
etaSquared(part.lib, anova = TRUE)

#Comupte Cell means for feelings towards liberals
groups <- group_by(data, partfactor, ideofactor)
liberalpart <- summarise(groups,
                      mean = mean(feellib, na.rm=TRUE),
                      sd = sd(feellib, na.rm=TRUE),
                      n = n(),
                      se=sd/sqrt(n),
                      ci = qt(0.975,df=n-1)*se)
liberalpart
ggplot(liberalpart, aes(x=ideofactor, y=mean, fill = partfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Ideology and Participation") +
  xlab("Political Ideology") +
  ylab("Feelings towards Liberals") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Participation", values = c("Less" = "royalblue1","Moderate" ="royalblue2", "More" = "royalblue3"))

#Feelings towards Conservatives
part.cons <- aov(feelcons ~ partfactor*ideofactor, data = data)
summary(part.cons)
etaSquared(part.cons, anova = TRUE)

#Comupte Cell means for feelings towards liberals
groups <- group_by(data, partfactor, ideofactor)
conservepart <- summarise(groups,
                       mean = mean(feelcons, na.rm=TRUE),
                       sd = sd(feelcons, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975,df=n-1)*se)
conservepart
ggplot(conservepart, aes(x=ideofactor, y=mean, fill = partfactor )) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, size = 2, position=position_dodge(.9)) +
  ggtitle("Ideology and Participation") +
  xlab("Political Ideology") +
  ylab("Feelings towards Conservatives") + ylim(0,100)+ theme_classic()+
  theme(text = element_text(size = 22, colour="black"),
        axis.title = element_text(size = 24, colour="black"),
        title = element_text(size = 26, colour="black"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual("Participation", values = c("Less" = "firebrick1", "Moderate" = "firebrick2", "More" = "firebrick3"))







