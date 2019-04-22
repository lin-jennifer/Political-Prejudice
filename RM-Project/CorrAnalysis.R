########## General Information #################
#File: Participation and Knowledge Correlation Analysis
#Author: Jennifer Lin
#Requirements: "RMProjData.csv" - Refer to `Management.R` for the construction of this file
#Root File: ANES 2016 Time Series (https://electionstudies.org/project/2016-time-series-study/)
#Purpose: Analysis of the Correlation proposals for project
#         - Does more knowledge lead to more polarized feelings towards the oppssite party
#         - Does more participation lead to more polarized feelings towards the opposite party
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

#Subsection data by political party identification
dem.sub <- data[data$party=="Democrat",]
rep.sub <- data[data$party=="Republican",]
ind.sub <- data[data$party=="Independent",]

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

#Subsection data by political ideology identification
lib.sub <- data[data$ideo3re=="Liberal",]
con.sub <- data[data$ideo3re=="Conservative",]
mod.sub <- data[data$ideo3re=="Moderate",]

#################### Knowledge and Feeling by PARTY ################################
#Political PARTY as grouping variable 
#Political Party Feeling Thermometer as DV

#Correlations between Knowledge and feelings towards political others

#Knowledge and Feelings towards Democrats
#Use entire dataset
cor.test(data$knowledge, data$feeldem, method = "pearson", use = "complete.obs")
#Use subsection data by party
cor.test(dem.sub$knowledge, dem.sub$feeldem, method = "pearson", use = "complete.obs")
cor.test(rep.sub$knowledge, rep.sub$feeldem, method = "pearson", use = "complete.obs")
cor.test(ind.sub$knowledge, ind.sub$feeldem, method = "pearson", use = "complete.obs")

#Create scatterplot
ggscatter(data, x = "knowledge", y = "feeldem", fill = "party", color = "party", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Level of Political Knowledge", ylab = "Feelings towards Democrats", title = "Democrats")+theme_classic()+
  scale_color_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  scale_fill_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

#Knowledge and Feelings towards Republicans
#Use entire dataset
cor.test(data$knowledge, data$feeldem, method = "pearson", use = "complete.obs")
#Use subsection data by party
cor.test(dem.sub$knowledge, dem.sub$feelrep, method = "pearson", use = "complete.obs")
cor.test(rep.sub$knowledge, rep.sub$feelrep, method = "pearson", use = "complete.obs")
cor.test(ind.sub$knowledge, ind.sub$feelrep, method = "pearson", use = "complete.obs")

#Create scatterplot
ggscatter(data, x = "knowledge", y = "feelrep", fill = "party", color = "party", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Level of Political Knowledge", ylab = "Feelings towards Republicans", title = "Republicans")+theme_classic()+
  scale_color_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  scale_fill_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

############ Participation and Feeling by PARTY ########################
#Correlations between participation and feelings

#Participation and feelings towards Democrats
#Use entire dataset
cor.test(data$participation, data$feeldem, method = "pearson", use = "complete.obs")
#Use subsection data by party
cor.test(dem.sub$participation, dem.sub$feeldem, method = "pearson", use = "complete.obs")
cor.test(rep.sub$participation, rep.sub$feeldem, method = "pearson", use = "complete.obs")
cor.test(ind.sub$participation, ind.sub$feeldem, method = "pearson", use = "complete.obs")

#Create scatterplot
ggscatter(data, x = "participation", y = "feeldem", fill = "party", color = "party", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Level of Political Participation", ylab = "Feelings towards Democrats", title = "Democrats")+theme_classic()+
  scale_color_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  scale_fill_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))


#Participation and feelings towards Republicans
#Use entire dataset
cor.test(data$participation, data$feelrep, method = "pearson", use = "complete.obs")
#Use subsection data by party
cor.test(dem.sub$participation, dem.sub$feelrep, method = "pearson", use = "complete.obs")
cor.test(rep.sub$participation, rep.sub$feelrep, method = "pearson", use = "complete.obs")
cor.test(ind.sub$participation, ind.sub$feelrep, method = "pearson", use = "complete.obs")

#Create scatterplot
ggscatter(data, x = "participation", y = "feelrep", fill = "party", color = "party", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Level of Political Participation", ylab = "Feelings towards Republicans", title = "Republicans")+theme_classic()+
  scale_color_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  scale_fill_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

#################### Knowledge and Feeling by IDEOLOGY ###################################

#Political Ideology as grouping variable
#Political Ideology as Dependent variable

#Knowledge and feelings towards Liberals
#Use entire dataset
cor.test(data$knowledge, data$feellib, method = "pearson", use = "complete.obs")
#Use subsection data by party
cor.test(lib.sub$knowledge, lib.sub$feellib, method = "pearson", use = "complete.obs")
cor.test(con.sub$knowledge, con.sub$feellib, method = "pearson", use = "complete.obs")
cor.test(mod.sub$knowledge, mod.sub$feellib, method = "pearson", use = "complete.obs")

#Create scatterplot
ggscatter(data, x = "knowledge", y = "feellib", fill = "ideo3re", color = "ideo3re", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Level of Political Knowledge", ylab = "Feelings towards Liberals", title = "Liberals")+theme_classic()+
  scale_color_manual("Ideology", breaks = c("Liberal", "Conservative", "Moderate"), values = c("Liberal" = "blue", "Moderate" = "purple", "Conservative" = "red"))+
  scale_fill_manual("Ideology", breaks = c("Liberal", "Conservative", "Moderate"), values = c("Liberal" = "blue", "Moderate" = "purple", "Conservative" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

#Knowledge and feelings towards Conservatives
#Use entire dataset
cor.test(data$knowledge, data$feelcons, method = "pearson", use = "complete.obs")
#Use subsection data by party
cor.test(lib.sub$knowledge, lib.sub$feelcons, method = "pearson", use = "complete.obs")
cor.test(con.sub$knowledge, con.sub$feelcons, method = "pearson", use = "complete.obs")
cor.test(mod.sub$knowledge, mod.sub$feelcons, method = "pearson", use = "complete.obs")

#Create scatterplot
ggscatter(data, x = "knowledge", y = "feelcons", fill = "ideo3re", color = "ideo3re", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Level of Political Knowledge", ylab = "Feelings towards Conservatives", title = "Conservatives")+theme_classic()+
  scale_color_manual("Ideology", breaks = c("Liberal", "Conservative", "Moderate"), values = c("Liberal" = "blue", "Moderate" = "purple", "Conservative" = "red"))+
  scale_fill_manual("Ideology", breaks = c("Liberal", "Conservative", "Moderate"), values = c("Liberal" = "blue", "Moderate" = "purple", "Conservative" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

############### Participation and Feeling by IDEOLOGY ########################

#Participation and feelings towards Liberals
#Use entire dataset
cor.test(data$participation, data$feellib, method = "pearson", use = "complete.obs")
#Use subsection data by party
cor.test(lib.sub$participation, lib.sub$feellib, method = "pearson", use = "complete.obs")
cor.test(con.sub$participation, con.sub$feellib, method = "pearson", use = "complete.obs")
cor.test(mod.sub$participation, mod.sub$feellib, method = "pearson", use = "complete.obs")

#Create scatterplot
ggscatter(data, x = "participation", y = "feellib", fill = "ideo3re", color = "ideo3re", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Level of Political Participation", ylab = "Feelings towards Liberals", title = "Liberals")+theme_classic()+
  scale_color_manual("Ideology", breaks = c("Liberal", "Conservative", "Moderate"), values = c("Liberal" = "blue", "Moderate" = "purple", "Conservative" = "red"))+
  scale_fill_manual("Ideology", breaks = c("Liberal", "Conservative", "Moderate"), values = c("Liberal" = "blue", "Moderate" = "purple", "Conservative" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

#Participation and feelings towards Conservatives
#Use entire dataset
cor.test(data$participation, data$feelcons, method = "pearson", use = "complete.obs")
#Use subsection data by party
cor.test(lib.sub$participation, lib.sub$feelcons, method = "pearson", use = "complete.obs")
cor.test(con.sub$participation, con.sub$feelcons, method = "pearson", use = "complete.obs")
cor.test(mod.sub$participation, mod.sub$feelcons, method = "pearson", use = "complete.obs")

#Create scatterplot
ggscatter(data, x = "participation", y = "feelcons", fill = "ideo3re", color = "ideo3re", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Level of Political Participation", ylab = "Feelings towards Conservatives", title = "Conservatives")+theme_classic()+
  scale_color_manual("Ideology", breaks = c("Liberal", "Conservative", "Moderate"), values = c("Liberal" = "blue", "Moderate" = "purple", "Conservative" = "red"))+
  scale_fill_manual("Ideology", breaks = c("Liberal", "Conservative", "Moderate"), values = c("Liberal" = "blue", "Moderate" = "purple", "Conservative" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))



