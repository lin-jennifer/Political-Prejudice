########## General Information #################
#Purpose: Create Graphics
#Author: Jennifer Lin
#Requires: ANES 2016 Time Series Data 
#Access: https://electionstudies.org/project/2016-time-series-study/
###############################################

#Load in data and change the name
data <- read.csv("~/Desktop/Working/Political-Prejudice/AS-Project/anes16.csv", header = TRUE)

#Set the Working Directory
getwd()
setwd("~/Desktop/Working/Political-Prejudice/AS-Project")

#Load Libraries
library(ggplot2) #For graphics
library(car) #For general statistics
library(ggpubr) #For publishing quality graphs
library(dplyr) #Create summary graphs
library(coefplot) #Coefficient plot

########## Clean Party ID ##############

#Rid implicit NAs for the party id variable
library(forcats)
data$pid3<- fct_explicit_na(data$pid3, na_level = "(Missing)")

#Recode PID3
data$party = recode(data$pid3, "(0) 0. No preference (FTF ONLY)" = "No Party", "(1) 1. Democrat" = "Democrat", "(2) 2. Republican" = "Republican", "(3) 3. Independent" = "Independent", "(5) 5. Other party SPECIFY" = "Other", "(Missing)" = "NA" )
table(data$party)

#Remove "No Party", "other" and NAs from party
data<-data[!(data$party==""),]
data<-data[!(data$party=="NA"),]
data<-data[!(data$party=="No Party"),]   
data<-data[!(data$party=="Other"),]
table(data$party)

########## Correlations ################

#Republicans

#Correlation Test
cor.test(data$feelrep, data$trump, method = "pearson", use = "complete.obs")

#Scatterplot
#Not sorted by party
ggscatter(data, x = "feelrep", y = "trump", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Feelings towards Republicans", ylab = "Feelings towards Trump", title = "Republicans")+theme_classic()+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

#Sorted by party
ggscatter(data, x = "feelrep", y = "trump", fill = "party", color = "party", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Feelings towards Republicans", ylab = "Feelings towards Trump", title = "Republians")+theme_classic()+
  scale_color_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  scale_fill_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

#Democrats

#Correlation Test
cor.test(data$feeldem, data$clinton, method = "pearson", use = "complete.obs")

#Scatterplot
#Not sorted by party
ggscatter(data, x = "feeldem", y = "clinton", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Feelings towards Democrats", ylab = "Feelings towards Clinton", title = "Democrats")+theme_classic()+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

#Sorted by party
ggscatter(data, x = "feeldem", y = "clinton", fill = "party", color = "party", 
          add = "reg.line", conf.int = TRUE, size = 1.5,
          cor.coef = FALSE, cor.method = "pearson",
          xlab = "Feelings towards Democrats", ylab = "Feelings towards Clinton", title = "Democrats")+theme_classic()+
  scale_color_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  scale_fill_manual("Party ID", breaks = c("Democrat", "Republican", "Independent"), values = c("Democrat" = "blue", "Independent" = "purple", "Republican" = "red"))+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

############## Multiple Regression #############

# Republicans

#Simultaneous
trump <- lm(trump ~ education + income + pid7, data = data)
summary(trump)

coefplot(trump)+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

#Hierarchal
TS1 <- lm(trump ~ pid7, data = na.omit(data))
TS2 <- lm(trump ~ education + income + pid7, data = na.omit(data))
summary(TS1)
summary(TS2)
anova(TS1, TS2)

# Democrats

#Simultaneous
clinton <- lm(clinton ~ education + income + pid7, data = data)
summary(clinton)

coefplot(clinton)+
  theme(text = element_text(size = 18, colour="black"),          
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        axis.text.x = element_text(hjust = 0, vjust = 0),
        plot.title = element_text(hjust = 0.5))

#Hierarchal
CS1 <- lm(clinton ~ pid7, data = na.omit(data))
CS2 <- lm(clinton ~ education + income + pid7, data = na.omit(data))
summary(CS1)
summary(CS2)
anova(CS1, CS2)
