#Party Distribution on CCES
#Jennifer Lin
#CCES 2016 - https://cces.gov.harvard.edu/

#Load R Data
#cces = read.table("/Users/JenniferLin/Desktop/Working/CCES/CCES2016.tab", header = TRUE)
load("/Users/JenniferLin/Desktop/Data/CCES/CCES16_Common_OUTPUT_Feb2018_VV.RData")

#Set Working Directory
setwd("~/Desktop/Working/Political-Prejudice/Map")

#Load Packages
library(zipcode)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(car)

#Look at Respondent Party
str(x$pid3)

#Remove Missing from Party ID
x$pid3
x<-x[!(x$pid3=="Not sure"),]
x<-x[!(x$pid3=="Other"),]
x<-x[!(x$pid3=="NA"),]

#Rid implicit NAs for the party id variable
library(forcats)
x$pid3 <- fct_explicit_na(x$pid3, na_level = "(Missing)")

#Recode Missing Variables
x$pid3 = recode(x$pid3, "(Missing)" = "NA")

#Generate Frequency graph of Respondent party distribution
party <- x %>%
  group_by(pid3) %>%
  summarise(counts = n())
party

#remove NA from graph
party = party[!(party$pid3 == "NA"),]

#Generate Bar graph
ggplot(party, aes(x = pid3, y = counts)) +
  geom_bar(fill = c("Democrat" = "blue","Republican" = "red", "Independent" = "purple"), stat = "identity", width = .5) +
  geom_text(aes(label = counts), vjust = -0.3, size = 6) + ylim(0, 30000)+
  theme_classic()+
  ggtitle("Party ID of CCES 2016 Respondents")+xlab("Party Identification")+ylab("Number of People")+
  theme(text = element_text(size = 18, colour="black"),
        axis.title = element_text(size = 20, colour="black"),
        title = element_text(size = 24, colour="black"),
        plot.title = element_text(hjust = 0.5))

