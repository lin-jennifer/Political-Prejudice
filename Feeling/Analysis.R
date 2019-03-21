#Analysis of Feeling Thermometers 
#Jennifer Lin
#ANES Time Series Cumulative 1948-2016
#https://electionstudies.org/data-center/ 

#Load in data
anescdf = read.csv("~/Desktop/Working/ANES/anes.csv", header = TRUE)

#Load packages
library(car)
library(ggplot2) #Graphs
library(ggpubr)
library(gmodels) #Cross Tab

#Analyze Party ID Variable
str(anescdf$VCF0303)
table(anescdf$VCF0303)

#Recode Party ID variable to omit missing data
anescdf$pid3 <- car::recode(anescdf$VCF0303, "0 = 'NA'")
CrossTable(anescdf$VCF0303, anescdf$pid3)
